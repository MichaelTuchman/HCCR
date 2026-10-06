## Read Spreadsheet Tables

rm(list=ls())

## packages and parameters
# program: setup.R
# load project dependencies

require(rlang)
require(tidyverse)
require(data.table)
require(readxl)
require(readr)
require(lubridate)
require(yaml)

source('config.R') # loads CONFIG + hccr_*() helpers from config.yaml
source('table3_reader.R') # finds Table 3's columns by header, so layout changes between years don't break it

# date  utility and helper functions


count_na=function(v) sum(is.na(v))
checkNA=function(DT) DT[,lapply(.SD,count_na)]


## Model year and workbook path now come from config.yaml (model.year,
## model.workbook.path) instead of being hard-coded here. To move to a
## new benefit year: download that year's "DIY tables" workbook from CMS
## and point config.yaml at it - see README "Updating to a new model year".

MODEL_YEAR = hccr_model_year()
fn = hccr_workbook_path()

## hcc_group : Data structures to write code for computing the 
## grouping variables

## set_to_zero table
# which hcc get set to zero if you have more serious condition
# split out commas, trim new variables, then pivot long
# result wil be a table with HCC | set_zero as columns

ss = function(hcc_code) {
  x=tstrsplit(hcc_code,'_',fill='')
  x0='HHS_HCC'
  x1=str_pad(str_squish(x[[1]]),3,'left','0')
  if (length(x)>1)
    x2=ifelse(x[[2]]=='','',paste('_',x[[2]],sep=''))
  else
    x2=''
  res=paste(x0,x1,x2,sep='')
  res[res=='HHS_HCCNA']=NA # inefficient. think this through!
  return(res)
}

## HCC Grouping variables
extra_vars=function(SheetNoC,skip=4) {
  read_excel(fn,
             sheet = SheetNoC,
             col_names=c('Model','Variable','Description','Used','Formula'),skip=skip) %>%
    filter(!is.na(Formula))
}

# fill in blank rows with value of nearest non-blank cell above it.

fill_in_blank_rows=function(formula_table,partial_column='Model') {
  
  non_blanks=!is.na(formula_table[[partial_column]])
  
  # for each non-blank line bump a counter this will be the group number
  
  F1 = formula_table %>% mutate(group=cumsum(non_blanks)) 
  
  # get those first lines so that we can propagate their values
  # into each empty row
  
  # number the non-blank rows
  
  GT=formula_table[which(non_blanks),]
  GT$group=1:(nrow(GT))
  
  # formula_table must contain the columns variable, description, model, use, etc.
  
  Result = F1 %>% left_join(GT %>% select(group,Variable,Used,Model,Description),by=c('group'))
  
  # fill in columns with top row only if the current row is blank
  # this needs some work as there is some generalizability here
  # that hasn't been made
  
  print(names(Result))
  
  Result %>% mutate(Model=coalesce(Model.x,Model.y),
                    Variable=coalesce(Variable.x,Variable.y),
                    Description=coalesce(Description.x,Description.y),
                    Used=coalesce(Used.x,Used.y)) %>%
    select(Model,Variable,Description,Used,Formula)
  
  
}

# do the same for child and infant classes

Adult=extra_vars(hccr_sheet('adult_variables'),hccr_skip('adult_variables',4))
Child=extra_vars(hccr_sheet('child_variables'),hccr_skip('child_variables',4))
Infant=extra_vars(hccr_sheet('infant_variables'),hccr_skip('infant_variables',4))

AllAges=rbind(Adult,Child,Infant) %>% fill_in_blank_rows
rm(list=c('Adult','Child','Infant')) # now redundant


## map diagnosis to HCC

## in order to replace periods with underlines in HCC codes

dash=function(str) str_replace_all(str,'\\.','_')

## Table 3 (ICD-10 -> condition category). table3_reader.R finds each
## column by its header text, so a year whose layout adds or reorders
## columns (CY2025 has 17, CY2022 has 13) reads without edits here.

HCC=read_icd10_crosswalk(fn,sheet=hccr_sheet('icd10_crosswalk'),
                         first_data_row=hccr_skip('icd10_crosswalk',4),
                         model_year=MODEL_YEAR) %>% as_tibble


HCC2=HCC%>%
# mutate(across(starts_with('cc'),dash)) %>%
  pivot_longer(starts_with('cc'),names_to=NULL,values_to = 'CC') %>%
  filter(!is.na(CC)) %>%
  filter(valid.current=='Y') %>%
  select(-obs) %>% mutate(CC=dash(as.character(CC))) %>% data.table

# simplify Sex conditions

HCC2[!is.na(sex.cond),`:=`(sex.cond=toupper(str_sub(sex.cond,1,1)))]
HCC2[!is.na(sex.split),`:=`(sex.split=toupper(str_sub(sex.split,1,1)))]
HCC2[,HCC:=ss(CC)]
# convert dots to underlines

SetToZeroRAW=read_excel(fn,skip=hccr_skip('hierarchies',3),
                     sheet = hccr_sheet('hierarchies'),col_names = c('Obs','HCC','SetZero','label')) %>% data.table

## One row per (HCC, set_zero) pair. Splitting with str_split rather than
## separate(into=X1..X8) means a model year whose Table 4 lists more than 8
## lower-ranked categories on one row is not silently truncated.
SetToZero=SetToZeroRAW[!is.na(SetZero),
                       .(set_zero=str_trim(unlist(str_split(SetZero,',')))),
                       by=.(HCC=str_trim(as.character(HCC)))][set_zero!='']


SetToZero = SetToZero %>% data.table
SetToZero %>% setkey(HCC)

SetToZero=SetToZero[,lapply(.SD,ss)][order(HCC)]
# SetToZero[,Z:=1:nrow(.SD),by=HCC] 

# simple standardize (ss)
# object is to get a list of assignments to set to zero based on 
# the variable naming convention used in ModelFactors (how will this change for medicare?)

# create assignments


## age sex bands and definitions

AgeSexBands = read_excel(fn,sheet=hccr_sheet('age_sex_bands'),skip=hccr_skip('age_sex_bands',2),col_types = rep('text',5)) %>%
  filter(!is.na(Model)) # remove blank rows

agest_stmt <- function(variable) {
  as="([MF])AGE_LAST_(\\d\\d)_(\\d\\d)"
  str_detect(variable,as)
}

## 
score_model = function(Model_factor_table,by='pat_id') {
  ## scoring might fail quietly if variables are not defined
  ## we could check for this
  
  ## we should also have one record per id per variable
  ## we should check for this also
  
  ## model data needs to have a by var, in this case patient id
  MFT=Model_factor_table
  function(MD) {
    ScoreByTerm=merge(MD,Model_factor_table,by='Variable')
    scores=ScoreByTerm[,lapply(.SD,sum),by=by] # apply all models (one per column)
    return(scores)
  }
  
}



## model_factors table

model_factors=function(Table,skip=2,MODEL_YEAR=hccr_model_year()) {
  if (is.numeric(Table)) {
    tbl=sprintf("Table %d",as.integer(Table))
  }
  else {
    tbl=Table
  }

  U= read_excel(fn,sheet=tbl,skip=skip,col_types = rep('text',8)) %>%
    filter(!is.na(Model)) %>% # remove blank rows
    pivot_longer(cols=ends_with('Level'),names_to = 'Metal',values_to = 'coeff') %>%
    mutate(coeff=round(as.numeric(coeff),4)) %>%
    mutate(Metal=str_trim(str_remove_all(Metal,'Level')),year=as.integer(MODEL_YEAR))

  names(U)=c('Model','Variable','isUsed','Metal','Coeff','Year')
  # Table 9 spells some interaction names in lower case (RXC_01_x_HCC001)
  # while Tables 6-8 use upper case (RXC_01_X_HCC001); match on upper case
  U$Variable=toupper(U$Variable)
  return(U)
}

ModelFactors = data.table(model_factors(hccr_sheet('model_factors'),hccr_skip('model_factors',2)))

ELIG=ModelFactors[Variable %like% 'ED_']

## can I make this into a function? 
STZCode=SetToZero[,.(set_zero=paste(set_zero,':=0'),HCC=paste(HCC,'==1'))]
STZCode2=STZCode[1:3,.(assignment=paste("X[",HCC,",",set_zero,"]"))]

# higher level function setter
# takes a data table of assignment statements as input
# and returns a function that will execute those
# statements on any data table (assuming it has the required fields)

setterhl = function(codeDT) {
  base = function(X) {}
  # must condense to a single expression
  block = paste("{",paste(codeDT$assignment,collapse=';'),";return(X)}")
  body(base)=parse_expr(block)
  return(base)
}

MF_Wide=ModelFactors %>% dcast.data.table(Model+Variable+isUsed+Year~Metal,value.var='Coeff')

## need smore development regarding partial cartesian joining!

ScoreModel=function(LongForm,MF) {
  merge(LongForm,MF,by=c('Variable','Model'))[value!=0]
}
Metals=hccr_metal_tiers() # from config.yaml model.metal_tiers

## Prescription drug categories (RXC)
## Table 10a maps pharmacy NDC codes, Table 10b maps medical-claim HCPCS
## codes, to the drug categories RXC_01 ... RXC_10

rxc_name=function(rxc) sprintf('RXC_%02d',as.integer(rxc))

rxc_crosswalk=function(sheet,code_col,skip=3) {
  X=read_excel(fn,sheet=sheet,skip=skip,col_types='text') %>% data.table
  setnames(X,c('RXC','RXC_LABEL','CODE'))
  X=X[str_detect(RXC,'^\\d+$') & !is.na(CODE)]  # drop the notes under the table
  X[,RXC:=rxc_name(RXC)]
  setnames(X,'CODE',code_col)
  setkeyv(X,code_col)
  return(X)
}

HCPCS_CODES=rxc_crosswalk(hccr_sheet('rxc_hcpcs_crosswalk'),'HCPCS',hccr_skip('rxc_hcpcs_crosswalk',3))
NDC_CODES=rxc_crosswalk(hccr_sheet('rxc_ndc_crosswalk'),'NDC',hccr_skip('rxc_ndc_crosswalk',3))

## Table 11: drug category hierarchies, same idea as Table 4

RXCSetToZero=read_excel(fn,sheet=hccr_sheet('rxc_hierarchies'),skip=hccr_skip('rxc_hierarchies',3),
                        col_names=c('RXC','SetZero','label'),col_types='text') %>%
  data.table
RXCSetToZero=RXCSetToZero[str_detect(RXC,'^\\d+$') & !is.na(SetZero)]
RXCSetToZero=RXCSetToZero[,.(set_zero=str_trim(unlist(str_split(SetZero,',')))),by=RXC]
RXCSetToZero=RXCSetToZero[,.(HCC=rxc_name(RXC),set_zero=rxc_name(set_zero))]

RXCvars=rxc_name(1:10)
