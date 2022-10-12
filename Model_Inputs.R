## Read Spreadsheet Tables

rm(list=ls())

## packages and parameters
# program: setup.R
# load project dependencies

require(lobstr)
require(rlang)
require(tidyverse)
require(data.table)
require(readxl)
require(readr)
require(lubridate)

# date  utility and helper functions
fp=function(model.year) {
 file.path(sprintf('CY%d DIY tables 06.30.%d.xlsx',model.year,model.year))
}

fn=fp(2022)

## hcc_group : Data structures to write code for computing the 
## grouping variables

## HCC Grouping variables
extra_vars=function(SheetNoC) {
  read_excel(fn,
             sheet = SheetNoC,
             col_names=c('Model','Variable','Description','Used','Formula'),skip=4) %>%
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

Adult=extra_vars('Table 6')
Child=extra_vars('Table 7')
Infant=extra_vars('Table 8')

AllAges=rbind(Adult,Child,Infant) %>% fill_in_blank_rows
rm(list=c('Adult','Child','Infant')) # now redundant


## map diagnosis to HCC

## in order to replace periods with underlines in HCC codes

dash=function(str) str_replace_all(str,'\\.','_')

HCC=read_excel(fn,sheet='Table 3', skip=4, 
                         col_names=c('obs','ICD10','icd10.label','valid.2021','valid.2022','age.cond','sex.cond','age.split','sex.split','cc.1','cc.2','cc.3','comment'),
                         col_types=c(rep('text',9),rep('numeric',3),'text'))


HCC2=HCC%>%
# mutate(across(starts_with('cc'),dash)) %>% 
  pivot_longer(starts_with('cc'),names_to=NULL,values_to = 'CC') %>% 
  filter(!is.na(CC)) %>%
  filter(valid.2022=='Y') %>%
  select(-valid.2021,-obs) %>% mutate(CC=dash(as.character(CC))) %>% data.table

# simplify Sex conditions

HCC2[!is.na(sex.cond),`:=`(sex.cond=toupper(str_sub(sex.cond,1,1)))]
HCC2[!is.na(sex.split),`:=`(sex.split=toupper(str_sub(sex.cond,1,1)))]

# convert dots to underlines

# ------------- HCC set to zero condition table -------------------

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

SetToZeroRAW=read_excel(fn,skip=3,
                     sheet = 'Table 4',col_names = c('Obs','HCC','SetZero','label')) %>% data.table

SetToZero=SetToZeroRAW %>%
       select(-Obs) %>%
       separate(SetZero,sep=',',into=paste('X',1:8,sep='')) %>%
       mutate(across(.fns=~(str_trim(.x)))) %>%
       pivot_longer(starts_with('X')) %>% mutate(label=NULL) %>%
       rename(set_zero=value) %>% filter(!is.na(set_zero)) %>% mutate(name=NULL)


SetToZero = SetToZero %>% data.table
SetToZero %>% setkey(HCC)

SetToZero[,lapply(ss,.SD)]

# simple standardize (ss)
# object is to get a list of assignments to set to zero based on 
# the variable naming convention used in ModelFactors (how will this change for medicare?)



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

# create assignments

SetToZero=SetToZero[,lapply(.SD,ss)][order(HCC)]
SetToZero[,Z:=1:nrow(.SD),by=HCC]

## age sex bands and definitions

AgeSexBands = read_excel(fn,sheet='Table 5',skip=2,col_types = rep('text',5)) %>% 
  filter(!is.na(Model)) # remove blank rows

agest_stmt <- function(variable) {
  as="([MF])AGE_LAST_(\\d\\d)_(\\d\\d)"
  str_detect(variable,as)
}




## model_factors table

model_factors=function(Table,MODEL_YEAR=2022) {
  if (is.integer(Table)) {
    tbl=sprintf("Table %d",Table)
  }
  else {
    tbl=paste('Table',Table,sep=' ')
  }
  
  U= read_excel(fn,sheet=tbl,skip=2,col_types = rep('text',8)) %>%
    filter(!is.na(Model)) %>% # remove blank rows 
    pivot_longer(cols=ends_with('Level'),names_to = 'Metal',values_to = 'coeff') %>%
    mutate(coeff=round(as.numeric(coeff),4)) %>%
    mutate(Metal=str_trim(str_remove_all(Metal,'Level')),year=as.integer(MODEL_YEAR))
  
  names(U)=c('Model','Variable','isUsed','Metal','Coeff','Year')
  return(U)
}

ModelFactors = data.table(model_factors(9))

ELIG=ModelFactors[Variable %like% 'ED_']

