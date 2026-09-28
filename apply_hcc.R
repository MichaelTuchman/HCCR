

## assign HCC codes, checking the age/sex conditions and splits from Table 3

## create data table with specific columns, assumed all numeric
cdt=function(varlist) {
  ## "a","b","c"
  eval(parse(text=paste('data.table(',paste(varlist,"=as.numeric()", collapse=','),')')))
}

# create a blank data table used only for its names

g=function(S,y) (is.na(S)|S==y) # needs some lazy eval to be even more efficient

# Test an age condition from Table 3 against a vector of ages.
# The workbook writes conditions like '9 <= age <= 64', 'age >=15',
# 'age < 50' and 'age = 0'. A blank condition always passes.
# An unrecognised condition stops the run so that a new workbook
# format is noticed rather than silently ignored.

age_condition_met=function(cond,age) {
  cond=str_remove_all(cond,' ')
  ok=rep(TRUE,length(age))

  between=str_match(cond,'^(\\d+)<=age<=(\\d+)$')
  one_sided=str_match(cond,'^age(<=|>=|<|>|=)(\\d+)$')

  known=is.na(cond) | !is.na(between[,1]) | !is.na(one_sided[,1])
  if (!all(known))
    stop('Unrecognised age condition(s): ',paste(unique(cond[!known]),collapse=', '))

  i=!is.na(between[,1])
  ok[i]=age[i]>=as.numeric(between[i,2]) & age[i]<=as.numeric(between[i,3])

  i=!is.na(one_sided[,1])
  op=one_sided[i,2]; x=as.numeric(one_sided[i,3]); a=age[i]
  ok[i]=ifelse(op=='<',a<x,
        ifelse(op=='<=',a<=x,
        ifelse(op=='>',a>x,
        ifelse(op=='>=',a>=x,a==x))))
  return(ok)
}


# Build a table of the HCC varaibles that will be used over gain
# this helps create a sort order that might make this easier to manage

# turn dashes back to decimals for sorting
HCCvars=HCC2[,.N,by=.(CC,CCN=as.numeric(str_replace(CC,'_','.')))][order(CCN),.(HCC=ss(CC),sortorder=1:.N)]

###
### Assign Diagnosis codes to HCC codes
### 

## Table 3 has two kinds of age/sex test:
##  age.cond, sex.cond   (MCE conditions) the diagnosis is not valid for
##                       this patient at all, e.g. a pregnancy code at age 6
##  age.split, sex.split (CC splits) the same diagnosis maps to a different
##                       HCC depending on age, e.g. breast cancer under or
##                       over 50. Only the matching row is kept.
## The workbook tests MCE conditions on AGE_AT_DIAGNOSIS and splits on
## AGE_LAST; the client data only has one age, pat_age, so it is used
## for both.

assign_hcc=function(HCC) 
  function(Diagnostic) {
  AHCC = merge(Diagnostic,HCC,by='ICD10')[,.(pat_id,pat_age,pat_gender,age.cond,sex.cond,age.split,sex.split,HCC)][order(pat_id)]

## handle sex issues.  Standardize to M and F

  AHCC[,sex.split:=str_sub(toupper(sex.split),1,1)]
  AHCC[,sex.cond:=str_sub(toupper(sex.cond),1,1)]

# g says two quantities must match if they are non-NA
## remove items where age or sex condition does not fit the patient
## but count them first for fraud detection
  AHCC[,`:=`(age.fit=age_condition_met(age.cond,pat_age),
             age.split.fit=age_condition_met(age.split,pat_age),
             sex.fit=g(sex.cond,pat_gender),
             sex.split.fit=g(sex.split,pat_gender))]
  require(knitr)
  AHCC[,.N,by=.(age.fit,sex.fit)] %>% kable %>% print
  AHCC=AHCC[age.fit & age.split.fit & sex.fit & sex.split.fit]
  setkey(AHCC,HCC)
  
  AHCC = AHCC[,.(pat_id,pat_age,pat_gender,HCC)]
  
  return(distinct(AHCC))
  }


## Table 4 (and Table 11 for drugs): when a patient has the category in
## HCC, the categories in set_zero are dropped. Works on the long
## patient x category table, before widening.

apply_hierarchy=function(set_to_zero_tbl)
  function(pt_codes) {
  drop=merge(pt_codes[,.(pat_id,HCC)],set_to_zero_tbl,by='HCC',allow.cartesian=TRUE)[,.(pat_id,HCC=set_zero)]
  pt_codes[!drop,on=.(pat_id,HCC)]
  }


## Prescription drug categories: map a patient x code table (column
## named HCPCS or NDC, matching the crosswalk) to RXC_01 ... RXC_10.
## The drug categories go in the HCC column so they widen alongside
## the condition categories.

assign_rxc=function(crosswalk)
  function(pt_codes) {
  code_col=key(crosswalk)
  merge(pt_codes,crosswalk,by=code_col)[,.(pat_id,HCC=RXC)] %>% unique
  }


# widen = function (X,vars=sort(unique(HCC2$HCC))) {
#   TRY2=data.table(pat_id=NA,pat_gender=NA,HCC=sort(unique(HCC2$HCC)))
#   X=X %>% bind_rows(TRY2) %>% dcast.data.table(pat_id+pat_age+pat_gender~HCC,fill=0,fun.aggregate = length)
#   X=X[!is.na(pat_id)]
#   return(X)
# }

## inputs which variables to use as ID vars, which are var vars
## this does two important functions that ordinary pivoting does not
## ensures all variables in vars (HCCs and RXCs) will appear as columns
## even if they don't have a row in the data

widenfb = function(vars) {
  DUMMY = data.table(pat_id=NA,pat_gender=NA,HCC=sort(unique(vars)))
  function(X) {
    X=X %>% bind_rows(DUMMY) %>% dcast.data.table(pat_id+pat_age+pat_gender+AgeBAND+Model~HCC,fill=0,fun.aggregate = length)
    X=X[!is.na(pat_id)]
    return(X)
  }
                    
}


## when do I actually want to ingest my inputs?
## X must have AgeBAND, pat_id, AGE_LAST, pat_gender, HCC,Model
## columns as variables







