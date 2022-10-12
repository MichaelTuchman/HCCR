# get diagnosis sample working

D2=readRDS('D2.RData')

compact_diags=function(diags){
  diags %>% pivot_longer(cols=starts_with('diag'),names_to='drop',values_to='ICD10') %>% 
  filter(!is.na(ICD10)) %>% select(-drop) %>% distinct
}

D3=compact_diags(D2) %>% data.table

# set to zero function
# have to set key of patient diags and set_to_zero to HCC


# here' show this code works;
# create pairs of hcc by patient merge(pt_diags_pt_diags)
# merge with the set to zero requirement table - these are the ones we need deleted
# Relabel the set_to_zero has HCC so we can merge back to original table
# when we merge back, delete the ones that match, keeping the ones that don't [is.na(set_zero)]
#
# this is the following correlated query in sql
# delete from pt_diags 
# where hcc in (select set_zero has hcc from pt_diags join set_to_zero_table on pt_diags.hcc=set_to_zero_table.hcc))

D3[,pat_age := round(as.numeric((mdy('05-31-2022') -ymd(D3$pat_birth_dt)) ) / 365.2453)]

## assign HCC codes,but  will be invalid because of age sex split not yet being checked

## create data table with specific columns, assumed all numeric
cdt=function(varlist) {
  ## "a","b","c"
  eval(parse(text=paste('data.table(',paste(varlist,"=as.numeric()", collapse=','),')')))
}

# create a blank data table used only for its names

standard_varname=function(CCNWhole,CCNPart) {
  CCNPart=replace_na(CCNPart,0)
  ifelse(CCNPart==0,sprintf("HHS_HCC%03d",CCNWhole),
         sprintf("HHS_HCC%03d_%1d",CCNWhole,CCNPart)
  )
  
}

# Build a table of the HCC varaibles that will be used over gain
# this helps create a sort order that might make this easier to manage

# turn dashes back to decimals for sorting
HCCvars=HCC2[,.N,by=.(CC,CCN=as.numeric(str_replace(CC,'_','.')))]

# split the HCC code up into whole and decimal parts
HCCvars[,`:=`(CCNWhole=floor(CCN))]
HCCvars=HCCvars[,`:=`(CCNPart=CCN-CCNWhole)]
HCCvars[,`:=`(CCNPart2=as.integer(round(10*CCNPart)))]

HCCvars=HCCvars[order(CCNWhole,CCNPart2)]
HCCvars[,HCC:=CC]
HCCvars[,CC:=NULL]
# order as though cc were numeric and then assign a sort order to them.  

HCCvars[,sortorder:=1:.N]

HCC3=HCCvars[order(CCN),paste('HCC',HCC,sep='')]
HCCvars[,standardized_var:=standard_varname(CCNWhole,CCNPart2)]

#### 
#### Assign HCC to each diagnosis
#### and make sure age ranges match expectations
####

# merge codes, select fields, and order result by patient iD

AHCC = merge(D3,HCC2,by='ICD10')[,.(pat_id,pat_age,pat_gender,age.cond,sex.cond,age.split,sex.split,CC)][order(pat_id)]

# translate condition into a high and lo age that the patient age must fit to

AHCC[str_detect(age.split,'(.*)<=age<=(.*)'),`:=`(lo=str_match(age.split,'(.*)<=age<=(.*)')[,2],
                                                  hi=str_match(age.split,'(.*)<=age<=(.*)')[,3])]

# this default condition fits all ages
AHCC[,`:=`(lo=-Inf,hi=+Inf)]

# create age bands for age splits
AHCC[str_detect(age.split,'(.*)<=age<=(.*)'),`:=`(lo=as.numeric(str_match(age.split,'(.*)<=age<=(.*)')[,2]),
                                                  hi=as.numeric(str_match(age.split,'(.*)<=age<=(.*)')[,3]))]

AHCC[str_detect(age.split,'age < (.*)'),`:=`(lo=-Inf,
                                            hi=as.numeric(str_match(age.split,'age < (.*)')[,2]))]

AHCC[str_detect(age.split,'age >= (.*)'),`:=`(lo=as.numeric(str_match(age.split,'age >= (.*)')[,2],
                                            hi=+Inf))]



# really should be a loop
infant.age.split=function(str) {
  j=str_match('age = 2','age *= (\\d)')[,2]
  as.integer(j)
}

AHCC[str_detect(age.split,'age = (\\d)'),`:=`(j=infant.age.split(age.split))]
AHCC[str_detect(age.split,'age = (\\d)'),`:=`(hi=j,lo=j)]
AHCC[,j:=NULL]

AHCC[,age.fit:=(pat_age<hi) & (pat_age>=lo)]

# need to do this with age cond and gender too



# replication of pairlists is defunct


## need to fix
## create age bands for age conditions
##

AHCC[,`:=`(lo.2=-Inf,hi.2=+Inf)]

# pull high and low values from inequalities and put them in their own columns
# for easy testing

AHCC[str_detect(age.cond,'(.*)<=age<=(.*)'),`:=`(lo.2=as.numeric(str_match(age.cond,'(.*)<=age<=(.*)')[,2]),
                                                  hi.2=as.numeric(str_match(age.cond,'(.*)<=age<=(.*)')[,3]))]

AHCC[str_detect(age.cond,'age < (.*)'),`:=`(lo.2=-Inf,
                                             hi.2=as.numeric(str_match(age.cond,'age < (.*)')[,2]))]

AHCC[str_detect(age.cond,'age >= (.*)'),`:=`(lo.2=as.numeric(str_match(age.cond,'age >= (.*)')[,2],
                                                            hi=+Inf))]

AHCC[,age_fit.2:=(pat_age>=lo.2) & (pat_age<hi.2)]

## handle sex issues.  Standardize to M and F

AHCC[,sex.split:=str_sub(toupper(sex.split),1,1)]
AHCC[,sex.cond:=str_sub(toupper(sex.cond),1,1)]

# g says two quantities must match if they are non-NA

g=function(S,y) (is.na(S)|S==y) # needs some lazy eval to be even more efficient

## remove items where sex condition does not meet patient gender
## but count them first for fraud detection
require(knitr)
AHCC[,.N,by=.(g(sex.cond,pat_gender))] %>% kable
AHCC=AHCC[g(sex.cond,pat_gender)]
AHCC=AHCC[g(sex.split,pat_gender)]
AHCC=AHCC[age.fit==TRUE & age_fit.2==TRUE,.(pat_id,pat_gender,HCC=CC,pat_age)]
AHCC=distinct(AHCC)
setkey(AHCC,HCC)

# standardize variable names

AHCCS=merge(AHCC,HCCvars,by='HCC')[,.(pat_id,pat_gender,pat_age,standardized_var)]

# standardize variable names in set to zero tables
# 
# dashed_hcc=SetToZeroRAW[,c('CCNWhole','CCNPart'):=tstrsplit(HCC,'_')]
# dashed_hcc[,CCNPart:=as.integer(CCNPart)]
# dashed_hcc[,CCNWhole:=as.integer(CCNWhole)]
# dashed_hcc[,HCCStd:=standard_varname(CCNWhole,CCNPart)]
# step2_dashed_hcc=dashed_hcc[,.(HCCStd,SetZero)]
# 
# dashed_hcc[,c('ZeroW','ZeroP'):=tstrsplit(SetZero,'_')]
# dashed_hcc[,`:=`(ZeroW=as.integer(ZeroW),ZeroP=as.integer(ZeroP))]
# dashed_hcc[,`:=`(ZeroStd=standard_varname(ZeroW,ZeroP))]
# 
# dashed_hcc[,.(HCCStd=standardized_var,ZeroStd)]

# came out null

step2_dashed_hcc[,paste('X',1:8,sep=''):=tstrsplit(SetZero,',')]
P=step2_dashed_hcc[!is.na(X1)]

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

P[,paste('Y',1:8,sep=''):=lapply(.SD,ss),.SDcols=paste0('X',1:8)]
P[,paste('X',1:8,sep=''):=rep(NULL,8)]
P[,SetZero:=NULL]

# create assignments




## we have now removed all HCC assignments incompatible with age or gender
## ---------------------------------------------------------------------
## reduce assignments taking only the first 
## merge but without using cartesian products

b2=function(csv) str_split(csv,',')
chain_assign=function(z,pref='HCC') {
  b=function(w,pref)  paste(pref,w,'=0',sep='')
  lapply(str_split(z,','),function(x) paste(sapply(x,b,pref),collapse=',' )) %>% unlist
}

dt_assign=function(CC,z,pref='HCC') {
  paste('X[',pref,CC,'==1,`:=`(',chain_assign(z,pref),')]',sep='')
}
