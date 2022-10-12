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
HCCvars=HCC2[,.N,by=.(CC,CCN=as.numeric(str_replace(CC,'_','.')))][order(CCN),.(HCC=ss(CC),sortorder=1:.N)]

###
### Assign Diagnosis codes to HCC codes
### 
AHCC = merge(D3,HCC2,by='ICD10')[,.(pat_id,pat_age,pat_gender,age.cond,sex.cond,age.split,sex.split,CC)][order(pat_id)]
AHCC[,HCC:=ss(CC)]
AHCC[,CC:=NULL]

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

# return a data table to create a patient who has all the codes 
add_dummy=function(X,codes){}

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
# move code to input table processing

widen = function (X) {
  X %>% dcast.data.table(pat_id+pat_age+pat_gender~ss(HCC),fill=0,fun.aggregate = length)
}




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
