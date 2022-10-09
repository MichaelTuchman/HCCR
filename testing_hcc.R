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

remove_subordinate_hcc = function(pt_diags,set_to_zero_tbl) {
  merge(pt_diags,
        merge(merge(pt_diags,pt_diags),set_to_zero_tbl)[,HCC:=set_zero],
        all.x=TRUE)[is.na(set_zero)]
}  

# as much as humanly possible, I want to avoid straight assignment code
# since I can do this as a merge, I will do so.

# iterate over the rows of J and execute 28 or so data table statements
# on the main demographic table

D3[,pat_age := round(as.numeric((mdy('05-31-2022') -ymd(D3$pat_birth_dt)) ) / 365.2453)]

## assign HCC codes,but  will be invalid because of age sex split not yet being checked


#### 
#### Assign HCC to each diagnosis
####


AHCC = merge(D3,HCC2,by='ICD10')[,.(pat_id,pat_age,pat_gender,age.cond,sex.cond,age.split,sex.split,CC)][order(pat_id)]

# when we have a compound age condition such as 65<=age<=100, split it into two conditions, since R cannot handle this

AHCC[str_detect(age.split,'(.*)<=age<=(.*)'),`:=`(lo=str_match(age.split,'(.*)<=age<=(.*)')[,2],
                                                  hi=str_match(age.split,'(.*)<=age<=(.*)')[,4])]


AHCC[is.na(age.split.fixed),age.split.fixed:=expr(TRUE)]
JJ=AHCC$age.cond.fixed %>% unique

# only problem is that this is not vectorizable
# i would really need to create a vector of functions

str_2_agef = function(str) {
  tmp=function(age) {}
  body(tmp)=parse_expr(str)
  return(tmp)
}

AHCC[,`:=`(lo=-Inf,hi=+Inf)]
AHCC[str_detect(age.split,'(.*)<=age<=(.*)'),`:=`(lo=as.numeric(str_match(age.split,'(.*)<=age<=(.*)')[,2]),
                                                  hi=as.numeric(str_match(age.split,'(.*)<=age<=(.*)')[,3]))]

AHCC[str_detect(age.split,'age < (.*)'),`:=`(lo=-Inf,
                                            hi=as.numeric(str_match(age.split,'age < (.*)')[,2]))]

AHCC[str_detect(age.split,'age >= (.*)'),`:=`(lo=as.numeric(str_match(age.split,'age >= (.*)')[,2],
                                            hi=+Inf))]
AHCC[,age.fit:=(pat_age<hi) & (pat_age>=lo)]

## need to fix
AHCC[,`:=`(lo.2=-Inf,hi.2=+Inf)]

AHCC[str_detect(age.cond,'(.*)<=age<=(.*)'),`:=`(lo.2=as.numeric(str_match(age.cond,'(.*)<=age<=(.*)')[,2]),
                                                  hi.2=as.numeric(str_match(age.cond,'(.*)<=age<=(.*)')[,3]))]

AHCC[str_detect(age.cond,'age < (.*)'),`:=`(lo.2=-Inf,
                                             hi.2=as.numeric(str_match(age.cond,'age < (.*)')[,2]))]

AHCC[str_detect(age.cond,'age >= (.*)'),`:=`(lo.2=as.numeric(str_match(age.cond,'age >= (.*)')[,2],
                                                            hi=+Inf))]



AHCC[,age_fit.2:=(pat_age>=lo.2) & (pat_age<hi.2)]

## handle sex issues

AHCC[,sex.split:=str_sub(toupper(sex.split),1,1)]

g=function(S,y) (is.na(S)|S==y) # needs some lazy eval to be even more efficient

## remove items where sex condition does not meet patient gender
## but count them first for fraud detection
require(knitr)
AHCC[,.N,by=.(g(sex.cond,pat_gender))] %>% kable
AHCC=AHCC[g(sex.cond,pat_gender)]
AHCC=AHCC[g(sex.split,pat_gender)]
AHCC=AHCC[age.fit==TRUE & age_fit.2==TRUE,.(pat_id,pat_gender,HCC=CC)]
AHCC=distinct(AHCC)
setkey(AHCC,HCC)
## we have now remoted all HCC assignments incompatible with age or gender

## ---------------------------------------------------------------------
## reduce assignments taking only the first 
