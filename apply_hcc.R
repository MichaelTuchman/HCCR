

## assign HCC codes,but  will be invalid because of age sex split not yet being checked

## create data table with specific columns, assumed all numeric
cdt=function(varlist) {
  ## "a","b","c"
  eval(parse(text=paste('data.table(',paste(varlist,"=as.numeric()", collapse=','),')')))
}

# create a blank data table used only for its names

g=function(S,y) (is.na(S)|S==y) # needs some lazy eval to be even more efficient

# really should be a loop
infant.age.split=function(str) {
  j=str_match('age = 2','age *= (\\d)')[,2]
  as.integer(j)
}


# Build a table of the HCC varaibles that will be used over gain
# this helps create a sort order that might make this easier to manage

# turn dashes back to decimals for sorting
HCCvars=HCC2[,.N,by=.(CC,CCN=as.numeric(str_replace(CC,'_','.')))][order(CCN),.(HCC=ss(CC),sortorder=1:.N)]

###
### Assign Diagnosis codes to HCC codes
### 

###
### We need to go back and remember where hcc2 is created
###

### needs to be redone in proper format

assign_hcc=function(HCC) 
  function(Diagnostic) {
  AHCC = merge(Diagnostic,HCC,by='ICD10')[,.(pat_id,pat_age,pat_gender,age.cond,sex.cond,age.split,sex.split,HCC)][order(pat_id)]
  # AHCC[,HCC:=ss(CC)]
  # AHCC[,CC:=NULL]

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


  AHCC[str_detect(age.split,'age = (\\d)'),`:=`(j=infant.age.split(age.split))]
  AHCC[str_detect(age.split,'age = (\\d)'),`:=`(hi=j,lo=j)]
  AHCC[,j:=NULL]

  AHCC[,age.fit:=(pat_age<hi) & (pat_age>=lo)]

# 
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
## remove items where sex condition does not meet patient gender
## but count them first for fraud detection
require(knitr)
  AHCC[,.N,by=.(g(sex.cond,pat_gender))] %>% kable
  AHCC=AHCC[g(sex.cond,pat_gender)]
  AHCC=AHCC[g(sex.split,pat_gender)]
  # need to review how HCC is defined
  # AHCC=AHCC[age.fit==TRUE & age_fit.2==TRUE,.(pat_id,pat_gender,pat_age)]
  # AHCC=distinct(AHCC)
  setkey(AHCC,HCC)
  # AHCC[,HCC:=ss(HCC)]
  
  AHCC = AHCC[,.(pat_id,pat_age,pat_gender,HCC)]
  
  return(distinct(AHCC))
  }



# widen = function (X,vars=sort(unique(HCC2$HCC))) {
#   TRY2=data.table(pat_id=NA,pat_gender=NA,HCC=sort(unique(HCC2$HCC)))
#   X=X %>% bind_rows(TRY2) %>% dcast.data.table(pat_id+pat_age+pat_gender~HCC,fill=0,fun.aggregate = length)
#   X=X[!is.na(pat_id)]
#   return(X)
# }

## inputs which variables to use as ID vars, which are var vars
## this does two important functions that ordinary pivoting does not
## ensures all variables in the list will appear as columns
## even if they don't have a row in the data

widenfb = function(HCC) {
  vars=sort(unique(HCC$HCC))
  DUMMY = data.table(pat_id=NA,pat_gender=NA,HCC=sort(unique(HCC2$HCC)))
  function(X) {
    X=X %>% bind_rows(DUMMY) %>% dcast.data.table(pat_id+pat_age+pat_gender+AgeBAND+Model~HCC,fill=0,fun.aggregate = length)
    X=X[!is.na(pat_id)]
    return(X)
  }
                    
}


## when do I actually want to ingest my inputs?
## X must have AgeBAND, pat_id, AGE_LAST, pat_gender, HCC,Model
## columns as variables







