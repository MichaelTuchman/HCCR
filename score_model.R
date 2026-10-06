#############################################################
## Program: score_model.R
## Combine functions created by data munging spreadsheet
## with actual data.  
##
## Dependencies: 
##  assign_hcc (apply_hcc.R)
##  widenfb    (apply_hcc.R)
##  apply_hierarchy, assign_rxc (apply_hcc.R)
##  more_vars  (interactions.R)
##  AgeSexModel(AgeSexFactors.R)
##
## is that they are developed only using the data from
## the excel sheet.  This should be the first place
## the code touches actual coalims data.
##
## might load those as a package, but I am waiting for
## other things to happen first
#############################################################
## package requirements.  These are the packages needed
## to score.  Additional packages are needed to build the
## scoring functions; those are kept in model_Inputs.R
## and don't concert the scoring aspects
#############################################################

require(tidyverse)
require(data.table)
require(readr)
require(lubridate)

# note: it is OK if Variable has missing values
# that just means there will be no contribution
# to the score

Scored_DM=AgeSexModel(DM2[,
                         .(pat_id,
                           pat_gender,
                           ENROLDURATION,
                           pat_age)])

Scored_DM = Scored_DM[,.(pat_id,pat_gender,pat_age,Variable,enrvar)]
Scored_DM = Scored_DM %>% melt.data.table(c('pat_id','pat_gender','pat_age'),
                                          c('Variable','enrvar'),
                                          value.name = 'Variable')
                                        
Scored_DM = Scored_DM %>% select(-variable) %>% filter(!is.na(pat_id)) %>% mutate(value=1)
# put model back - this code needs to be put into the AgeSexModel code

ASB=AgeSexBands[,.(Model,Variable)]
setkey(ASB,Variable)
Scored_DM=merge(Scored_DM,ASB,by='Variable')

#############################################################
## Assign HCC to diagnostic codes
#############################################################

AHCCf=assign_hcc(HCC2)  # this will be our exported function
HIERf=apply_hierarchy(SetToZero)    # Table 4
STEP2=AHCCf(D3) %>% HIERf

#############################################################
## Assign drug categories (RXC) from HCPCS codes on medical
## claims and, when the data has an NDC table (pat_id, NDC), from
## pharmacy fills.
#############################################################

RXCf=assign_rxc(HCPCS_CODES)
RXHIERf=apply_hierarchy(RXCSetToZero)  # Table 11
NDCf=assign_rxc(NDC_CODES)
RX_CODES=RXCf(HCPCS)
if (exists('NDC')) RX_CODES=unique(rbind(RX_CODES,NDCf(NDC)))  # pharmacy fills, when the data has them
STEP2RX=RX_CODES %>% RXHIERf
STEP2RX=merge(STEP2RX,DM2[,.(pat_id,pat_age,pat_gender)],by='pat_id')

STEP2=bind_rows(STEP2,STEP2RX[,.(pat_id,pat_age,pat_gender,HCC)])

#############################################################
## Add Model Variable based on
#############################################################



STEP2A=merge(STEP2,Scored_DM,by='pat_id')

STEP2A=STEP2A[,.(pat_id,
                 pat_age=pat_age.x,
                 pat_gender=pat_gender.x,
                 HCC,
                 AgeBAND=Variable)]  


STEP2A=merge(STEP2A,AgeSexBands[,.(AgeBAND=Variable,Model)],by='AgeBAND')

# rpt=function(DT) {DT[,.(pat_id,Model,AgeBAND,pat_gender,AGE_LAST,HCC)][order(pat_id)]}
# f= line should go in a previous file

f=widenfb(c(HCC2$HCC,RXCvars)) # all these function builders must take spreadsheet tables as arguments
STEP3=f(STEP2A)

## Every patient goes through the rules, not only those with a diagnosis
## or drug category. The rules score a patient with nothing to report too:
## the infant model gives every infant a maturity x severity variable
## (a healthy age-1 infant gets AGE1_X_SEVERITY1), and Table 1 says so. A
## female infant has no age/sex band at all (Table 5 has male infant bands
## only), so she would otherwise vanish from the scoring. Patients missing
## from STEP3 are added with every category set to 0.

patient_base = unique(DM2[,.(pat_id,pat_age,pat_gender)])
patient_base[,Model:=model_for_age(pat_age)]
check_models = patient_base[is.na(Model)]
if (nrow(check_models)>0)
  warning(nrow(check_models),' patient(s) have an age outside every model in Table 1 and will not be scored: ',
          paste(head(check_models$pat_id,5),collapse=', '))
patient_base=merge(patient_base[!is.na(Model)],
                   unique(Scored_DM[,.(pat_id,AgeBAND=Variable)]),by='pat_id',all.x=TRUE)

## a patient's age/sex band must belong to their own model
band_model = merge(patient_base[!is.na(AgeBAND)],AgeSexBands[,.(AgeBAND=Variable,band_model=Model)],by='AgeBAND')
stopifnot(all(band_model$Model==band_model$band_model))

no_categories = patient_base[!pat_id %in% STEP3$pat_id]
STEP3 = rbind(STEP3,no_categories,fill=TRUE)
category_cols = setdiff(names(STEP3),c('pat_id','pat_age','pat_gender','AgeBAND','Model'))
for (v in category_cols) set(STEP3,which(is.na(STEP3[[v]])),v,0L)

## note that AGE0_MALE and AGE1_MALE act like both rows and columns
## in this program.  This will become important later. It occurs
## to me while I'm writing this that we need age variables in 
## the HCC dataset as well as HCC variables.  Then we can


STEP3[,`:=`(AGE_LAST=pat_age,
            AGE0_MALE=as.integer(AgeBAND=='AGE0_MALE'),
            AGE1_MALE=as.integer(AgeBAND=='AGE1_MALE'))]



# temporary while we fix eligibility, which we kinda need to do


## RED FLAG: HARDCODE. step3 : AGE_LABELS AGE0_MALE, AND AGE1_MALE
## THESE SHOULD BE READ IN DIRECTLY FROM A SPREADSHEET
## BUT WE CAN FIX THIS LATER SINCE THE CATEGORIES ARE PRETTY CONSTANT YEAR TO 
## YEAR

## LOOK AT WHERE THE AGE/SEX FACTORS ARE DEFINED AND WHY THIS WASN'T SET THERE
## this feels hardcoded to me.   What if we inserted some female categories?
## right now, the program is assuming HHS is only looking at age 0,1 Males

## The root issue is that we're treating the age sex variables as different
## from the HCC variables.  However, AGE0_MALE and AGE1_MALE play a direct
## role in both the age/sex AND the regression model.
## this assignment needs to be moved into the more_vars function


## Rules in Tables 6-8 may test ENROLDURATION (CY2025's HCC_ED1..HCC_ED11:
## enrollment duration for patients with at least one HCC), so it has to
## be on the table the rules run against.
STEP3=merge(STEP3,unique(DM2[,.(pat_id,ENROLDURATION)]),by='pat_id',all.x=TRUE)

## it is not necessary to remove NA at this point.
STEP4 = more_vars(STEP3)
STEP4[,ENROLDURATION:=NULL] # only needed by the rules; not a scored variable
varnames=copy(names(STEP4))
ids = c('pat_id','pat_age','pat_gender','AgeBAND','Model')
measures=setdiff(varnames,ids)

## YELLOW: 
## convert doubles to ints.  don't know why these fell out as doubles
## we need to enforce types earlier in the process

## now melt and score
## ignore the issue with ints being converted to doubles
## when converting to SQL we will need to be able to pivot longer
## these should be abstract functions and not immediately
## executable statements, perhaps?

## developer note:
## if we were doing this in SQL, the functions more_vars,assign_hcc,widenfb would
## all be views on SQL tables.  This means that either the target language
## is a parameter, and perhaps we have method dispatch on target language?

# YOU CAN SAFELY IGNORE THE TYPE CASTING WARNING FROM THIS STEP
STEP5=melt.data.table(STEP4,ids,measures,na.rm=TRUE)
STEP5[,Variable:=variable]
STEP5[,variable:=NULL]

### Get coefficients
### again, make these functions?
### step6 = function(STEP5, etc.)
### this cartesian jo

## RED FLAG : hardcode metal value to avoid cartesian join
## but there are other, better ways, even using data table
## task: look into these

## STEP6=merge(STEP5,ModelFactors[Metal==METAL_LEVEL],by.x=c('variable','Model'),
##                               ,by.y=c('Variable','Model'))

## step 7 is useful for seeing how a score breaks down for a particular person

## STEP7=STEP6[,.(pat_id,Model,variable,pat_age,pat_gender,AgeBAND,value,Coeff,variable,y=value*Coeff)][y!=0]

## STEP8 = STEP7[,sum(y),by=.(pat_id,Model,pat_age,pat_gender,AgeBAND)]

# merge the Age Sex Enrollment variable in
outvars=c('pat_id','Model','pat_gender','pat_age','Variable','value')


## Where the rules set a variable that the age/sex step also set (an age 0
## male infant with no newborn diagnosis is moved from AGE0_MALE to AGE1_MALE),
## the rules' value is the one that counts.
Scored_DM_kept = Scored_DM[!is.na(pat_id)][!STEP5[,.(pat_id,Model,Variable)],on=.(pat_id,Model,Variable)]
STEP6 = bind_rows(STEP5[,..outvars],Scored_DM_kept[,..outvars]) %>% distinct
setkey(STEP6,pat_id,Model,Variable)

# step7 is useful for breaking down how a risk score was arrived at

# STEP7= ScoreModel(STEP6,MF_Wide) %>% melt.data.table(c('Model','Variable','pat_id','pat_gender','pat_age','isUsed','Year'),
#                                                    c('Bronze','Silver','Gold','Platinum','Catastrophic'))
  
STEP7=ScoreModel(STEP6,MF_Wide)                                                   
setkey(STEP7,pat_id)

byVars = c('pat_id','Model','pat_gender','pat_age','isUsed')


STEP8 = STEP7[,lapply(.SD,sum),.SDcols=Metals,by= c('pat_id','Model','pat_gender','pat_age','isUsed')][,.(pat_id,Model,pat_gender,pat_age,isUsed,Bronze,Silver,Gold,Platinum,Catastrophic)]
setkey(STEP8,pat_id)

dups=STEP8[,.N,by='pat_id'][N>1]   # get all dups
setkey(dups,pat_id)

dup_resolve=STEP8[dups][,head(.SD,1),by=pat_id] # get itms in the original data set then take first


STEP8=STEP8[!dups] # remove dups

Answer=bind_rows(STEP8,dup_resolve %>% select(-N)) # put them back bu tonly 1

## Cost-sharing adjustment (CY2025 on). Patients may carry a csr_indicator
## column (Table 13); without one everyone is indicator 1, i.e. unadjusted.
if (!is.null(CSRByIndicator)) Answer = apply_csr(Answer, DM2, CSRByIndicator)

write_csv(Answer,hccr_output_csv()) # path from config.yaml output.risk_scores_csv

cleanup=function(){
rm(list =ls(pattern='STEP'))
rm(D3,DM,DM2,DM3,Scored_DM)
}

