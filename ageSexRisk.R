#
# data munging
#

# inputs: DM comes from readClaims.R
# TBL1 comes from readSpreadsheet.R
require(data.table)
require(tidyverse)


DM=AMTS[,.N,.(deid_pat_id,pat_gender,pat_birth_dt)]
DM$age_rpt=as.numeric((as.Date('2022-05-31') - as.Date(DM$pat_birth_dt))/365.2453)
DM=DM[!is.na(pat_birth_dt)]


# age to band to label
age_labels=TBL1 %>% mutate(z=str_sub(y,2)) %>% select(age_low,z) %>% arrange(age_low) %>% unique()
age_labels=bind_rows(tibble(age_low=0,z='AGE_LAST_0_2'),age_labels) %>% unique

AgeVec=TBL1$age_low %>% unique %>% c(.,0,Inf) %>% sort()

DM %>% setkey(deid_pat_id)
DM[,`:=`(age_low=AgeVec[findInterval(age_rpt,AgeVec)])]

# if over 65 we will need to apply the medicare risk model

AL=data.table(age_labels)
DM2=merge(DM,AL,by='age_low')

# merge patient gender back for better prep
DM2[,AgeSexBand:=paste0(pat_gender,z)]
DM2[,z:=NULL]

# get the age/sex factors.   Use Silver Level Plan for now.
AgeSexRisk=merge(DM2,TBL1 %>% select(Model,Silver,y),by.x='AgeSexBand',by.y='y')
AgeSexRisk[,`:=`(age_low=NULL,Risk_12mo=Silver,Silver=NULL)]

## end of age band settings

## todo: make plan type a variable rather than a constant



