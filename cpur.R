##
## client data.R
##
## rm(list=ls())
library(data.table)
library(tidyverse)
library(RODBC)
library(odbc)
library(dplyr)
library(lubridate)
library(rlang)

con <- dbConnect(odbc(), 
                 Driver = "SQL Server", 
                 Server = "172.16.0.3",
                 Database = "ModelDevelopment",
                 Trusted_Connection = "True" ,
                 timeout = 5000
)
## hi there

qry="
select  deid_pat_id as pat_id
,claim_id
,place_of_srv_cd
,place_of_srv_cd_desc
,proc_cd_type
,allowed_charge_amt_src as allowed
,total_charge_amt
,cov_amt
,not_covered_amt
,rend_name
,rend_city
,rend_state
,rend_zip
,prov_name
,prov_city
,prov_state
,prov_zip
,prov_npi
,prov_cms_spec_cd
,prov_cms_spec_cd_desc
,rend_spec_src_cd
,rend_spec_src_cd_desc
,rend_cms_spec_cd
,rend_cms_spec_cd_desc
,rend_npi
,allowed_charge_amt_src
,refer_npi
,refer_cms_spec_cd
,refer_cms_spec_cd_desc
,proc_cd
,proc_cd_desc
,bill_type
,bill_type_desc
,prov_tin
,srv_start_dt
,srv_end_dt
,prov_taxonomy_cd_classification
from dbo.claims_20210601_to_20220531
where clm_coverage_type='MED' and clm_status='P'
"

qry2="select * from [ReferenceData].[dbo].[ICD]"



DP=data.table(dbGetQuery(con,qry)) %>% data.table
ICD=data.table(dbGetQuery(con,qry2))
setkey(DP,pat_id) 
ICD=ICD[,.(ICD10=code,short_desc)]
setkey(ICD,ICD10)

## atrociously slow

## DP[,`:=`(isGP=rend_cms_spec_cd_desc %like% 'Practice',
##          isNP=rend_cms_spec_cd_desc %like% 'Nurse Practitioner')]

S=DP[,unique(.SD),.SDcols=c('rend_cms_spec_cd_desc','rend_cms_spec_cd')]
setkey(S,rend_cms_spec_cd_desc)
# S[,GP:=NA]
S[c(10,36,45,53,55,59,72,74,77),GP:=1]
AL_S=S[GP==1]
AL_S[,rend_cms_spec_cd:=NULL]

## DP[,`:=`(isFP=isGP | isNP)]


count_distinct=function(DT,field) {DT[,uniqueN(field)]}
DP=merge(DP,AL_S,by='rend_cms_spec_cd_desc')
V

DP[,.(.N , avg_cost=round(mean(allowed_charge_amt_src,2)),
           min_cost=round(min(allowed_charge_amt_src,2)),
           max_cost=round(max(allowed_charge_amt_src,2))
      ),by=.(proc_cd,proc_cd_desc)]

PC=DP[ , .N , by=.(proc_cd_type, proc_cd, proc_cd_desc, place_of_srv_cd,place_of_srv_cd_desc)]

##
## which place of service to use
## 
cand=DP[,.N,place_of_srv_cd_desc] %>% slice_max(N,n=20) %>% copy()
cand[c(1,6,9,10,11,15),UsePOS:=1]
cand[is.na(UsePOS),UsePOS:=0]

  # |place_of_srv_cd_desc              |      N| UsePOS|
  # |:---------------------------------|------:|------:|
  # |Office                            | 713427|      1|
  # |Telehealth                        |  13329|      1|
  # |Rural Health Clinic               |   6364|      1|
  # |Walk-in Retail Health Clinic      |   5657|      1|
  # |Off Campus-Outpatient Hospital    |   5093|      1|
  # |Federally Qualified Health Center |   2729|      1|

