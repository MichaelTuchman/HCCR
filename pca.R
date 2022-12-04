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
################################################################
## uSER iNPUT 
################################################################
UNNEEDED_SECTIONS=c('Prolonged Services','Adaptive Behavior Services',
  'Non-Face-to-Face Evaluation and Management Services','Hospital Inpatient Services',
  'Special Evaluation and Management Services','Newborn Care Services',
  'Delivery/Birthing Room Attendance and Resuscitation Services',
  'Inpatient Neonatal Intensive Care Services and Pediatric and Neonatal Critical Care Services',
  'Cognitive Assessment and Care Plan Services',
  'General Behavioral Health Integration Care Management')
  
proc=function(R0,UNNEEDED_SECTIONS) {
  R0=delete_overlap(UNNEEDED_SECTIONS)
  Matched_codes=fuzzy_merge(DataCodes,R0)
  Dups=Matched_codes[thedups(Matched_codes,keys='code')]
  Miss = DataCodes %>% anti_join(Matched_codes,by='code')
  return(list(Duplicates=Dups,MissingCodes=Miss))
}



  

con <- dbConnect(odbc(), 
                 Driver = "SQL Server", 
                 Server = "172.16.0.3",
                 Database = "ModelDevelopment",
                 Trusted_Connection = "True" ,
                 timeout = 5000
)
## hi there

qry="
select distinct deid_pat_id as pat_id
,clm_type
,deid_clm_id 
,rend_src_id
,prov_src_id
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
,rend_tin
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
,rend_taxonomy_cd_desc
,(case when rend_taxonomy_cd_desc is null then 0 else 1 end) as has_tax_cd
,rend_npi
,allowed_charge_amt_src
,refer_npi
,refer_cms_spec_cd
,refer_cms_spec_cd_desc
,proc_cd
,proc_cd_desc
,prov_tin
,srv_start_dt
,srv_end_dt
,case when (proc_cd_desc like '%Fee%') then 1 else 0 end as isFee
,prov_taxonomy_cd_classification
from dbo.claims_20210601_to_20220531
where clm_coverage_type='MED' and clm_status='P' and clm_type='HC'
and rend_npi is not null 
"




qry2="select * from [ReferenceData].[dbo].[ICD]"


## now it is safe to merge procedure code to section codes
##

## download claims

DP=data.table(dbGetQuery(con,qry)) %>% data.table
ICD=data.table(dbGetQuery(con,qry2))
setkey(DP,pat_id) 
ICD=ICD[,.(ICD10=code,short_desc)]
setkey(ICD,ICD10)
## exclusions
## procedure code starts with '$' 
#  num       claims allowed
#1	886      124135.78
DP=DP[str_sub(proc_cd,1,1)!='$']
# have to make this a one to one match
# try to find a way to get to one per
DP=DP[!is.na(proc_cd)]



## ultimately we will want different ways to group the procedure
## codes so if we have a function that maps code to group, that
## should be the input to our remaining code


## procedure code grouping
## bring this in and massage the tables nicely
##

qry.pcg='with x as (SELECT  distinct
      [section]
      ,[range_start]
      ,[range_end]
  FROM [ReferenceData].[dbo].[proc_cd_section])
  select a.code as proc_cd,x.range_start,x.range_end,x.section,
  substring(coalesce(a.short_desc,a.long_description),1,30) as description
  from referencedata.dbo.hcpcs a,
       x
  where a.code between range_start and range_end
  order by code,range_start'

range0.qry='SELECT  distinct
                [section]
                ,[range_start]
                ,[range_end]
FROM [ReferenceData].[dbo].[proc_cd_section]'

range1.qry='select * from referencedata.dbo.hcpcs a'

grp_proc_cd = function(code) {
  ifelse(str_detect(code,'....[A-Z]'),str_sub(code,5,5),str_sub(code,1,1))
}

# map procedure codes to ranges
# get range data, including fixes

R0=dbGetQuery(con,range0.qry) %>% data.table
NR = fread('range_fix.csv',colClasses=rep('character',3))
R0=rbind(NR,R0,fill=TRUE)


R0[,`:=`(g1=grp_proc_cd(range_start),g2=grp_proc_cd(range_end))]
# fix some overlapping ranges by moving one goal post or the other

R0[range_start=='80143',range_end:='80304']  # drug assays
R0[range_start=='80305',range_end:='80399']
R0[range_start=='81105',`:=`(range_end='81364',section='Molecular Pathology Tier I')]
R0[range_end=='81479',range_start:='81472']
R0[.('90460'),range_end:='90475']

## - deletion of overlapping intervals
`%notin%` = Negate(`%in%`)
delete_overlap = function(sec,DT=R0) {
  DT=DT[section %notin% sec]
  return(DT)
}
R0=delete_overlap(UNNEEDED_SECTIONS)

R0=R0[section=='Diagnostic/Screening Processes or Results',`:=`(range_start='3006F',range_end='33015')] # cutoff
SEARCH_DIGITS=1
R0[,k1:=str_sub(range_start,1,SEARCH_DIGITS)]
R0[,k2:=str_sub(range_end,1,SEARCH_DIGITS)]
setkey(R0,g1,k1)
# use a three digit search key
addkeys=function(C,digits=SEARCH_DIGITS) {
  C[,`:=`(g1=grp_proc_cd(code),k1=str_sub(code,1,digits))]
  setkey(C,g1,k1)
  return(C)
}

########################################################
# convert procedure code to ranges
########################################################

# codes that exist in the data
DataCodes=DP %>% group_by(proc_cd) %>% summarize(n=n()) %>% rename(code=proc_cd) %>% data.table %>% addkeys(digits=1)
# make sure ranges (R0) above are set to the same number of digits for their search key!!
setkey(DataCodes,g1,k1)

Matched_codes=fuzzy_merge(DataCodes,R0)
MissingCodes = DataCodes %>% anti_join(Matched_codes,by='code')

Duplicates=Matched_codes[thedups(Matched_codes,keys='code')]

# Codes in data that are not on our reference table
thedups=function(DT,keys) { 
  setkeyv(DT,keys)
  A=DT[,.N,by=keys][N>1];
  setkeyv(A,keys)
  return(A)
}

dkey=function(DT,keys) {
  setkeyv(DT,keys)
  new=copy(DT[,dupkeyctr:=1:.N,by=keys])
  setkeyv(new,keys)
  return(new)
}

## remove things that are now out of range
in_range = function(x,range_start,range_end){
  x>=range_start & x<=range_end
}


## mapping from procedure codes to procedure groups
####################################################
#  > map1('V5364')
#  [1] "prcg_444"

#  map1(oneOnly=FALSE)
# shows entire table underlying the map. 

# tables are producing NA which is not OK
# fix this, then continue to run

# map1('99213')
# [1] "prcg_323"
# map1('99213',oneOnly = FALSE)
## > section proc_cd n proc_grp
## 1: Office visit for established p   99213 5 prcg_323

####################################################

## Proc codes successfully resolved
## Proc codes with duplicates no successful resolution

grouped_counts <- function(.data,group_col) {
  .data[,.N,by=eval(as_name(enquo(group_col)))]
}

sample_claims <- function(DP,prop) {
  grouped_counts(DP,deid_clm_id) %>% slice_sample(prop=.01) %>% left_join(DP,by='deid_clm_id')
}

# going to need to break procedure code down into groups

# hardcodes of difficult to place procedures




# mismatch of length is frustrating
DPS = left_join(DP,L_PC,by='proc_cd')

D## all sections should have a minimum number of claims
dim(DPS)

## PC only table


MDPC = DPS[,.(rend_npi,proc_cd,proc_cd_desc,proc_grp=w,rend_taxonomy_cd_desc)]
MDPC[,dummy:=1]
MDPC[,whichsample:=sample(1:10,.N,replace=TRUE)]
lbl=function(DT) merge(DT,SN[,.(proc_grp,section)],by='proc_grp')


wide_ds=function(MDPC,threshold=500) {
  USE=MDPC[,sum(dummy),by=proc_grp][V1>=threshold] %>% lbl
  ASET=MDPC %>% inner_join(USE,by='proc_grp')
  CAST1=ASET%>% dcast.data.table(rend_npi+rend_taxonomy_cd_desc~proc_grp,fill=0,value.var='dummy',fun.aggregate = sum)
  return(CAST1)
}

###########################################################################
## do the PC analysis 
###########################################################################


analysis_set=wide_ds(MDPC[whichsample==1]) # USE 10% OF DATA ONLY
numeric_columns=function(DT) {
  DT[,.SD,.SDcols=is.numeric]
}

f = function(i,...) {
  MDPC[whichsample==i] %>% wide_ds(threshold=500) %>% numeric_columns %>% prcomp(scale.=TRUE,...)
}

models=lapply(1:3,f,rank=40)

score_data = function(DT) {
   scores= DT %>% select(contains('prcg')) %>% predict(models[[1]],newdata=.)
   output=cbind(data,scores)
   return(output)
}







