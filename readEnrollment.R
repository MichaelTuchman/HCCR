##
## client data.R
##

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
# get all eligibility records who have some overlap with report date

qry= function(period_start,period_end) {
  paste("select deid_mbr_id,min(cov_start_dt),max(cov_end_dt) from [dbo].[eligibility]
        where deid_mbr_id is not null group by deid_mbr_id",sep='')
}

## need code to download AGe/Sex data, but for now use the saved r object


E=dbGetQuery(con,qry("2021-07-01", "2022-06-30"))  %>% 
     data.table %>% setkey(deid_mbr_id)

Diags=dbGetQuery(con,"/****** Script for SelectTopNRows command from SSMS  ******/
SELECT [deid_pat_id] as pat_id
      ,[pat_birth_dt]
      ,[diag_cd_1]
      ,[diag_cd_2]
      ,[diag_cd_3]
      ,[diag_cd_4]
      ,[diag_cd_5]
        FROM [ModelDevelopment].[dbo].[claims_20210601_to_20220531]") %>% data.table

measures=copy(names(Diags)) %>% setdiff(c('pat_birth_dt','pat_id'))

D3=Diags %>% melt.data.table('pat_id',measures,na.rm = TRUE,value.name = 'ICD10') %>% distinct


## age/sex data
DM = dbGetQuery(con,"  select distinct [deid_mbr_id] as pat_id,
                  [mbr_gender] as pat_gender,
				  [mbr_birth_dt] as pat_birth_dt,
				  DATEDIFF(YEAR,mbr_birth_dt,'2022-05-31') as age_rpt
		from [dbo].[eligibility]") %>% data.table
             
## only eligibility in current term
PERIOD_START_DT='2021-06-01'
ELIG0=E[V1<=PERIOD_START_DT,.(pat_id=deid_mbr_id)]
DM2=merge(ELIG0,DM,by='pat_id')

D3=merge(D3,DM2,by='pat_id')
