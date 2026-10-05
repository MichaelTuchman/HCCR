##
## client data.R
##
## Server address, database/table/column names, and the claims lookback
## window all come from config.yaml's `database:` section now (see
## config.R) instead of being hard-coded here. This is config wiring
## only: the queries themselves, and the SQL Server connection itself,
## are unchanged, and a new client's database still needs someone to
## confirm these table/column names actually match - see the "Client
## data integration" section of the project to-do list.

library(data.table)
library(tidyverse)
library(RODBC)
library(odbc)
library(dplyr)
library(lubridate)
library(rlang)

if (!exists('CONFIG')) source('config.R')
DB = hccr_db_config()

con <- dbConnect(odbc(),
                 Driver = DB$driver,
                 Server = DB$server,
                 Database = DB$database_name,
                 Trusted_Connection = if (isTRUE(DB$trusted_connection)) "True" else "False",
                 timeout = DB$timeout_ms
)
# get all eligibility records who have some overlap with report date

qry= function(period_start,period_end) {
  sprintf("select %s,min(%s),max(%s) from %s
        where %s is not null group by %s",
        DB$columns$eligibility$member_id,
        DB$columns$eligibility$coverage_start,
        DB$columns$eligibility$coverage_end,
        DB$tables$eligibility,
        DB$columns$eligibility$member_id,
        DB$columns$eligibility$member_id)
}

## only eligibility in current term

PERIOD_START_DT = DB$period$start_date
PERIOD_END_DT   = DB$period$end_date

## need code to download AGe/Sex data, but for now use the saved r object


E=dbGetQuery(con,qry(PERIOD_START_DT, PERIOD_END_DT))  %>%
     data.table %>% setkey(deid_mbr_id)

diag_cols = DB$columns$claims$diagnosis_codes
diag_select = paste(sprintf('[%s]',diag_cols),collapse=',\n      ')

Diags=dbGetQuery(con,sprintf("/****** Script for SelectTopNRows command from SSMS  ******/
SELECT [%s] as pat_id
      ,[%s]
      ,%s
        FROM %s",
        DB$columns$claims$patient_id,
        DB$columns$claims$birth_date,
        diag_select,
        DB$tables$claims)) %>% data.table

measures=copy(names(Diags)) %>% setdiff(c('pat_birth_dt','pat_id'))

D3=Diags %>% melt.data.table('pat_id',measures,na.rm = TRUE,value.name = 'ICD10') %>% distinct

####################################################################
## age/sex data an partial year eligibility
####################################################################

DM = dbGetQuery(con,sprintf("  select distinct [%s] as pat_id,
                  [%s] as pat_gender,
				  [%s] as pat_birth_dt,
				  DATEDIFF(YEAR,%s,'%s') as age_rpt
		from %s",
		DB$columns$eligibility$member_id,
		DB$columns$eligibility$gender,
		DB$columns$eligibility$birth_date,
		DB$columns$eligibility$birth_date,
		PERIOD_END_DT,
		DB$tables$eligibility)) %>% data.table

ELIG0=E[V1<=PERIOD_START_DT,.(pat_id=deid_mbr_id,cov_start_dt=V1)]

DM2=merge(ELIG0,DM,by='pat_id')
DM2[,pat_age:=age_rpt]

DM2=bind_rows(DM2, DM[pat_birth_dt>=PERIOD_START_DT,.(pat_id,pat_gender,pat_birth_dt,pat_age=age_rpt)]) # missed in elig screen

DM2[,age_rpt:=NULL]

DM2[is.na(cov_start_dt),cov_start_dt:=pat_birth_dt]

DM3=DM2[,.(pat_id,cov_start_dt,pat_gender,pat_birth_dt,pat_age,ENROLDURATION=(interval(cov_start_dt,PERIOD_END_DT) %/% months(1)))]

DM2=DM3[ENROLDURATION>0]

D3=merge(D3,DM2,by='pat_id')

D3=D3[,.(pat_id,ICD10,pat_gender,pat_age)]

rm(list=c('E','ELIG0','Diags'))


## Infusions and hospital drugs

qry=sprintf("
SELECT DISTINCT
[%s] as pat_id
,[%s] as HCPCS
FROM %s
where %s='HCPCS' AND [%s] IS NOT NULL
",
DB$columns$claims$patient_id,
DB$columns$claims$procedure_code,
DB$tables$claims,
DB$columns$claims$procedure_code_type,
DB$columns$claims$procedure_code)

HCPCS=dbGetQuery(con,qry)
HCPCS=data.table(HCPCS)
setkey(HCPCS,pat_id)
