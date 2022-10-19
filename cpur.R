# cost per unit risk 

# input 
# COST by person by provider 
# # appropriate time window

# [1] "deid_pat_id"  "rend_npi"     "cov_amt"      "pat_gender"   "pat_birth_dt"
# must use 'deid_pat_id' as the id variable

# risk table mapping containing ID and Risk Score (over what period)
set.seed(425)
FAKE_RISK_TABLE=diags %>% select(deid_pat_id) %>% distinct
FAKE_RISK_TABLE$hhs_risk= rgamma(nrow(FAKE_RISK_TABLE),.25,.25)


cpur = function(cost_table,risk_table,thresh=.20,min_cc=500) {
  
  # pat_id,rend_id,y.npi.pat
    CPatProv=cost_table %>% group_by(deid_pat_id,rend_npi) %>%
       summarize(y.npi.pat=sum(cov_amt))
  
  # cost per patient (id,y .pat)
  CPP=CPatProv %>% group_by(deid_pat_id) %>% summarize(y.pat=sum(y.npi.pat))
  
  # cost per patient with risk : tack on risk column (pat_id,y.pat,)
  CPP.w.risk = CPP %>% merge(risk_table,by='deid_pat_id')
  

  # cost for each patient with provider total and patient total
  # (pat_id,rend_id,y.npi.pat,y.pat,risk)
  
  Y=merge(CPatProv,CPP.w.risk,by='deid_pat_id') %>% mutate(ot=y.npi.pat/y.pat,use=ot>=thresh)
  
  # for large enough populations risk should be ~1 in mean
  
  # TPC = Y %>% group_by(rend_npi) %>% filter(!is.na(rend_npi)) %>% 
  #   summarise(y.npi=sum(y.npi.pat),n.npi=n(),risk.npi=sum(hhs_risk)) %>%
  #   filter(n.npi>=min_cc) %>%
  #   mutate(CPUR=y.npi / risk.npi)
  
  return(Y)
  
}

# debug(cpur)
RES=cpur(d,FAKE_RISK_TABLE) %>% data.table

