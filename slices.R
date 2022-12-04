## explore.R
## 
## reports and explorations===

slicer = function(M,N,plan='Silver') {
  K=Answer[Answer[[plan]]>=M & Answer[[plan]]<N,.SD,.SDcols=c('pat_id',plan,'Model','pat_age','pat_gender')]
  setkey(K,pat_id)
  return(K)
}



## number of patients

np = function(DT) DT %>% select(pat_id) %>% distinct %>% count()


NONAGE = function(DT) DT[!str_detect(Variable,'[MF]AGE_LAST')][!str_detect(Variable,'AGE[01]_MALE')][!str_detect(Variable,'AGE_LAST')]

find_factors = function(dt)  {
  dt %>% NONAGE %>% filter(value==1) %>% select(pat_id,Model,pat_gender,pat_age,Variable)
}

j=function(M,N) {
K1=slicer(M,N) %>% merge(STEP6[,.(pat_id,Variable,value)],by='pat_id')
K2=K1 %>% find_factors() %>% merge(ModelFactors[Metal=='Silver'],by=c('Model','Variable')) %>%
    arrange(pat_id,desc(Coeff)) %>% data.table %>%
    merge(HCC_HELPER,all.x = TRUE,by=c('Variable'))
K2[!is.na(Description),.N,by=.(Variable,Description)][order(-N)][1:5] %>% write.csv()
}

