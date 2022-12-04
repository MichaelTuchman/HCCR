leaking=function(PCS1,PCS2) {
  X1=PCS1[,.(N1=.N),by='deid_clm_id']
  X2=PCS2[,.(N2=.N),by='deid_clm_id']
  Comp=merge(X1,X2,by='deid_clm_id') %>% filter(N2>N1)
  setkey(Comp,deid_clm_id)
  return(Comp)
  
}

fuzzy_merge = function(Data,Ranges) {
  STAGE0 = left_join(Data,Ranges,by=key(Data))
  STAGE1 = STAGE0 %>% filter(code>=range_start)
  STAGE2 = STAGE1 %>% filter(code<=range_end)
  return(STAGE2)
}
debug(fuzzy_merge)
fuzzy_merge(DataCodes,R0)
