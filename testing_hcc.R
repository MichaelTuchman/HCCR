# get diagnosis sample working
D2=readRDS('D2.RData')
compact_diags=function(diags){
  diags %>% pivot_longer(cols=starts_with('diag'),names_to='drop',values_to='ICD10') %>% 
  filter(!is.na(ICD10)) %>% select(-drop) %>% distinct
}

D3=compact_diags(D2) %>% data.table
rm(D2)