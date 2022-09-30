settozero=read_excel(fn,skip=3,
             sheet = 'Table 4',col_names = c('Obs','HCC','SetZero','label')) %>%
  select(-Obs)
             
settozero %>% separate(SetZero,sep=',',into=paste('X',1:8,sep='')) %>%
     filter(!is.na(X8)) %>% mutate(across(.fns=~(str_trim(.x)))) %>%
     pivot_longer(starts_with('X'))

settozero %>% filter(HCC=='207')

# apply set_to_zero conditions but as a function
