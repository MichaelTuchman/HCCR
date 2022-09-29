# set to zero constraints

cc.table.icd10 = read_excel(fn,sheet='Table 3', skip=4, 
                   col_names=c('obs','icd10','icd10.label','valid.2021','valid.2022','age.cond','sex.cond','age.split','sex.split','cc.1','cc.2','cc.3','comment'),
                   col_types=rep('text',13))

dash=function(str) str_replace_all(str,'\\.','_')
cc.valid.2022=cc.table.icd10 %>% mutate(across(starts_with('cc'),dash)) %>% 
     pivot_longer(starts_with('cc'),names_to=NULL) %>% 
     filter(!is.na(value)) %>%
     filter(valid.2022=='Y') %>%
     select(-valid.2021)

cc.valid.2022 %>% select(age.split,age.cond,sex.split,sex.cond) %>% distinct

# select only rows where not all are NA







