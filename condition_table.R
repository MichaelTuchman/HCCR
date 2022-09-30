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
cc.valid.2022=cc.valid.2022 %>% select(-obs)
TBL1=cc.valid.2022 %>% select(age.split,age.cond,sex.split,sex.cond) %>% distinct

# find all the logical age requirements to create variables out of

A=TBL1 %>% separate(age.cond,sep='age',into = c('split.1','split.2') )%>% select(starts_with('split'))
B=TBL1 %>% separate(age.split,sep='age',into = c('split.1','split.2') )%>% select(starts_with('split'))

AB=bind_rows(A,B) %>% mutate(y=!is.na(split.1) & !is.na(split.2)) %>% filter(y) %>% mutate(z=sprintf("AGE_COND_%02d",1:nrow(AB)))
AB
# select only rows where not all are NA

alf=function(cond1,cond2) {
  low_age = str_replace_all(cond1,"\\D","") %>% as.integer
  high_age = str_replace_all(cond2,"\\D","") %>% as.integer
  operator.1 = str_replace_all(cond1,"\\d","") %>% str_trim
  operator.2 = str_replace_all(cond2,"\\d","") %>% str_trim
  
}







