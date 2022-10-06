J = AgeSexBands %>% separate(Variable,sep='_',into=c('c1','c2','c3','c4'),remove=FALSE) %>% 
       mutate(sex=str_sub(c1,1,1)) %>% rename(Label=Variable) %>% select(c(1:7,10)) %>%
      select(-Description) %>% data.table 

J[,`:=`(sex=ifelse(sex %in% c('A','S'),NA,sex))]


J[,operator:='IS BETWEEN']
J[c4=='GT',`:=`(c4=Inf,operator='>=')]

J[grep('AGE.',c1),`:=`(lo=as.numeric(str_sub(c1,4,4)),operator='EQ')]

J[Model=='Infant',`:=`(c3=as.numeric(str_sub(c1,4,4)),hi=NA,sex=str_sub(c2,1,1))]  

# need list of variable names here

J[operator %in% c('>=','EQ'),`:=`(sql1=paste('case when pat_age ',
                                            operator,' ',c3,' and ',
                                            "pat_gender='",
                                            str_trim(sex),
                                            "' then 1 else 0 end as "
                                            ,Label,sep=''),
                                  
                                  sql2=paste('when pat_age ',
                                            operator,' ',c3,' and ',
                                            "pat_gender='",
                                            str_trim(sex),
                                            "' then '",Label,"' \n",sep=''))]
  

J[operator=='IS BETWEEN',`:=`(sql1=paste('case when pat_age is between ',
                                        c3,' and ',c4,
                                        " and pat_gender='",
                                        sex,
                                        "' J$then 1 else 0 end as ",
                                        Label,sep=''),
                              
                              sql2=paste('when pat_age is between ',
                                         c3,' and ',c4,
                                         " and pat_sex='",
                                         sex,
                                         "' then '",Label,"'\n",
                                         sep=''))]


J=J[Model!='Adult, Child, Infant']


alternatives=J$sql2 %>% paste(collapse=',')

# set to zero function
# have to set key of patient diags and set_to_zero to HCC

# here' show this code works;
# create pairs of hcc by patient merge(pt_diags_pt_diags)
# merge with the set to zero requirement table - these are the ones we need deleted
# Relabel the set_to_zero has HCC so we can merge back to original table
# when we merge back, delete the ones that match, keeping the ones that don't [is.na(set_zero)]
#
# this is the following correlated query in sql
# delete from pt_diags 
# where hcc in (select set_zero has hcc from pt_diags join set_to_zero_table on pt_diags.hcc=set_to_zero_table.hcc))

remove_subordinate_hcc = function(pt_diags,set_to_zero_tbl) {
   merge(pt_diags,
         merge(merge(pt_diags,pt_diags),set_to_zero_tbl)[,HCC:=set_zero],
         all.x=TRUE)[is.na(set_zero)]
}  

# as much as humanly possible, I want to avoid straight assignment code
# since I can do this as a merge, I will do so.

