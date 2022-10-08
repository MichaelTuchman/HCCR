# get diagnosis sample working
D2=readRDS('D2.RData')
compact_diags=function(diags){
  diags %>% pivot_longer(cols=starts_with('diag'),names_to='drop',values_to='ICD10') %>% 
  filter(!is.na(ICD10)) %>% select(-drop) %>% distinct
}

D3=compact_diags(D2) %>% data.table




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

# iterate over the rows of J and execute 28 or so data table statements
# on the main demographic table



