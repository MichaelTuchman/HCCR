## HCC Grouping variables
## create compute HCC group variable code (SQL, R)

## extra variable tables
extra_vars=function(SheetNoC) {
  read_excel('CY2022 DIY tables 06.30.2022.xlsx',
             sheet = SheetNoC,
             col_names=c('Model','Variable','Description','Used','Formula'),skip=4) %>%
             filter(!is.na(Formula))
}

# filling in blank cells may make filtering easier down the road
fill_in_blank_rows=function(formula_table,partial_column='Model') {
  
  non_blanks=!is.na(formula_table[[partial_column]])
  
  # for each non-blank line bump a counter this will be the group number
  
  F1 = formula_table %>% mutate(group=cumsum(non_blanks)) 
  
  # get those first lines so that we can propagate their values
  # into each empty row
  
  # number the non-blank rows
  
  GT=formula_table[which(non_blanks),]
  GT$group=1:(nrow(GT))
  
  # formula_table must contain the columns variable, description, model, use, etc.
  
  Result = F1 %>% left_join(GT %>% select(group,Variable,Used,Model,Description),by=c('group'))
  
  # fill in columns with top row only if the current row is blank
  # this needs some work as there is some generalizability here
  # that hasn't been made
  
  print(names(Result))
  
  Result %>% mutate(Model=coalesce(Model.x,Model.y),
           Variable=coalesce(Variable.x,Variable.y),
           Description=coalesce(Description.x,Description.y),
           Used=coalesce(Used.x,Used.y)) %>%
    select(Model,Variable,Description,Used,Formula)
          
  
}

# do the same for child and infant classes

Adult=extra_vars('Table 6')
Child=extra_vars('Table 7')
Infant=extra_vars('Table 8')

All_Ages=rbind(Adult,Child,Infant)
All_Ages = fill_in_blank_rows(All_Ages)

# now that we have a nice table that is easy to view,
# we need to bring in the model specification tip
# and also start distilling the table to make SQL SET 
# statements

# tables describing the model and tables having data
# can create a table with one line per diagnosis and
# hcc interaction

