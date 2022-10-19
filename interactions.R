# interactions.R

# make some changes to the formulas
# and -> & or -> | do-> {  end-> }
# ;} -> }

## really what I'm doing here is writing a type of compiler so the
## the grammar really should be specified directly for both the
## source and target language


AllAgesImportantOnly=data.table(AllAges)[c(1:66,67:75,135:178,209:364)]

AllAgesImportantOnly[str_detect(Formula,"and"),Formula:=str_replace_all(Formula,'and','&')]
AllAgesImportantOnly[str_detect(Formula,"or"),Formula:=str_replace_all(Formula,'or','|')]

# split if then statements into antecedents and consequences
AllAgesImportantOnly[,c('antecedent','consequent'):=tstrsplit(Formula,'then',2)]

#
AllAgesImportantOnly[,antecedent:=str_squish(str_replace(antecedent,'if',''))]

# use R style condition testing (and btw we need to be using functions for these ideas)
AllAgesImportantOnly[,antecedent:=str_squish(str_replace(antecedent,'if',''))]
AllAgesImportantOnly[,antecedent:=str_replace_all(antecedent,'=','==')]

##  clean up consequent

AllAgesImportantOnly[,consequent:=str_replace(consequent,'do *;','')]
AllAgesImportantOnly[,consequent:=str_replace(consequent,'end *;*','')]

## trailing semis
AllAgesImportantOnly[,consequent:=str_replace(consequent,'; *$','')]

## convert chained assignments to data table format
AllAgesImportantOnly[,consequent:=str_replace_all(consequent,';',',')]

View(AllAgesImportantOnly)

AllAgesImportantOnly[,assignment:=str_squish(paste('X[',antecedent,
                                        ',`:=`(',
                                             consequent,')]',
                                            sep=''))]

# setterhl = function(code) {
#   base = function(X) {}
#   # must condense to a single expression
#   block = paste("{",paste(code$assignment,collapse=';'),";return(X)}")
#   body(base)=parse_expr(block)
#   return(base)
# }

more_vars=setterhl(AllAgesImportantOnly[!(Variable %like% '^ED')])

