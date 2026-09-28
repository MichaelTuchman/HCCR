# interactions.R

# make some changes to the formulas
# and -> & or -> | do-> {  end-> }
# ;} -> }

## really what I'm doing here is writing a type of compiler so the
## the grammar really should be specified directly for both the
## source and target language

## Keep the if/then rules from Tables 6-8. Left out:
##  ED_*     enrollment duration, handled in AgeSexModel
##  RXC_01.. the drug flags themselves, set from claims by assign_rxc
##  SCORE_*, CSR_ADJUSTED_SCORE_*  how to total the score; done in
##           score_model.R (the cost-sharing adjustment is not applied)
## (this replaces a hard-coded list of spreadsheet row numbers)

AllAgesImportantOnly=data.table(AllAges)[str_detect(str_squish(Formula),'^if ') &
                                         !str_detect(Formula,'^if any of the') &
                                         !(Variable %like% '^ED_') &
                                         !(Variable %like% '^(CSR_ADJUSTED_)?SCORE_')]

# whole words only, so variable names are never altered
AllAgesImportantOnly[,Formula:=str_replace_all(Formula,'\\band\\b','&')]
AllAgesImportantOnly[,Formula:=str_replace_all(Formula,'\\bor\\b','|')]

# split if then statements into antecedents and consequences
AllAgesImportantOnly[,c('antecedent','consequent'):=tstrsplit(Formula,'then',2)]

#
AllAgesImportantOnly[,antecedent:=str_squish(str_replace(antecedent,'if',''))]

# use R style condition testing (and btw we need to be using functions for these ideas)
AllAgesImportantOnly[,antecedent:=str_replace_all(antecedent,'(?<![<>!=])=(?!=)','==')]

##  clean up consequent

AllAgesImportantOnly[,consequent:=str_replace(consequent,'do *;','')]
AllAgesImportantOnly[,consequent:=str_replace(consequent,'end *;*','')]

## trailing semis
AllAgesImportantOnly[,consequent:=str_replace(consequent,'; *$','')]

## convert chained assignments to data table format
AllAgesImportantOnly[,consequent:=str_replace_all(consequent,';',',')]

## A group rule such as
##   if HHS_HCC019 = 1 then do; HHS_HCC019 = 0; G01 = 1; end;
## zeroes its member HCCs. The RXC x HCC interactions listed later
## (e.g. RXC_06_X_HCC018_019_020_021, insulin with diabetes) test those
## same HCCs, so zeroing them straight away would switch the interactions
## off. CMS builds the interactions from the HCCs as they stood after the
## hierarchies, so the zeroing is moved to the end of each model's rules.

AllAgesImportantOnly[,rule_order:=.I]
parts=AllAgesImportantOnly[,.(part=str_squish(unlist(str_split(consequent,',')))),
                           by=.(Model,Variable,rule_order,antecedent)][part!='']
parts[,deferred:=str_detect(part,'^HHS_HCC\\w+ *= *0$')]
AllAgesImportantOnly=parts[,.(consequent=paste(part,collapse=', ')),
                           by=.(Model,Variable,antecedent,deferred,rule_order)][order(Model,deferred,rule_order)]

AllAgesImportantOnly[,assignment:=str_squish(paste('X[',antecedent,
                                        ',`:=`(',
                                             consequent,')]',
                                            sep=''))]

## One function per model. The Adult, Child and Infant tables group HCCs
## differently (a group rule sets its member HCCs to 0), so each patient
## must only get the rules for their own model.

model_rules=lapply(split(AllAgesImportantOnly,by='Model'),setterhl)

more_vars=function(X) {
  rbindlist(lapply(split(X,by='Model'),
                   function(x) model_rules[[x$Model[1]]](copy(x))),
            fill=TRUE)
}
