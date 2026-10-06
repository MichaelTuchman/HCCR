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

## A count definition such as
##   HCC_CNT = SUM(HHS_HCC*, G*) - HHS_HCC022
## (new in the CY2025 workbook; Tables 6 and 7) is not an if/then rule.
## It is compiled to its own step: sum every column whose name starts with
## one of the listed prefixes, then subtract any named columns (a column
## that is NA for a patient counts as 0). Rules that
## test HCC_CNT (the SEVERE_HCC_COUNT variables) run after it.

count_definition_code=function(model,formula) {
  f=str_squish(str_remove(formula,';\\s*$'))
  args=str_trim(str_split_1(str_match(f,'SUM\\(([^)]*)\\)')[,2],','))
  prefixes=str_remove(args,'\\*$')
  minus=str_trim(str_split_1(str_remove(f,'^[^)]*\\)'),'-'))
  minus=minus[minus!='']
  code=sprintf("X[,HCC_CNT:=rowSums(.SD,na.rm=TRUE),.SDcols=patterns('^(%s)')]",
               paste(prefixes,collapse='|'))
  for (v in minus)
    code=paste0(code,sprintf(";if('%s' %%in%% names(X)) X[,HCC_CNT:=HCC_CNT-fcoalesce(as.numeric(%s),0)]",v,v))
  data.table(Model=model,assignment=code)
}

count_rows=data.table(AllAges)[str_detect(str_squish(Formula),'^HCC_CNT *= *SUM')]
count_defs=rbindlist(c(list(data.table(Model=character(),assignment=character())),
                       Map(count_definition_code,count_rows$Model,count_rows$Formula)))

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
                           by=.(Model,Variable,antecedent,deferred,rule_order)]

## Run order within a model: (0) set every assigned variable to 0 (below),
## (1) ordinary rules, (2) the deferred zeroing,
## (3) HCC_CNT, which counts the HCCs and groups left after the zeroing,
## (4) rules that test HCC_CNT.
AllAgesImportantOnly[,stage:=fcase(str_detect(antecedent,'\\bHCC_CNT\\b'),4,
                                   deferred,2,
                                   default=1)]

AllAgesImportantOnly[,assignment:=str_squish(paste('X[',antecedent,
                                        ',`:=`(',
                                             consequent,')]',
                                            sep=''))]

## Put each model's HCC_CNT step between stages 2 and 4, then order.
count_defs[,`:=`(stage=3,rule_order=0L)]

## Start every variable a model's rules set at 0. The CMS software does the
## same, and some rules test for a 0: the infant rule "if IHCC_SEVERITY5 = 0
## and ... IHCC_SEVERITY2 = 0 then IHCC_SEVERITY1 = 1" has to fire for an
## infant with no diagnoses, but a variable no rule has set yet is NA here,
## not 0, and an NA never equals 0. A variable that already has a value
## (an HCC flag, AGE0_MALE) keeps it; only NA becomes 0.
assigned=AllAgesImportantOnly[,.(var=unique(str_match_all(consequent,'(?:^|[ ,])([A-Za-z]\\w*) *=(?!=)')[[1]][,2])),by=.(Model,rule_order)]
init_defs=assigned[!is.na(var),.(var=list(unique(var))),by=Model][,.(Model,
  stage=0,rule_order=-1L,
  assignment=sprintf("for (v in %s) { if (!(v %%in%% names(X))) X[,(v):=0L] else X[is.na(get(v)),(v):=0L] }",
                     sapply(var,function(v) paste0('c(',paste(sprintf("'%s'",v),collapse=','),')'))))]

AllAgesImportantOnly=rbind(AllAgesImportantOnly,count_defs,init_defs,fill=TRUE)[order(Model,stage,rule_order)]

## One function per model. The Adult, Child and Infant tables group HCCs
## differently (a group rule sets its member HCCs to 0), so each patient
## must only get the rules for their own model.

model_rules=lapply(split(AllAgesImportantOnly,by='Model'),setterhl)

more_vars=function(X) {
  rbindlist(lapply(split(X,by='Model'),
                   function(x) model_rules[[x$Model[1]]](copy(x))),
            fill=TRUE)
}
