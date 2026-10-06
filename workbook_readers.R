## workbook_readers.R
##
## Small functions that each read one part of the CMS "DIY tables" workbook
## and return a data.table. Model_Inputs.R calls them in order and keeps the
## results as the globals the scoring code uses. Every function takes the
## workbook path explicitly, so none of them depends on a global.
##
## Table 3 (ICD-10 crosswalk), Table 1 (model membership) and Table 13
## (CSR indicators) have their own files: table3_reader.R, model_membership.R
## and csr.R.

## ---- naming helpers ---------------------------------------------------

## "HCC 8" or "8" -> "HHS_HCC008"; "G01" style groups and "1_2" style
## sub-categories keep their suffix. Used so that every table spells a
## category the same way as the coefficient table does.
ss = function(hcc_code) {
  x = tstrsplit(hcc_code, '_', fill = '')
  x1 = str_pad(str_squish(x[[1]]), 3, 'left', '0')
  x2 = if (length(x) > 1) ifelse(x[[2]] == '', '', paste('_', x[[2]], sep = '')) else ''
  res = paste('HHS_HCC', x1, x2, sep = '')
  res[res == 'HHS_HCCNA'] = NA
  res
}

## Condition categories are written 161.2 in Table 3 and 161_2 in variable names.
dash = function(str) str_replace_all(str, '\\.', '_')

## Drug categories: 1 -> "RXC_01"
rxc_name = function(rxc) sprintf('RXC_%02d', as.integer(rxc))

## ---- Tables 6-8: variable definitions --------------------------------

## One row per rule line: Model, Variable, Description, Used, Formula.
## Only the first line of each variable names the model/variable; the rest
## are blank, so fill_in_blank_rows() copies those fields down.
read_variable_rules = function(path, sheet, skip = 4) {
  read_excel(path, sheet = sheet,
             col_names = c('Model', 'Variable', 'Description', 'Used', 'Formula'), skip = skip) %>%
    filter(!is.na(Formula))
}

fill_in_blank_rows = function(formula_table, partial_column = 'Model') {
  non_blanks = !is.na(formula_table[[partial_column]])
  with_group = formula_table %>% mutate(group = cumsum(non_blanks))   # one group per variable
  first_lines = formula_table[which(non_blanks), ]
  first_lines$group = 1:nrow(first_lines)
  joined = with_group %>%
    left_join(first_lines %>% select(group, Variable, Used, Model, Description), by = 'group')
  joined %>% mutate(Model = coalesce(Model.x, Model.y),
                    Variable = coalesce(Variable.x, Variable.y),
                    Description = coalesce(Description.x, Description.y),
                    Used = coalesce(Used.x, Used.y)) %>%
    select(Model, Variable, Description, Used, Formula)
}

## Adult, Child and Infant rules stacked into one table.
read_all_variable_rules = function(path) {
  one = function(key) read_variable_rules(path, hccr_sheet(key), hccr_skip(key, 4))
  fill_in_blank_rows(rbind(one('adult_variables'), one('child_variables'), one('infant_variables')))
}

## ---- Table 3: diagnosis -> condition category -------------------------

## One row per (ICD-10 code, condition category) valid for the model year,
## with the age/sex conditions from the workbook and an HCC name like
## HHS_HCC008. `HCC` is the one-row-per-code table read_icd10_crosswalk() returns.
expand_diagnosis_categories = function(HCC) {
  X = HCC %>% as_tibble %>%
    pivot_longer(starts_with('cc'), names_to = NULL, values_to = 'CC') %>%
    filter(!is.na(CC)) %>%
    filter(valid.current == 'Y') %>%
    select(-obs) %>% mutate(CC = dash(as.character(CC))) %>% data.table
  X[!is.na(sex.cond),  `:=`(sex.cond  = toupper(str_sub(sex.cond, 1, 1)))]
  X[!is.na(sex.split), `:=`(sex.split = toupper(str_sub(sex.split, 1, 1)))]
  X[, HCC := ss(CC)]
  X[]
}

## ---- Table 4: hierarchies --------------------------------------------

## One row per (HCC, set_zero): having HCC means each set_zero category is
## dropped. str_split rather than a fixed number of columns, so a year that
## lists more than 8 lower-ranked categories on one row is not truncated.
read_hierarchies = function(path, sheet, skip = 3) {
  raw = read_excel(path, skip = skip, sheet = sheet,
                   col_names = c('Obs', 'HCC', 'SetZero', 'label')) %>% data.table
  X = raw[!is.na(SetZero),
          .(set_zero = str_trim(unlist(str_split(SetZero, ',')))),
          by = .(HCC = str_trim(as.character(HCC)))][set_zero != '']
  setkey(X, HCC)
  X[, lapply(.SD, ss)][order(HCC)]
}

## ---- Table 5: age/sex bands ------------------------------------------

read_age_sex_bands = function(path, sheet, skip = 2) {
  read_excel(path, sheet = sheet, skip = skip, col_types = rep('text', 5)) %>%
    filter(!is.na(Model))   # remove blank rows
}

## ---- Table 9: coefficients -------------------------------------------

## One row per (Model, Variable, Metal): Model, Variable, isUsed, Metal, Coeff, Year.
read_model_factors = function(path, sheet, skip = 2, model_year = hccr_model_year()) {
  U = read_excel(path, sheet = sheet, skip = skip, col_types = rep('text', 8)) %>%
    filter(!is.na(Model)) %>%
    pivot_longer(cols = ends_with('Level'), names_to = 'Metal', values_to = 'coeff') %>%
    mutate(coeff = round(as.numeric(coeff), 4)) %>%
    mutate(Metal = str_trim(str_remove_all(Metal, 'Level')), year = as.integer(model_year))
  names(U) = c('Model', 'Variable', 'isUsed', 'Metal', 'Coeff', 'Year')
  ## Table 9 spells some interaction names in lower case (RXC_01_x_HCC001)
  ## while Tables 6-8 use upper case (RXC_01_X_HCC001); match on upper case.
  U$Variable = toupper(U$Variable)
  data.table(U)
}

## Coefficients with one column per metal tier, ready to join to a patient's variables.
widen_model_factors = function(ModelFactors)
  ModelFactors %>% dcast.data.table(Model + Variable + isUsed + Year ~ Metal, value.var = 'Coeff')

## ---- Tables 10a/10b: drug codes -> RXC --------------------------------

## Table 10a maps pharmacy NDC codes, Table 10b medical-claim HCPCS codes,
## to the drug categories RXC_01 ... RXC_10.
rxc_crosswalk = function(path, sheet, code_col, skip = 3) {
  X = read_excel(path, sheet = sheet, skip = skip, col_types = 'text') %>% data.table
  setnames(X, c('RXC', 'RXC_LABEL', 'CODE'))
  X = X[str_detect(RXC, '^\\d+$') & !is.na(CODE)]   # drop the notes under the table
  X[, RXC := rxc_name(RXC)]
  setnames(X, 'CODE', code_col)
  setkeyv(X, code_col)
  X
}

## ---- Table 11: drug category hierarchies -------------------------------

read_rxc_hierarchies = function(path, sheet, skip = 3) {
  X = read_excel(path, sheet = sheet, skip = skip,
                 col_names = c('RXC', 'SetZero', 'label'), col_types = 'text') %>% data.table
  X = X[str_detect(RXC, '^\\d+$') & !is.na(SetZero)]
  X = X[, .(set_zero = str_trim(unlist(str_split(SetZero, ',')))), by = RXC]
  X[, .(HCC = rxc_name(RXC), set_zero = rxc_name(set_zero))]
}

## ---- helpers the scoring code uses ------------------------------------

## Takes a table of assignment statements (a column `assignment`) and returns a
## function that runs them on any data.table that has the needed columns.
setterhl = function(codeDT) {
  base = function(X) {}
  block = paste('{', paste(codeDT$assignment, collapse = ';'), ';return(X)}')   # one expression
  body(base) = parse_expr(block)
  base
}

## Join a patient's variables (long form) to the coefficients, keeping non-zero values.
ScoreModel = function(LongForm, MF) {
  merge(LongForm, MF, by = c('Variable', 'Model'))[value != 0]
}
