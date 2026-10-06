## table3_reader.R
##
## Reads Table 3 (the ICD-10 -> HHS condition category crosswalk) by
## finding each column from its header text, not from its position.
##
## Why: CMS has changed this table's layout between benefit years.
## CY2022 has 13 columns: one "Code Valid in FY" flag for each of two
## fiscal years and one MCE age/sex condition pair. CY2025 has 17: two
## validity flags (FY2025, FY2026) and three MCE age/sex pairs (FY2025,
## FY2026, and a combined "CY2025 (FY25/FY26)" pair). Matching headers
## means a layout that only adds or reorders columns still reads
## correctly, and one that drops a needed column stops with a message
## naming it.
##
## Output has one row per ICD-10 code with these columns:
##   obs, ICD10, icd10.label, valid.current, age.cond, sex.cond,
##   age.split, sex.split, cc.1, cc.2, cc.3, comment
## valid.current is "Y" when the code is valid in a fiscal year that
## overlaps the benefit year, else "N".

require(readxl)
require(stringr)

## Header text with line breaks and repeated spaces collapsed.

clean_headers = function(x) str_squish(str_replace_all(x, '[\r\n]+', ' '))

## Position of the one column whose header matches `pattern`;
## stops with a readable message if there are none or several.

find_column = function(headers, pattern, what) {
  hit = which(str_detect(headers, regex(pattern, ignore_case = TRUE)))
  if (length(hit) != 1) {
    stop(sprintf(
      "Table 3: expected exactly one column for %s (header matching /%s/) but found %d. Headers found: %s",
      what, pattern, length(hit), paste(sprintf("'%s'", headers), collapse = ', ')),
      call. = FALSE)
  }
  hit
}

## Choose the MCE age or sex condition column for a benefit year.
## `kind` is "Age" or "Sex". The workbook may offer a column for the
## whole benefit year ("CY2025 ... MCE Age Condition"), one per fiscal
## year ("FY2025 ... MCE Age Condition"), or both. Prefer the
## benefit-year column, else the fiscal year that matches the benefit
## year, else stop.

find_mce_column = function(headers, kind, model_year) {
  is_mce = str_detect(headers, regex(sprintf('MCE %s Condition', kind), ignore_case = TRUE))
  for (tag in c(sprintf('CY\\s*%d', model_year), sprintf('FY\\s*%d', model_year))) {
    hit = which(is_mce & str_detect(headers, regex(paste0('^', tag, '\\b'), ignore_case = TRUE)))
    if (length(hit) == 1) return(hit)
  }
  stop(sprintf(
    "Table 3: no MCE %s Condition column for benefit year %d (looked for a header starting 'CY%d' or 'FY%d'). Headers found: %s",
    kind, model_year, model_year, model_year,
    paste(sprintf("'%s'", headers[is_mce]), collapse = ', ')),
    call. = FALSE)
}

## Fiscal years run Oct 1 - Sep 30, so benefit year Y draws on codes valid
## in FY Y (Jan-Sep) and FY Y+1 (Oct-Dec). A workbook flags only the
## fiscal years it covers; use those that overlap the benefit year.

find_validity_columns = function(headers, model_year) {
  is_valid = str_detect(headers, regex('^Code Valid in FY', ignore_case = TRUE))
  fy = suppressWarnings(as.integer(str_match(headers, regex('^Code Valid in FY\\s*(\\d{4})', ignore_case = TRUE))[, 2]))
  hit = which(is_valid & fy %in% c(model_year, model_year + 1L))
  if (length(hit) == 0) {
    stop(sprintf(
      "Table 3: no 'Code Valid in FY...' column overlaps benefit year %d (needs FY%d or FY%d). Validity columns found: %s",
      model_year, model_year, model_year + 1L,
      paste(sprintf("'%s'", headers[is_valid]), collapse = ', ')),
      call. = FALSE)
  }
  hit
}

## Category numbers such as 161.2 are stored as numbers in the workbook, and
## reading them as text exposes the binary float ("161.19999999999999").
## Round-trip through numeric to get back the number as written.

as_category_text = function(x) {
  n = suppressWarnings(as.numeric(x))
  ifelse(is.na(n), x, as.character(n))
}

## first_data_row: the 1-based worksheet row where the first ICD-10 code
## sits (config.yaml's header_skip value); the header row is the row above.

read_icd10_crosswalk = function(path, sheet, first_data_row, model_year) {
  raw = read_excel(path, sheet = sheet, skip = first_data_row - 2,
                   col_names = TRUE, col_types = 'text', .name_repair = 'minimal')
  headers = clean_headers(names(raw))

  col = function(pattern, what) raw[[find_column(headers, pattern, what)]]

  valid_cols = find_validity_columns(headers, model_year)
  in_year = Reduce(`|`, lapply(valid_cols, function(i) raw[[i]] %in% 'Y'))

  out = data.frame(
    obs           = col('^Obs$', 'observation number'),
    ICD10         = col('^ICD-?10$', 'ICD-10 code'),
    icd10.label   = col('^ICD-?10 Label$', 'ICD-10 label'),
    valid.current = ifelse(in_year, 'Y', 'N'),
    age.cond      = raw[[find_mce_column(headers, 'Age', model_year)]],
    sex.cond      = raw[[find_mce_column(headers, 'Sex', model_year)]],
    age.split     = col('^CC Age Split', 'CC age split'),
    sex.split     = col('^CC Sex Split', 'CC sex split'),
    cc.1          = as_category_text(col('^CC$', 'first CC')),
    cc.2          = as_category_text(col('^Second CC$', 'second CC')),
    cc.3          = as_category_text(col('^Third CC$', 'third CC')),
    comment       = col('^Footnote', 'footnote'),
    stringsAsFactors = FALSE)

  ## Rows below the table (notes, blank lines) have no ICD-10 code.
  out[!is.na(out$ICD10), ]
}
