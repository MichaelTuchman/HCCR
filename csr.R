## csr.R
##
## Cost-sharing reduction (CSR) adjustment, new in the CY2025 workbook.
##
## Table 13 lists each CSR plan variation with the person-level
## CSR_INDICATOR (1-11) the CMS software expects and the CSR RA factor. Tables
## 6-8 then say: for an enrollee with indicator k, multiply the score of one
## metal tier by that indicator's factor (indicator 2 -> Gold x 1.07, 4 ->
## Bronze x 1.51, ...) and leave the other four tiers x 1.00.
##
## HCCR reads the indicator -> (tier, factor) mapping from Table 13 rather
## than copying the numbers from Tables 6-8, so a new year's factors flow
## through. The two sources agree for CY2025 (checked by hand when this was
## written; tests/run_csr.R re-checks the values it depends on).

## Read Table 13. Returns one row per HIOS variant / CSR level:
##   hios_variant, csr_level, csr_indicator, csr_factor, tier
read_csr_table = function(path, sheet) {
  raw = readxl::read_excel(path, sheet = sheet, skip = 2, col_types = 'text')
  names(raw) = c('hios_variant', 'csr_level', 'csr_indicator', 'csr_factor')
  X = as.data.table(raw)[!is.na(csr_indicator)]
  X[, `:=`(hios_variant = as.integer(hios_variant),
           csr_indicator = as.integer(csr_indicator),
           csr_factor = as.numeric(csr_factor),
           csr_level = str_squish(str_replace_all(csr_level, ' ', ' ')))]
  tiers = c('Catastrophic', 'Bronze', 'Silver', 'Gold', 'Platinum')
  X[, tier := vapply(csr_level, function(s) {
        hit = tiers[str_detect(s, tiers)]
        if (length(hit) == 1) hit else NA_character_ }, '')]
  X
}

## One row per indicator: which metal tier it adjusts and by what factor.
## Indicator 1 means "no adjustment" (every tier x 1.00), so it has no tier.
## Stops if Table 13 gives one indicator two different factors or tiers, since
## then the indicator alone would not determine the adjustment.
csr_by_indicator = function(csr_table) {
  by_ind = csr_table[, .(factor = unique(csr_factor), tier = unique(tier[csr_indicator != 1L])),
                     by = csr_indicator]
  bad = by_ind[, .N, by = csr_indicator][N > 1]
  if (nrow(bad)) stop('Table 13 maps CSR indicator(s) ', paste(bad$csr_indicator, collapse = ', '),
                      ' to more than one tier or factor; cannot apply the adjustment.')
  by_ind[csr_indicator == 1L, tier := NA_character_]
  if (any(is.na(by_ind[csr_indicator != 1L, tier])))
    stop('Could not read a metal tier from Table 13 for some CSR indicator.')
  by_ind[]
}

## Adds CSR_INDICATOR and CSR_ADJUSTED_<tier> columns to the score table.
## `patients` needs pat_id and, optionally, csr_indicator (default 1 = none).
apply_csr = function(Answer, patients, by_indicator, tiers = Metals) {
  P = unique(as.data.table(patients)[, c('pat_id', intersect('csr_indicator', names(patients))), with = FALSE])
  if (!'csr_indicator' %in% names(P)) P[, csr_indicator := 1L]
  P[is.na(csr_indicator), csr_indicator := 1L]
  unknown = setdiff(unique(P$csr_indicator), by_indicator$csr_indicator)
  if (length(unknown)) stop('csr_indicator value(s) not in Table 13: ', paste(unknown, collapse = ', '))
  A = merge(as.data.table(Answer), P, by = 'pat_id', all.x = TRUE, sort = FALSE)
  A[is.na(csr_indicator), csr_indicator := 1L]
  A = merge(A, by_indicator, by = 'csr_indicator', all.x = TRUE, sort = FALSE)
  for (t in tiers)
    A[, (paste0('CSR_ADJUSTED_', t)) := get(t) * fifelse(!is.na(tier) & tier == t, factor, 1)]
  setnames(A, 'csr_indicator', 'CSR_INDICATOR')
  A[, c('tier', 'factor') := NULL]
  setcolorder(A, c(setdiff(names(A), 'CSR_INDICATOR'), 'CSR_INDICATOR'))
  setcolorder(A, c(setdiff(names(A), grep('^CSR_ADJUSTED_', names(A), value = TRUE)), grep('^CSR_ADJUSTED_', names(A), value = TRUE)))
  A[]
}
