## tests/run_csr.R -- cost-sharing adjustment (Table 13), CY2025 only.
##
## Checks the Table 13 reader against the multipliers printed in Tables 6-8
## (indicator -> tier, factor), and that the adjusted scores on a generated
## population equal the unadjusted score times the right factor.
## Needs cy2025-diy-tables-03-30-2026.xlsx in the repo root.
## From the repo root:  Rscript tests/run_csr.R

if (!file.exists('cy2025-diy-tables-03-30-2026.xlsx'))
  stop('cy2025-diy-tables-03-30-2026.xlsx not found in the repo root; see tests/config-cy2025.yaml.')
options(hccr.config_path = 'tests/config-cy2025.yaml')
N = 3000; SEED = 11
source('tests/run_broad.R')

check = function(ok, what) {
  cat(sprintf('%s  %s\n', if (isTRUE(ok)) 'PASS' else 'FAIL', what))
  if (!isTRUE(ok)) quit(status = 1)
}

## What Tables 6-8 say, typed out from the workbook (Table 6 rows 154-163).
expected = data.table(csr_indicator = 2:11,
  tier   = c('Gold','Silver','Bronze','Bronze','Platinum','Platinum','Gold','Gold','Silver','Silver'),
  factor = c(1.07, 1.12, 1.51, 1.19, 1.31, 1.04, 1.39, 1.10, 1.46, 1.15))
got = CSRByIndicator[csr_indicator != 1L][order(csr_indicator)]
check(isTRUE(all.equal(got$tier, expected$tier)) && isTRUE(all.equal(got$factor, expected$factor)),
      'Table 13 indicator -> tier/factor matches Tables 6-8')
check(CSRByIndicator[csr_indicator == 1L, factor] == 1, 'indicator 1 is unadjusted')

A = as.data.table(Answer)
chk = merge(A, expected, by.x = 'CSR_INDICATOR', by.y = 'csr_indicator', all.x = TRUE)
ok = TRUE
for (t in Metals) {
  f = chk[, fifelse(!is.na(tier) & tier == t, factor, 1)]
  ok = ok && isTRUE(all.equal(chk[[paste0('CSR_ADJUSTED_', t)]], chk[[t]] * f))
}
check(ok, 'every CSR_ADJUSTED_<tier> = score x the factor for that indicator')
check(all(A[CSR_INDICATOR == 1L, Silver == CSR_ADJUSTED_Silver]), 'indicator 1 patients are unchanged')
check(A[CSR_INDICATOR > 1L, .N] > 0, 'the population includes CSR enrollees')
cat('CSR checks passed\n')
