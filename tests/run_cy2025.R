## tests/run_cy2025.R
## Runs the synthetic scoring test against the CY2025 workbook and checks
## the features that workbook adds over CY2022: the 17-column Table 3, the
## HCC_CNT count, and the HCC_ED enrollment-duration variables.
##
## Needs cy2025-diy-tables-03-30-2026.xlsx in the repo root (see the header
## of tests/config-cy2025.yaml for where to get it).
## From the repo root:  Rscript tests/run_cy2025.R

if (!file.exists('cy2025-diy-tables-03-30-2026.xlsx'))
  stop('cy2025-diy-tables-03-30-2026.xlsx not found in the repo root; see tests/config-cy2025.yaml for the download link.')

options(hccr.config_path = 'tests/config-cy2025.yaml')
source('tests/run_synthetic.R')

check = function(ok, what) {
  cat(sprintf('%s  %s\n', if (isTRUE(ok)) 'PASS' else 'FAIL', what))
  if (!isTRUE(ok)) quit(status = 1)
}

cnt = STEP4[, .(pat_id, HCC_CNT, HCC_ED6)][order(pat_id)]
## P1 has one HCC (HHS_HCC008); P2's HCC is a group (G01) and counts as one;
## P5 has only a drug category (no HCC); P4 has one HCC and 6 months enrolled.
check(identical(as.numeric(cnt[pat_id == 'P1', HCC_CNT]), 1), 'P1 HCC_CNT = 1')
check(identical(as.numeric(cnt[pat_id == 'P2', HCC_CNT]), 1), 'P2 HCC_CNT = 1 (group counts once)')
check(identical(as.numeric(cnt[pat_id == 'P5', HCC_CNT]), 0), 'P5 HCC_CNT = 0 (drug category only)')
check(isTRUE(cnt[pat_id == 'P4', HCC_ED6] == 1), 'P4 (6 months, one HCC) has HCC_ED6')
check(all(is.na(cnt[pat_id != 'P4', HCC_ED6])), 'no one else has HCC_ED6')
cat('CY2025 checks passed\n')
