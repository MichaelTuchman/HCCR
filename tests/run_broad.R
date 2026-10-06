## tests/run_broad.R
## Scores a larger generated population (tests/generate_synthetic.R) and
## checks the pipeline's diagnosis and drug categories against an
## independently computed expectation.
##
## From the repo root:
##   Rscript tests/run_broad.R            # 2000 patients, seed 1
##   Rscript tests/run_broad.R 20000 7    # patients, seed
## For another model year point config.yaml (or options(hccr.config_path=))
## at that year's workbook first, as tests/run_cy2025.R does.

source('Model_Inputs.R')
source('AgeSexfactors.R')
source('interactions.R')
source('apply_hcc.R')
source('tests/generate_synthetic.R')

## (read after the sources above: Model_Inputs.R clears the workspace)
args = commandArgs(trailingOnly = TRUE)
N    = if (length(args) >= 1) as.integer(args[1]) else 2000L
SEED = if (length(args) >= 2) as.integer(args[2]) else 1L

pop = generate_synthetic(N, SEED)
DM2 = pop$DM2; D3 = pop$D3; HCPCS = pop$HCPCS; NDC = pop$NDC
source('score_model.R')

check = function(ok, what) {
  cat(sprintf('%s  %s\n', if (isTRUE(ok)) 'PASS' else 'FAIL', what))
  if (!isTRUE(ok)) quit(status = 1)
}

cat(sprintf('\n%d patients, seed %d, model year %d\n', N, SEED, hccr_model_year()))

check(identical(generate_synthetic(200, SEED)$D3, generate_synthetic(200, SEED)$D3),
      'same seed gives the same population')

got = unique(STEP2[, .(pat_id, cat = HCC)])
exp = unique(pop$expected)
check(nrow(fsetdiff(exp, got)) == 0, 'every expected category was assigned')
check(nrow(fsetdiff(got, exp)) == 0, 'no unexpected categories were assigned')

## Known gap (README, "Known gaps"): the infant age/sex variables in the
## workbook are AGE0_MALE and AGE1_MALE only, and the pipeline scores a
## patient only if they have an age/sex row, so female infants (age 0-1)
## are dropped with no score and no warning. Anything else missing is new.
unscored = setdiff(DM2$pat_id, Answer$pat_id)
female_infants = DM2[pat_age <= 1 & pat_gender == 'F', pat_id]
check(length(setdiff(unscored, female_infants)) == 0,
      'every patient got a score, except female infants (known gap)')
cat(sprintf('NOTE  %d of %d female infants were not scored (known gap)\n',
            length(unscored), length(female_infants)))
check(!anyNA(Answer$Silver) && all(is.finite(Answer$Silver)), 'all Silver scores are finite')
check(all(Answer$Silver > 0), 'all Silver scores are positive')

cat('\nPopulation\n')
print(DM2[, .(patients = .N, mean_age = round(mean(pat_age), 1), pct_female = round(100 * mean(pat_gender == 'F')),
              pct_partial_year = round(100 * mean(ENROLDURATION < 12))), by = .(model = fcase(pat_age <= 1, 'Infant', pat_age <= 20, 'Child', default = 'Adult'))])
cat(sprintf('\nPatients with a diagnosis: %d (%.0f%%);  with a pharmacy fill: %d (%.0f%%)\n',
            uniqueN(D3$pat_id), 100 * uniqueN(D3$pat_id) / N,
            uniqueN(pop$RX$pat_id), 100 * uniqueN(pop$RX$pat_id) / N))
cat('\nPharmacy fills\n')
RX = pop$RX
check(!any(RX[is.na(RXC), NDC] %in% NDC_CODES$NDC),
      'drugs outside the risk model are not in the NDC crosswalk')
cat(sprintf('%d fills for %d patients (%.0f%% of patients); %.1f%% of fills map to a risk-model drug category\n',
            nrow(RX), uniqueN(RX$pat_id), 100 * uniqueN(RX$pat_id) / N,
            100 * mean(!is.na(RX$RXC))))
print(RX[, .(patients = uniqueN(pat_id), fills = .N),
         by = .(drug_class = ifelse(is.na(RXC), drug_class, 'risk-model drug'))][order(-patients)])
cat('\nDrug categories (patients, after hierarchy)\n')
print(exp[cat %like% '^RXC', .N, by = cat][order(cat)])
cat('\nSilver risk score by model\n')
print(Answer[, .(patients = .N, mean = round(mean(Silver), 2),
                 median = round(median(Silver), 2),
                 p90 = round(quantile(Silver, 0.9), 2),
                 max = round(max(Silver), 2)), by = Model][order(Model)])
cat('\nAll checks passed\n')
