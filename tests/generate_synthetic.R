## tests/generate_synthetic.R
##
## Generates a larger made-up population than tests/synthetic_data.R, in the
## same shapes the scoring code reads (DM2, D3, HCPCS), plus a pharmacy
## history (RX, and NDC, its one-row-per-patient-per-drug summary).
##
## Needs Model_Inputs.R already sourced: it samples diagnosis codes from
## Table 3 (HCC2) and drug codes from Tables 10a/10b, so the population is
## valid for whichever model year config.yaml points at.
##
## What is realistic and what is not
##  - Ages, enrollment months, the number of conditions per patient, and the
##    drug-given-diagnosis rules below are plausible shortcuts, not
##    epidemiology. Condition prevalences are random (seeded) heavy-tailed
##    weights; they do not match any real population.
##  - Patients get conditions in clusters: a per-patient "frailty" draw makes
##    some patients have many conditions and most have none or one.
##  - Only diagnosis codes with no age or sex condition or split in Table 3
##    are used, so the expected result is unambiguous. The age/sex logic is
##    exercised by tests/synthetic_data.R instead.
##
## generate_synthetic(n, seed) returns a list:
##   DM2, D3, HCPCS, NDC       the scoring inputs
##   RX                        pharmacy fills: pat_id, NDC, RXC (NA for drugs the
##                             risk model ignores), drug_class, fill_date, days_supply
##   expected                  pat_id x HCC / RXC rows that should survive the
##                             hierarchies, computed here independently of
##                             apply_hcc.R

require(data.table)

## Drug classes a diagnosis tends to bring with it: ICD-10 prefix, drug
## category, chance the patient is on that drug.
DRUG_GIVEN_DIAGNOSIS = data.table(
  prefix = c('E10',   'E11',   'E11',   'B20',   'B182',  'I48',   'N185',  'N186',
             'K50',   'K51',   'G35',   'M05',   'M06',   'L40',   'E84'),
  rxc    = c('RXC_06','RXC_07','RXC_06','RXC_01','RXC_02','RXC_03','RXC_04','RXC_04',
             'RXC_05','RXC_05','RXC_08','RXC_09','RXC_09','RXC_09','RXC_10'),
  prob   = c(0.90,    0.45,    0.20,    0.85,    0.60,    0.50,    0.60,    0.60,
             0.50,    0.50,    0.70,    0.50,    0.50,    0.30,    0.80))

## Fills of one drug across the year. A chronic drug is first filled on a
## random day, then refilled every days_supply days, each refill made with
## probability `adherence` (a missed refill is just skipped, so gaps
## appear). An acute drug (an antibiotic course) is one short fill, now and
## then two.

fill_history = function(pat_id, ndc, rxc, year, drug_class = 'risk_model_rxc',
                        adherence = 0.85, acute = FALSE) {
  year_end = as.Date(sprintf('%d-12-31', year))
  if (acute) {
    first = as.Date(sprintf('%d-01-01', year)) + sample(0:350, 1)
    made = c(first, if (runif(1) < 0.2) first + sample(14:60, 1))
    made = made[made <= year_end]
    return(data.table(pat_id = pat_id, NDC = ndc, RXC = rxc, drug_class = drug_class,
                      fill_date = made, days_supply = sample(c(5L, 7L, 10L), 1)))
  }
  days_supply = sample(c(30L, 90L), 1, prob = c(0.7, 0.3))
  first = as.Date(sprintf('%d-01-01', year)) + sample(0:300, 1)
  due = seq(first, year_end, by = days_supply)
  made = due[runif(length(due)) < adherence | seq_along(due) == 1]
  data.table(pat_id = pat_id, NDC = ndc, RXC = rxc, drug_class = drug_class,
             fill_date = made, days_supply = days_supply)
}

## Everyday drugs that are NOT in the risk model's drug categories, so the
## scorer should ignore them. For each class: the chance an adult aged 50 is
## on it (p50), how that chance changes per decade of age on the log-odds
## scale (slope), the chance for someone under 21 (p_child), whether it is
## taken continuously, and how much each drug is used within the class.
## These are popularity guesses, not epidemiology.

BACKGROUND_DRUGS = data.table(
  drug_class = c('statin', 'blood_pressure', 'thyroid', 'acid_reducer', 'antidepressant',
                 'metformin', 'asthma', 'nerve_pain', 'antibiotic', 'steroid'),
  p50     = c(0.20, 0.25, 0.06, 0.10, 0.10, 0.06, 0.07, 0.03, 0.15, 0.04),
  slope   = c(0.45, 0.50, 0.20, 0.20, 0.00, 0.40, -0.10, 0.20, -0.05, 0.00),
  p_child = c(0.00, 0.00, 0.005, 0.01, 0.01, 0.00, 0.10, 0.00, 0.22, 0.03),
  chronic = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE))

BACKGROUND_DRUG_WEIGHT = c(
  'atorvastatin calcium' = 0.45, 'rosuvastatin calcium' = 0.20, 'simvastatin' = 0.20,
  'pravastatin sodium' = 0.10, 'lovastatin' = 0.05,
  'lisinopril' = 0.30, 'losartan potassium' = 0.20, 'amlodipine besylate' = 0.25,
  'metoprolol tartrate' = 0.10, 'metoprolol succinate' = 0.10, 'hydrochlorothiazide' = 0.05,
  'pantoprazole sodium' = 0.50, 'omeprazole' = 0.50,
  'sertraline hydrochloride' = 0.55, 'escitalopram oxalate' = 0.45,
  'albuterol sulfate' = 0.60, 'montelukast sodium' = 0.40,
  'amoxicillin' = 0.60, 'azithromycin' = 0.40)   # others: the only drug in their class

generate_synthetic = function(n = 2000, seed = 1, year = hccr_model_year()) {
  set.seed(seed)
  pat_id = sprintf('S%05d', seq_len(n))

  ## --- demographics ---------------------------------------------------
  age_group = sample(c('infant', 'child', 'adult'), n, replace = TRUE,
                     prob = c(0.015, 0.20, 0.785))
  pat_age = integer(n)
  pat_age[age_group == 'infant'] = sample(0:1,  sum(age_group == 'infant'), TRUE)
  pat_age[age_group == 'child']  = sample(2:20, sum(age_group == 'child'),  TRUE)
  pat_age[age_group == 'adult']  = pmin(64L, pmax(21L, as.integer(round(rnorm(sum(age_group == 'adult'), 42, 12)))))
  DM2 = data.table(pat_id = pat_id,
                   pat_gender = sample(c('F', 'M'), n, TRUE, prob = c(0.53, 0.47)),
                   pat_age = as.numeric(pat_age),
                   ENROLDURATION = ifelse(runif(n) < 0.8, 12L, sample(1:11, n, TRUE)))
  DM2[, ENROLDURATION := as.numeric(ENROLDURATION)]

  ## --- diagnoses ------------------------------------------------------
  pool = unique(HCC2[is.na(age.cond) & is.na(sex.cond) & is.na(age.split) & is.na(sex.split),
                     .(ICD10, HCC)])
  ## Costly conditions are rarer: weight falls with the condition's Adult
  ## Silver coefficient (Table 9), times a random factor so prevalences are
  ## not a clean function of cost. A condition with no coefficient gets the
  ## median one.
  coef = ModelFactors[Model == 'Adult' & Metal == 'Silver', .(HCC = Variable, coef = Coeff)]
  ## Many HCCs are priced through a group variable (G01, G02B ...) rather
  ## than their own coefficient, so a member HCC's cost is the larger of its
  ## own coefficient and its group's. Group rules read: "if <HCCs> then
  ## G.. = 1" (compiled in interactions.R).
  grp_rules = AllAgesImportantOnly[Model == 'Adult' & str_detect(consequent, '\\bG\\w+ *= *1')]
  grp_members = rbindlist(lapply(seq_len(nrow(grp_rules)), function(i)
    data.table(HCC = str_extract_all(grp_rules$antecedent[i], 'HHS_HCC\\w+')[[1]],
               group = str_match(grp_rules$consequent[i], '\\b(G\\w+) *= *1')[, 2])))
  if (nrow(grp_members) > 0) {
    grp_members = merge(grp_members, coef[, .(group = HCC, group_coef = coef)], by = 'group')
    coef = merge(coef, grp_members[, .(group_coef = max(group_coef)), by = HCC], by = 'HCC', all.x = TRUE)
    coef[!is.na(group_coef), coef := pmax(coef, group_coef)]
  }
  hcc_weight = merge(data.table(HCC = unique(pool$HCC)), coef[, .(HCC, coef)], by = 'HCC', all.x = TRUE)
  median_coef = median(hcc_weight$coef, na.rm = TRUE)
  hcc_weight[is.na(coef), coef := median_coef]
  hcc_weight[, w := rlnorm(.N, 0, 0.6) / (1 + pmax(coef, 0))^2]
  pool = merge(pool, hcc_weight, by = 'HCC')
  ## Common drug-linked conditions get a boost so the drug rules are used
  ## often enough to test. Rare ones (HIV, hepatitis C, MS, cystic fibrosis)
  ## are not boosted: their drugs are costly enough to dominate a population's
  ## scores if they are made common.
  boost_prefix = c('E10', 'E11', 'I48', 'N185', 'N186', 'M05', 'M06', 'L40')
  boosted = pool[, str_detect(ICD10, paste0('^(', paste(boost_prefix, collapse = '|'), ')'))]
  pool[boosted, w := w * 3]                                 # keep drug-linked conditions common enough to test
  pool[, w := w / .N, by = HCC]                             # a condition's codes share its weight

  frailty = rgamma(n, shape = 0.6, scale = 1.4)             # most near 0, a few high
  n_dx = rpois(n, frailty * ifelse(age_group == 'adult', 0.6, 0.25))
  D3 = rbindlist(lapply(which(n_dx > 0), function(i) {
    codes = pool[sample(.N, min(n_dx[i], .N), prob = w), unique(ICD10)]
    data.table(pat_id = pat_id[i], ICD10 = codes)
  }))
  if (nrow(D3) == 0) D3 = data.table(pat_id = character(), ICD10 = character())
  D3 = merge(D3, DM2[, .(pat_id, pat_gender, pat_age)], by = 'pat_id')

  ## --- pharmacy: drugs that follow diagnoses, plus a little noise ------
  want = rbindlist(lapply(seq_len(nrow(DRUG_GIVEN_DIAGNOSIS)), function(j) {
    r = DRUG_GIVEN_DIAGNOSIS[j]
    hit = unique(D3[str_detect(ICD10, paste0('^', r$prefix)), pat_id])
    hit = hit[runif(length(hit)) < r$prob]
    data.table(pat_id = hit, RXC = rep(r$rxc, length(hit)))
  }))
  ## Drug with no matching diagnosis (e.g. the diagnosis was never coded).
  ## Drawn mostly from the common classes.
  noise_rxc = data.table(RXC = sprintf('RXC_%02d', 1:10),
                         p = c(0.03, 0.01, 0.20, 0.02, 0.04, 0.20, 0.40, 0.02, 0.07, 0.01))
  noise = data.table(pat_id = sample(pat_id, ceiling(0.02 * n)))
  noise[, RXC := sample(noise_rxc$RXC, .N, TRUE, prob = noise_rxc$p)]
  want = unique(rbind(want, noise))                         # drug with no matching diagnosis

  RX = rbindlist(lapply(seq_len(nrow(want)), function(i) {
    ndc = NDC_CODES[RXC == want$RXC[i]][sample(.N, 1), NDC]
    fill_history(want$pat_id[i], ndc, want$RXC[i], year)
  }))
  if (nrow(RX) == 0) RX = data.table(pat_id = character(), NDC = character(), RXC = character(),
                                     drug_class = character(), fill_date = as.Date(character()), days_supply = integer())
  ## --- everyday drugs the scorer should ignore ------------------------
  bg_file = getOption('hccr.background_ndcs', 'tests/data/background_ndcs.csv')
  bg = fread(bg_file, colClasses = 'character')
  bg = bg[!ndc %in% NDC_CODES$NDC]               # never a risk-model drug
  bg[, w := BACKGROUND_DRUG_WEIGHT[drug]]
  bg[is.na(w), w := 1]
  has_dx = pat_id %in% D3$pat_id
  bg_fills = rbindlist(lapply(seq_len(nrow(BACKGROUND_DRUGS)), function(j) {
    cls = BACKGROUND_DRUGS[j]
    p = ifelse(pat_age < 21, cls$p_child,
               plogis(qlogis(cls$p50) + cls$slope * (pat_age - 50) / 10))
    p = pmin(0.95, p * ifelse(has_dx, 1.6, 1))   # people with diagnoses take more drugs
    on = which(runif(n) < p)
    if (length(on) == 0) return(NULL)
    pool_cls = bg[drug_class == cls$drug_class]
    drugs = unique(pool_cls[, .(drug, w)])
    rbindlist(lapply(on, function(i) {
      k = if (cls$drug_class == 'blood_pressure') sample(1:3, 1, prob = c(0.6, 0.3, 0.1)) else 1
      chosen = drugs[sample(.N, min(k, .N), prob = w), drug]
      rbindlist(lapply(chosen, function(d)
        fill_history(pat_id[i], pool_cls[drug == d][sample(.N, 1), ndc], NA_character_, year,
                     drug_class = cls$drug_class, adherence = 0.8, acute = !cls$chronic)))
    }))
  }))
  RX = rbind(RX, bg_fills)
  NDC = unique(RX[, .(pat_id, NDC)])
  setkey(NDC, pat_id)

  ## --- infused/injected drugs billed on medical claims ----------------
  inf = sample(pat_id, ceiling(0.01 * n))
  HCPCS = data.table(pat_id = inf, HCPCS = HCPCS_CODES[sample(.N, length(inf), TRUE), HCPCS])
  setkey(HCPCS, pat_id)

  ## --- expected categories after hierarchies --------------------------
  ## Done with plain lists rather than apply_hierarchy() so the check is
  ## independent of that code.
  zero_map = split(SetToZero$set_zero, SetToZero$HCC)
  rxc_zero_map = split(RXCSetToZero$set_zero, RXCSetToZero$HCC)

  dx_cats = merge(unique(D3[, .(pat_id, ICD10)]), unique(HCC2[, .(ICD10, HCC)]), by = 'ICD10')[, .(pat_id, cat = HCC)]
  rx_cats = rbind(unique(RX[!is.na(RXC), .(pat_id, cat = RXC)]),
                  merge(HCPCS, HCPCS_CODES, by = 'HCPCS')[, .(pat_id, cat = RXC)])
  survivors = function(cats, zmap) {
    cats = unique(cats)
    cats[, .(cat = {
      drop = unlist(zmap[cat], use.names = FALSE)
      setdiff(cat, drop)
    }), by = pat_id]
  }
  expected = rbind(survivors(dx_cats, zero_map), survivors(rx_cats, rxc_zero_map))

  list(DM2 = DM2, D3 = D3, HCPCS = HCPCS, NDC = NDC, RX = RX, expected = expected)
}
