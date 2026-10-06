# HCCR to-do list

Status of the remaining work, organized by system component. See the
[README](../README.md) for what the project is and its known gaps. Items
marked **DB** need a live database connection to test and are unverified.

## Project direction

CMS now publishes official Python software for the HHS-HCC model (phased in
from benefit year 2026), so HCCR is a reference implementation and portfolio
example, not a production scorer. The goal of the work below is to make the
R code readable and able to handle the current CMS features, so that it can
later be compared against CMS's Python software.

## Done recently

- **CY2025 workbook now reads and scores.** `table3_reader.R` finds Table 3's
  columns by header (13 columns in CY2022, 17 in CY2025). `interactions.R`
  compiles the `HCC_CNT` count and the rules that test it; `score_model.R`
  passes `ENROLDURATION` to the rules (`HCC_ED1`-`HCC_ED11`). CY2022 output is
  unchanged. Tests: `tests/run_cy2025.R`.
- **Broader synthetic population.** `tests/generate_synthetic.R` and
  `tests/run_broad.R` generate and score thousands of patients with
  diagnoses sampled from Table 3, risk-model drugs from Table 10a, and
  everyday drugs the model ignores (real NDCs from the FDA NDC Directory).
  Checks category assignment and hierarchies against an independent
  calculation; passes on CY2022 and CY2025. The NDC (pharmacy) path of the
  scorer is now exercised.
- **Table 4 reader** no longer caps a row at 8 set-to-zero categories.

## Next up (in order)

1. **Affiliated Cost Factors (ACFs).** New for CY2026: Tables 10c
   (ACF to NDC) and 10d (ACF to HCPCS). Needs the CY2026 workbook, which was
   not on the CMS page at the time of writing; locate it first. The CMS
   resources page that lists the workbooks is
   https://www.cms.gov/cciio/resources/regulations-and-guidance
2. **Compare against CMS's Python software.** Locate the software (no
   download link was found in the DIY instructions or the implementation
   memo), run it on the same synthetic patients, and compare the HCC/RXC
   assignment layer first. The broad population is the input to use.

## Findings to keep in mind

- The sheet in the CY2025 workbook is named `"Table 2 "` (trailing space);
  a config key `"Table 2"` would not match it.
- CMS's instructions say Python is being phased in from benefit year 2026; a
  CMS implementation memo says SAS and Python are both released through
  CY2027, and Python only from CY2028. Not reconciled.
- Not yet compared with CMS's software for any year. CY2022 and CY2025 run
  end to end; the tests check category assignment and hierarchies, not that
  the scores match CMS.
- CMS's instructions list four input files for its software: PERSON
  (including metal level, CSR indicator, enrollment duration), DIAG (with
  age at diagnosis), NDC and HCPCS. Our pipeline has the same four inputs
  except the CSR indicator and a separate age at diagnosis.

## 1. Workbook ingestion (`Model_Inputs.R`, `workbook_readers.R`, `config.yaml`)

- [x] `Model_Inputs.R` split into one reader function per table
      (`workbook_readers.R`); output unchanged (identical score files on
      every test, both years).

- [x] Second benefit year (CY2025) runs end to end.
- [ ] Decide how to handle a year that adds a table (Table 13 is handled by
      `csr.R`; 10c and 10d are new for CY2026). Table 3's layout change is handled
      by header matching; other tables still use fixed positions.
- [ ] Tables 1, 2 and 12 are not read; confirm that skipping them is
      intentional.
- [ ] Fail fast with a clear message if a configured sheet name is missing
      from the workbook.

## 2. Age/sex model (`AgeSexfactors.R`)

- [ ] The `sql1`/`sql2` SQL text this script builds is never used: wire it
      in or remove it.
- [ ] `AGE_LAST` is computed but unused; confirm that is intentional.
- [ ] Enrollment-duration names (`ED_1` ... `ED_11`) are assumed, not read
      from the workbook.

## 3. Interactions / SAS-rule compiler (`interactions.R`)

- [ ] The parser is not a full SAS grammar. Add a test that fails loudly on
      an unrecognized `if/then` shape.
- [ ] Add a coverage check that every `Used` row in Tables 6-8 produced a
      rule. (CY2025 added a non-`if` form, `HCC_CNT = SUM(...)`, now
      compiled; a further new shape would again need new code.)

## 4. HCC assignment and hierarchies (`apply_hcc.R`)

- [ ] The workbook distinguishes age at diagnosis from age at year end, but
      client data has one `pat_age`, used for both.
- [ ] Add a test that every HCC named in Table 4 exists after widening.
      (A one-off check on CY2022 passed; it is not automated.)
- Done: the Table 4 reader no longer caps a row at 8 set-to-zero
  categories.

## 5. Prescription drug categories (RXC)

- [ ] **DB.** Pull pharmacy claims from a real database; `assign_rxc(NDC_CODES)` is ready but
      `readClientData.R` does not read an NDC table yet. Add its table and
      NDC column to `config.yaml`.
- [x] NDC path tested by the broad population (`tests/run_broad.R`).

## 6. Client data integration (`readClientData.R`) - all **DB**

Out of scope for the reference-implementation direction unless a real
database is available.

| Part | Reads from (config key) | Status |
|---|---|---|
| Eligibility summary | `database.tables.eligibility` + `columns.eligibility.*` | Config-wired, unverified |
| Age/sex/enrollment | same, plus `database.period.*` | Config-wired, unverified |
| Diagnosis codes (5 columns) | `database.tables.claims` + `columns.claims.diagnosis_codes` | Config-wired; column count assumed |
| Procedure/HCPCS codes | `database.tables.claims` + `procedure_code`, `procedure_code_type` | Config-wired, unverified |
| Pharmacy/NDC claims | none configured | Not started |

- [ ] Run against a real database.
- [ ] Decide how to support more than one client (one config per client, or
      a `clients:` list).

## 7. Scoring (`score_model.R`)

- [x] Female infants now score (maturity x severity only); Table 1 model
      ages are read by `model_membership.R`. Infant behavior still needs a
      comparison with CMS's software.
- [x] Cost-sharing adjustment applied for CY2025+ (`csr.R`).
- [ ] Map a plan's HIOS variant and metal level to a CSR indicator (Table 13
      has the lookup); today the client supplies `csr_indicator`.
- [ ] Pick the patient's own plan tier instead of reporting all five.
- [ ] Log how many duplicate patients `dup_resolve` drops per run.

## 8. Reporting (`byPerson.R`, `slices.R`)

- [ ] `slices.R` uses `HCC_HELPER`, which is not defined in the repo.
- [ ] Neither script is parameterized by metal tier; both assume Silver.

## 9. Testing

- [x] `Rscript tests/run_synthetic.R` passes on CY2022 and matches the
      pre-refactor output (R 4.3.3).
- [x] `Rscript tests/run_cy2025.R`: CY2025 workbook, plus checks of `HCC_CNT`
      and `HCC_ED`. Needs the CY2025 workbook downloaded (not in the repo).
- [x] `Rscript tests/run_csr.R`: Table 13 mapping vs Tables 6-8, and adjusted
      scores = score x factor on a generated population (CY2025).
- [x] `Rscript tests/run_broad.R [patients] [seed]`: generated population;
      20,000 patients took about 70 seconds.
- [x] `RiskScoresFinal.csv` and the downloaded CY2025 workbook are in
      `.gitignore`.
- [ ] Run the tests on Windows R 4.6.0 too; so far they have run on Linux
      R 4.3.3 only.
- [ ] Tests do not yet cover age/sex-restricted diagnosis codes at scale (the
      generator uses only unrestricted codes; the small test covers a few).
- [ ] No test compares scores with CMS's software.
- [ ] Add a test for the SQL that `readClientData.R` builds from config
      (mocked connection).

## 10. Cleanup

- [x] Removed `readEnrollment.R`, `ageSexRisk.R`, `age_sex_testing.R`,
      `so_r_example.R`, `please_determine.R`, `pca2.R`.
- [ ] Decide whether the procedure-code clustering scripts (`pca.R`,
      `DP_Hardcodes.R`, `cpur.R`, `leaking_diagnosis.R`) belong in this repo,
      or should be archived under a labeled folder.
