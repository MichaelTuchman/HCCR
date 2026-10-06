## Model_Inputs.R
##
## Reads the CMS workbook (Tables 1-13) into the objects the scorer uses.
## The reading itself is done by small functions in workbook_readers.R,
## table3_reader.R, model_membership.R and csr.R; this file just calls them
## in order and names the results.
##
## Note: starts with rm(list=ls()), so set any arguments *after* sourcing.

rm(list=ls())

require(rlang)
require(tidyverse)
require(data.table)
require(readxl)
require(readr)
require(lubridate)
require(yaml)

source('config.R')              # CONFIG + hccr_*() helpers, from config.yaml
source('workbook_readers.R')    # one function per workbook table
source('model_membership.R')    # which model (Adult/Child/Infant) an age belongs to, from Table 1
source('csr.R')                 # Table 13 CSR indicators and the cost-sharing adjustment
source('table3_reader.R')       # Table 3, found by header text so layout changes between years don't break it

## Model year and workbook path come from config.yaml. To move to a new
## benefit year, download that year's "DIY tables" workbook from CMS and
## point config.yaml at it - see README "Updating to a new model year".
MODEL_YEAR = hccr_model_year()
fn = hccr_workbook_path()

## Table 1: which model each age belongs to
ModelMembership = read_model_membership(fn, hccr_sheet('model_membership'))

## Tables 6-8: the Adult, Child and Infant variable definitions (SAS-style rules)
AllAges = read_all_variable_rules(fn)

## Table 3: ICD-10 code -> condition category
HCC  = read_icd10_crosswalk(fn, sheet = hccr_sheet('icd10_crosswalk'),
                            first_data_row = hccr_skip('icd10_crosswalk', 4),
                            model_year = MODEL_YEAR) %>% as_tibble
HCC2 = expand_diagnosis_categories(HCC)

## Table 4: which categories a more serious one zeroes out
SetToZero = read_hierarchies(fn, hccr_sheet('hierarchies'), hccr_skip('hierarchies', 3))

## Table 5: demographic (age/sex) bands
AgeSexBands = read_age_sex_bands(fn, hccr_sheet('age_sex_bands'), hccr_skip('age_sex_bands', 2))

## Table 9: coefficients, long and one column per metal tier
ModelFactors = read_model_factors(fn, hccr_sheet('model_factors'), hccr_skip('model_factors', 2))
MF_Wide = widen_model_factors(ModelFactors)
Metals  = hccr_metal_tiers()   # from config.yaml model.metal_tiers

## Tables 10a, 10b, 11: prescription drug categories (RXC)
HCPCS_CODES   = rxc_crosswalk(fn, hccr_sheet('rxc_hcpcs_crosswalk'), 'HCPCS', hccr_skip('rxc_hcpcs_crosswalk', 3))
NDC_CODES     = rxc_crosswalk(fn, hccr_sheet('rxc_ndc_crosswalk'),   'NDC',   hccr_skip('rxc_ndc_crosswalk', 3))
RXCSetToZero  = read_rxc_hierarchies(fn, hccr_sheet('rxc_hierarchies'), hccr_skip('rxc_hierarchies', 3))
RXCvars       = rxc_name(1:10)

## Table 13: CSR indicator -> metal tier and factor. Only years whose workbook
## has the table (CY2025 on) list it in config.yaml; without it no
## cost-sharing adjustment is applied.
CSRByIndicator = NULL
if (!is.null(CONFIG$model$workbook$sheets$csr_indicators)) {
  CSRTable = read_csr_table(fn, hccr_sheet('csr_indicators'))
  CSRByIndicator = csr_by_indicator(CSRTable)
}
