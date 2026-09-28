## tests/synthetic_data.R
## Stand-in for readClientData.R: a handful of made-up patients that
## exercise the hierarchy, age/sex filter, and drug (RXC) logic without
## a database connection. Produces D3, DM2 and HCPCS in the same shape
## readClientData.R does.

require(data.table)

DM2 = data.table(
  pat_id        = c('P1',  'P2',  'P3',  'P4',  'P5',  'P6'),
  pat_gender    = c('F',   'M',   'F',   'M',   'M',   'F'),
  pat_age       = c(45,    30,    6,     40,    50,    55),
  ENROLDURATION = c(12,    12,    12,    6,     12,    12))

D3 = data.table(
  pat_id = c('P1',   'P1',     'P2',    'P3',    'P4',  'P6'),
  ICD10  = c('C787', 'C50911', 'E1165', 'O0000', 'C61', 'C50911'))
# P1: metastatic cancer (HCC 8) + breast cancer under 50 (HCC 11): 11 should be dropped
# P2: type 2 diabetes (HCC 21), plus insulin below
# P3: pregnancy code at age 6: fails the 9 <= age <= 64 condition, dropped
# P4: prostate cancer (HCC 12), male, 6 months enrolled
# P5: no diagnoses, insulin only
# P6: breast cancer at 55: age split picks HCC 12, not 11

D3 = merge(D3, DM2[, .(pat_id, pat_gender, pat_age)], by = 'pat_id')

HCPCS = data.table(pat_id = c('P2', 'P5'), HCPCS = c('J1815', 'J1815'))  # insulin, RXC 6
setkey(HCPCS, pat_id)
