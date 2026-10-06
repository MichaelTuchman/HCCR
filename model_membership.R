## model_membership.R
##
## Which risk model (Adult, Child or Infant) a patient belongs to, read from
## Table 1 of the workbook ("Model Membership") instead of being implied by
## which age/sex bands happen to exist. That matters for infants: Table 5
## has age/sex bands for male infants only (AGE0_MALE, AGE1_MALE), so a
## female infant has no band, but she is still an infant and is scored from
## the maturity and severity variables in Table 8.
##
## Table 1 lists one row per model with a definition such as
##   "21 <= AGE_LAST"   or   "2 <= AGE_LAST <= 20"

require(readxl)
require(stringr)
require(data.table)

## Table of model, lo, hi (inclusive ages), one row per model.

read_model_membership = function(path, sheet) {
  raw = suppressMessages(read_excel(path, sheet = sheet, col_names = FALSE,
                                    col_types = 'text', .name_repair = 'minimal'))
  raw = data.table(model = trimws(raw[[1]]), variable = trimws(raw[[2]]),
                   definition = str_squish(raw[[ncol(raw)]]))
  rows = raw[model %in% c('Adult', 'Child', 'Infant') & variable == 'AGE_LAST']
  if (nrow(rows) != 3)
    stop(sprintf("Table 1: expected one AGE_LAST row each for Adult, Child and Infant but found %d (%s). Check the sheet name and layout.",
                 nrow(rows), paste(rows$model, collapse = ', ')), call. = FALSE)
  nums = lapply(str_extract_all(rows$definition, '\\d+'), as.numeric)
  rows[, `:=`(lo = sapply(nums, `[`, 1),
              hi = sapply(nums, function(n) if (length(n) >= 2) n[2] else Inf))]
  rows[, .(model, lo, hi)]
}

## Model name for each age; NA where no model's range includes it.

model_for_age = function(age, membership = ModelMembership) {
  out = rep(NA_character_, length(age))
  for (i in seq_len(nrow(membership)))
    out[!is.na(age) & age >= membership$lo[i] & age <= membership$hi[i]] = membership$model[i]
  out
}
