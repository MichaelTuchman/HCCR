## config.R
##
## Loads config.yaml once into the global CONFIG list and provides a few
## small helpers the rest of the pipeline uses to look up a sheet name,
## a header-skip offset, or the "valid.<year>" column for the current
## model year. This is what makes Model_Inputs.R (and, for the pieces
## that are wired up, readClientData.R) work for any benefit year's
## workbook instead of only CY2022.
##
## Usage: source('config.R') before Model_Inputs.R. Model_Inputs.R does
## this itself, so you normally don't need to source it directly.

require(yaml)

## Allow a different config file to be used by setting this option
## before sourcing, e.g. options(hccr.config_path = 'config-2023.yaml')

config_path = getOption('hccr.config_path', 'config.yaml')

if (!file.exists(config_path)) {
  stop(sprintf(
    "HCCR config file not found: '%s'. Copy config.yaml and point it at the model year/workbook you want, or set options(hccr.config_path=...) before sourcing config.R.",
    config_path))
}

CONFIG = read_yaml(config_path)

## ---- model-year / workbook helpers -----------------------------------

hccr_model_year = function(cfg = CONFIG) {
  as.integer(cfg$model$year)
}

hccr_workbook_path = function(cfg = CONFIG) {
  path = cfg$model$workbook$path
  if (!file.exists(path)) {
    stop(sprintf(
      "Workbook not found for model year %d: '%s'. Download that year's 'DIY tables' workbook from CMS (see README -> Updating to a new model year) and update config.yaml's model.workbook.path.",
      hccr_model_year(cfg), path))
  }
  return(path)
}

## Sheet name for a logical table key (e.g. 'icd10_crosswalk' -> 'Table 3').
## Also accepts a literal sheet name/number for callers (like
## model_factors()) that already have one, so existing call sites don't
## all have to change shape.

hccr_sheet = function(key, cfg = CONFIG) {
  sheets = cfg$model$workbook$sheets
  if (!is.null(sheets[[key]])) return(sheets[[key]])
  return(key) # fall through: caller already passed a literal sheet name/number
}

## Header-skip offset for a logical table key. Falls back to a supplied
## default (rather than erroring) since not every table needs an entry.

hccr_skip = function(key, default = 0, cfg = CONFIG) {
  skips = cfg$model$workbook$header_skip
  if (!is.null(skips[[key]])) return(as.integer(skips[[key]]))
  return(default)
}

hccr_metal_tiers = function(cfg = CONFIG) {
  unlist(cfg$model$metal_tiers)
}

hccr_output_csv = function(cfg = CONFIG) {
  cfg$output$risk_scores_csv
}

## ---- database helpers (config wiring only - see README known gaps) ---

hccr_db_config = function(cfg = CONFIG) {
  cfg$database
}
