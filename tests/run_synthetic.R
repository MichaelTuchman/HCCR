## tests/run_synthetic.R
## Run the whole scoring pipeline on tests/synthetic_data.R.
## From the repo root:  Rscript tests/run_synthetic.R
##
## Model_Inputs.R sources config.R and reads config.yaml's model.year /
## model.workbook.path, so this runs against whichever benefit year's
## workbook config.yaml currently points at - no database needed.

source('Model_Inputs.R')
source('AgeSexfactors.R')
source('interactions.R')
source('apply_hcc.R')
source('tests/synthetic_data.R')
source('score_model.R')

print(STEP2)
print(STEP6[value != 0 & !Variable %like% '^(ED_|[MF]AGE)', .(pat_id, Model, Variable)])
print(Answer[order(pat_id), .(pat_id, Model, pat_age, pat_gender, Silver)])
