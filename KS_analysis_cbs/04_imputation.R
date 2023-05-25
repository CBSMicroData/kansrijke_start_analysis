##### MICE #####
### Create 3 imputed datasets
library(tidyverse)
library(mice)

# read data
df_analysis <- read_rds("processed_data/analysis_data.rds")

# exclude vars
excluded_vars <- c("RINPERSOONS_ki", "RINPERSOON_ki", "ID_ki", "RINPERSOONS_ma", "RINPERSOON_ma",  "ID_ma", 
                   "RINPERSOONS_pa", "RINPERSOON_pa", "ID_pa", 
                   "SOORTOBJECTNUMMER_ki", "RINOBJECTNUMMER_ki", "postcode4_ki", "gemeentecode_ki", "wijkcode_ki", "buurtcode_ki", 
                   "SOORTOBJECTNUMMER_ma", "RINOBJECTNUMMER_ma", "postcode4_ma", "gemeentecode_ma", "wijkcode_ma", "buurtcode_ma", 
                   "SOORTOBJECTNUMMER_pa", "RINOBJECTNUMMER_pa", "postcode4_pa", "gemeentecode_pa", "wijkcode_pa", "buurtcode_pa", 
                   "dd_3", "ddgeb_ma", "ddgeb_pa", "OPLNIVHB_ki_2018", "OPLNIVHB_ma", "OPLNIVHB_pa")

# make the predictor matrix
pred_mat <- make.predictorMatrix(df_analysis)
pred_mat[, excluded_vars] <- 0

## Skip the following variables from being imputed
method_selection <- make.method(df_analysis)
method_selection[excluded_vars] <- ""

### No error when performing imputation for full dataset
## Creating imputations
mids_analysis <- mice(
  data = df_analysis, 
  m = 3, 
  method = method_selection, 
  predictorMatrix = pred_mat,
  maxit = 1
)

write_rds(mids_analysis, file = "processed_data/mids_analysis.rds")
