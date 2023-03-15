# script for creating models
library(tidyverse)
library(mice)
library(broom)

# read data
mids_analysis <- read_rds("processed_data/mids_analysis.rds")

##### Performing analyses #####

fit <- glm(edulate ~ gesl + lbw + preterm + ISCED_ma + ISCED_pa + SECMma + SECM_pa,
  family = binomial(), data = mids_analysis
)


#### outcome = edulate
## Consider glmnet (more efficient?)
# library(glmnet)
# fit <- glmnet(Data1imp[, c("ISCED_ma", "ISCED_pa")], Data1imp$edulate, family = "binomial")
# print(fit)
# coef(fit)

### Checking for factorial polynomial transformations of continuous variables
# library(mfp)

### standard logistic regression
## fit for all imputed datasets
for (i in 1:3) {
  readRDS(file = paste0("Data1imp", i, ".rds"))

  fit <- glm(edulate ~ gesl + lbw + preterm + ISCED_ma + ISCED_pa + SECM_ma + SECM_pa,
    family = binomial(link = "logit"), data = Data1imp
  )
  summary(fit)

  saveRDS(fit, file = paste0("fit", i, ".rds"))
}


### import all model fits and pool models
## Create list of model fits
listofmodels <- list()

for (i in 1:3) {
  listofmodels[[i]] <- assign(paste0("fit", i), readRDS(file = paste0("fit", i, ".rds")))
}

## convert list back into mira type (for MICE functions)
listofmodels <- as.mira(listofmodels)
# pool model fits
fit_pool <- pool(listofmodels)
summary(fit_pool)

# To extract estimates, ORs, and 95% CIs and p-values
fit_pool_summ <- summary(fit_pool)

fit_pool_est <- cbind(
  names(readRDS(file = "fit1.rds")$coefficients), fit_pool_summ[, 2], fit_pool_summ[, 2] - 1.96 * (fit_pool_summ[, 3]),
  fit_pool_summ[, 2] + 1.96 * (fit_pool_summ[, 3]), fit_pool_summ[, 4]
)
colnames(fit_pool_est) <- (c("Variable", "estimate", "95% Lower", "95% Upper", "p-value"))
fit_pool_est

fit_pool_OR <- cbind(
  names(readRDS(file = "fit1.rds")$coefficients), exp(fit_pool_summ[, 2]), exp(fit_pool_summ[, 2] - 1.96 * (fit_pool_summ[, 3])),
  exp(fit_pool_summ[, 2] + 1.96 * (fit_pool_summ[, 3])), fit_pool_summ[, 4]
)
colnames(fit_pool_OR) <- (c("Variable", "OR", "95% Lower", "95% Upper", "p-value"))
fit_pool_OR

# ### ALTERNATIVE: Using mitools (https://bookdown.org/mwheymans/bookmi/data-analysis-after-multiple-imputation.html)
# library(mitools)
#
# coefs <- MIextract(listofmodels, fun = coefficients)
# se <- MIextract(listofmodels, fun = vcov)
