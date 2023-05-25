

##### Model performance in ONE dataset #####
### Hacky way to use predict(): replace stored coefficients from one model fit with final pooled estimates
### https://stackoverflow.com/questions/52713733/how-to-use-predict-function-with-my-pooled-results-from-mice
### Stef van Buuren suggested alternative ways: https://github.com/amices/mice/issues/82
fit_pool_glm <- readRDS(file = "fit1.rds")
fit_pool_glm$coefficients <- summary(fit_pool)$estimate

# import one imputed dataset (just for outcome)
Data1imp <- readRDS(file = "Data1imp1.rds")

## Accuracy
predMODEL <- predict(fit_pool_glm, newdata = Data1imp, type='response')
fitMODEL <- ifelse(predMODEL >= quantile(predMODEL, 0.6),1,0) ## Define model threshold
misClasificErrorMODEL <- mean(fitMODEL != Data1imp$edulate)
print(paste('Accuracy MODEL',1-misClasificErrorMODEL))

## AUC
library(pROC)
predictionMODEL <- roc(Data1imp$edulate, predMODEL)
predictionMODEL
ci.auc(predictionMODEL, conf.level=0.95, method="delong")

## sensitivity, specificity, PPV, NPV
library(caret)
mtxMODEL <- confusionMatrix(table(fitMODEL, Data1imp$edulate))$table
mtxMODEL # columns represent actual outcomes; rows represent predicted outcomes
prop.table(mtxMODEL, 2)

## Sensitivity
sens_errorsMODEL <- sqrt(sensitivity(mtxMODEL) * (1 - sensitivity(mtxMODEL)) / sum(mtxMODEL[,1]))
sensLowerMODEL <- sensitivity(mtxMODEL) - 1.96 * sens_errorsMODEL
sensUpperMODEL <- sensitivity(mtxMODEL) + 1.96 * sens_errorsMODEL

## Specificity
spec_errorsMODEL <- sqrt(specificity(mtxMODEL) * (1 - specificity(mtxMODEL)) / sum(mtxMODEL[,2]))
specLowerMODEL <- specificity(mtxMODEL) - 1.96 * spec_errorsMODEL
specUpperMODEL <- specificity(mtxMODEL) + 1.96 * spec_errorsMODEL

## Positive Predictive Values
ppv_errorsMODEL <- sqrt(posPredValue(mtxMODEL) * (1 - posPredValue(mtxMODEL)) / sum(mtxMODEL[1,]))
ppvLowerMODEL <- posPredValue(mtxMODEL) - 1.96 * ppv_errorsMODEL
ppvUpperMODEL <- posPredValue(mtxMODEL) + 1.96 * ppv_errorsMODEL

## Negative Predictive Values
npv_errorsMODEL <- sqrt(negPredValue(mtxMODEL) * (1 - negPredValue(mtxMODEL)) / sum(mtxMODEL[2,]))
npvLowerMODEL <- negPredValue(mtxMODEL) - 1.96 * npv_errorsMODEL
npvUpperMODEL <- negPredValue(mtxMODEL) + 1.96 * npv_errorsMODEL

paste0(format(round(confusionMatrix(table(fitMODEL, Data1imp$edulate))[["byClass"]][["Sensitivity"]], 3), nsmall = 3),", ", 
       format(round(sensLowerMODEL, 3), nsmall = 3), "-", format(round(sensUpperMODEL, 3), nsmall = 3))

paste0(format(round(confusionMatrix(table(fitMODEL, Data1imp$edulate))[["byClass"]][["Specificity"]], 3), nsmall = 3),", ", 
       format(round(specLowerMODEL, 3), nsmall = 3), "-", format(round(specUpperMODEL, 3), nsmall = 3))

paste0(format(round(confusionMatrix(table(fitMODEL, Data1imp$edulate))[["byClass"]][["Pos Pred Value"]], 3), nsmall = 3),", ", 
       format(round(ppvLowerMODEL, 3), nsmall = 3), "-", format(round(ppvUpperMODEL, 3), nsmall = 3))

paste0(format(round(confusionMatrix(table(fitMODEL, Data1imp$edulate))[["byClass"]][["Neg Pred Value"]], 3), nsmall = 3),", ", 
       format(round(npvLowerMODEL, 3), nsmall = 3), "-", format(round(npvUpperMODEL, 3), nsmall = 3))


##### Model performance across multiple imputed datasets (with confidence bands) #####
### Hacky way to use predict(): replace stored coefficients from one model fit with final pooled estimates
### https://stackoverflow.com/questions/52713733/how-to-use-predict-function-with-my-pooled-results-from-mice
### Stef van Buuren suggested alternative ways: https://github.com/amices/mice/issues/82
library(tidyverse)
fit_pool_glm <- readRDS(file = "fit1.rds")
fit_pool_glm$coefficients <- summary(fit_pool)$estimate

# import one imputed dataset (just for outcome)
Data1imp <- readRDS(file = "Data1imp1.rds")

# Create empty tibble
tableMODEL <- tibble(
  modelname = character(),
  outcomename = character(),
  dataset = character(),
  n = integer(),
  cases = integer(),
  ROCAUC = numeric(),
  threshold = numeric(),
  true_positive = integer(),
  false_positive = integer(),
  true_negative = integer(),
  false_negative = integer(),
  true_positive_prop = numeric(),
  false_positive_prop = numeric(),
  true_negative_prop = numeric(),
  false_negative_prop = numeric(),
  sensitivity = numeric(),
  specificity = numeric(),
  PPV = numeric(),
  NPV = numeric()
)

# Define the following:
modelname1 <- "fit_pool_glm"
outcome1 <- Data1imp$edulate
outcomename1 <- "edulate"
dataset1 <- "Data1imp"
threshold1 <- as.list(seq(0.2,0.8,0.2))

#### Start loop
library(pROC)
library(caret)
for (i in 1:3) { # Define number of datasets
  # import one imputed dataset (just for outcome)
  Data1imp <- readRDS(file = paste0(dataset1, i, ".rds")) ## Define dataset
  
  ### Start loop for each threshold
  for (j in threshold1) {
    ## Accuracy
    predMODEL <- predict(get(modelname1), newdata = Data1imp, type='response') ## Define model
    fitMODEL <- ifelse(predMODEL >= quantile(predMODEL, j), 1, 0) ## Define model threshold
    misClasificErrorMODEL <- mean(fitMODEL != outcome1) ## Define dataset and outcome
    
    ## AUC
    predictionMODEL <- roc(outcome1, predMODEL) ## Define dataset and outcome
    
    ## sensitivity, specificity, PPV, NPV
    mtxMODEL <- confusionMatrix(table(fitMODEL, outcome1))$table ## Define dataset and outcome
    mtxMODEL # columns represent actual outcomes; rows represent predicted outcomes
    mtxMODEL_prop <- prop.table(mtxMODEL, 2)
    
    ## Sensitivity
    sens_errorsMODEL <- sqrt(sensitivity(mtxMODEL) * (1 - sensitivity(mtxMODEL)) / sum(mtxMODEL[,1]))
    
    ## Specificity
    spec_errorsMODEL <- sqrt(specificity(mtxMODEL) * (1 - specificity(mtxMODEL)) / sum(mtxMODEL[,2]))
    
    ## Positive Predictive Values
    ppv_errorsMODEL <- sqrt(posPredValue(mtxMODEL) * (1 - posPredValue(mtxMODEL)) / sum(mtxMODEL[1,]))
    
    ## Negative Predictive Values
    npv_errorsMODEL <- sqrt(negPredValue(mtxMODEL) * (1 - negPredValue(mtxMODEL)) / sum(mtxMODEL[2,]))
    
    ## Summarize in tibble
    tableMODEL <- tableMODEL |> 
      add_row(
        modelname = modelname1,
        outcomename = outcomename1,
        dataset = paste0(dataset1, i),
        n = length(Data1imp),
        cases = length(outcome1 == 1),
        ROCAUC = predictionMODEL$auc[1],
        threshold = j,
        true_positive = mtxMODEL[4],
        false_positive = mtxMODEL[2],
        true_negative = mtxMODEL[1],
        false_negative = mtxMODEL[3],
        true_positive_prop = mtxMODEL_prop[4],
        false_positive_prop = mtxMODEL_prop[2],
        true_negative_prop = mtxMODEL_prop[1],
        false_negative_prop = mtxMODEL_prop[3],
        sensitivity = sens_errorsMODEL,
        specificity = spec_errorsMODEL,
        PPV = ppv_errorsMODEL,
        NPV = npv_errorsMODEL
      )
  }
}

### find median, upper, and lower confidence bands across imputed datasets for each threshold
### Find confidence band for each metric (width of confidence band depends on # datasets)
### i.e., 11 datasets = 80% CBs, 21 datasets = 90% CBs, 41 datasets = 95% CBs
## Define lower and upper CB
lowerCB <- 0.10
upperCB <- 0.90

for (j in threshold1) {
  # Median
  tableMODEL <- tableMODEL |> 
    add_row(
      modelname = modelname1,
      outcomename = outcomename1,
      dataset = paste0(dataset1,"1-3 median"), # Define number of datasets
      n = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(median(true_positive + false_positive + true_negative + false_negative))),
      cases = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(median(true_positive + false_negative))),
      ROCAUC = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(median(ROCAUC))),
      threshold = j,
      true_positive = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(median(true_positive))),
      false_positive = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(median(false_positive))),
      true_negative = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(median(true_negative))),
      false_negative = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(median(false_negative))),
      true_positive_prop = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(median(true_positive_prop))),
      false_positive_prop = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(median(false_positive_prop))),
      true_negative_prop = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(median(true_negative_prop))),
      false_negative_prop = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(median(false_negative_prop))),
      sensitivity = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(median(sensitivity))),
      specificity = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(median(specificity))),
      PPV = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(median(PPV))),
      NPV = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(median(NPV)))
    )
  # Lower CB
  tableMODEL <- tableMODEL |> 
    add_row(
      modelname = modelname1,
      outcomename = outcomename1,
      dataset = paste0(dataset1,"1-3 ",lowerCB*100,"% band"), # Define number of datasets
      n = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(true_positive + false_positive + true_negative + false_negative, probs = lowerCB))),
      cases = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(true_positive + false_negative, probs = lowerCB))),
      ROCAUC = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(ROCAUC, probs = lowerCB))),
      threshold = j,
      true_positive = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(true_positive, probs = lowerCB))),
      false_positive = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(false_positive, probs = lowerCB))),
      true_negative = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(true_negative, probs = lowerCB))),
      false_negative = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(false_negative, probs = lowerCB))),
      true_positive_prop = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(true_positive_prop, probs = lowerCB))),
      false_positive_prop = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(false_positive_prop, probs = lowerCB))),
      true_negative_prop = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(true_negative_prop, probs = lowerCB))),
      false_negative_prop = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(false_negative_prop, probs = lowerCB))),
      sensitivity = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(sensitivity, probs = lowerCB))),
      specificity = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(specificity, probs = lowerCB))),
      PPV = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(PPV, probs = lowerCB))),
      NPV = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(NPV, probs = lowerCB)))
    )
  # upper CB
  tableMODEL <- tableMODEL |> 
    add_row(
      modelname = modelname1,
      outcomename = outcomename1,
      dataset = paste0(dataset1,"1-3 ",upperCB*100,"% band"), # Define number of datasets
      n = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(true_positive + false_positive + true_negative + false_negative, probs = upperCB))),
      cases = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(true_positive + false_negative, probs = upperCB))),
      ROCAUC = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(ROCAUC, probs = upperCB))),
      threshold = j,
      true_positive = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(true_positive, probs = upperCB))),
      false_positive = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(false_positive, probs = upperCB))),
      true_negative = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(true_negative, probs = upperCB))),
      false_negative = as.integer(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(false_negative, probs = upperCB))),
      true_positive_prop = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(true_positive_prop, probs = upperCB))),
      false_positive_prop = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(false_positive_prop, probs = upperCB))),
      true_negative_prop = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(true_negative_prop, probs = upperCB))),
      false_negative_prop = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(false_negative_prop, probs = upperCB))),
      sensitivity = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(sensitivity, probs = upperCB))),
      specificity = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(specificity, probs = upperCB))),
      PPV = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(PPV, probs = upperCB))),
      NPV = as.numeric(tableMODEL |> 
        filter(threshold == j) |> 
        summarize(quantile(NPV, probs = upperCB)))
    )
}


## Save tibble
saveRDS(tableMODEL, file = "tableMODEL.rds")



##### Model performance per municipality across multiple imputed datasets (with confidence bands) #####
### Hacky way to use predict(): replace stored coefficients from one model fit with final pooled estimates
### https://stackoverflow.com/questions/52713733/how-to-use-predict-function-with-my-pooled-results-from-mice
### Stef van Buuren suggested alternative ways: https://github.com/amices/mice/issues/82
library(tidyverse)
fit_pool_glm <- readRDS(file = "fit1.rds")
fit_pool_glm$coefficients <- summary(fit_pool)$estimate

# import one imputed dataset (just for outcome)
Data1imp <- readRDS(file = "Data1imp1.rds")

# Create empty tibble
tableMODEL_gem <- tibble(
  modelname = character(),
  outcomename = character(),
  dataset = character(),
  municipality = character(),
  n = integer(),
  cases = integer(),
  ROCAUC = numeric(),
  threshold = numeric(),
  true_positive = integer(),
  false_positive = integer(),
  true_negative = integer(),
  false_negative = integer(),
  true_positive_prop = numeric(),
  false_positive_prop = numeric(),
  true_negative_prop = numeric(),
  false_negative_prop = numeric(),
  sensitivity = numeric(),
  specificity = numeric(),
  PPV = numeric(),
  NPV = numeric()
)

# Define the following:
modelname1 <- "fit_pool_glm"
outcomename1 <- "edulate"
dataset1 <- "Data1imp"
threshold1 <- as.list(seq(0.2,0.8,0.2))
municipality1 <- as.list(as.character(unique(na.omit(Data1imp$gemeentecode_ki))))

#### Start loop
library(pROC)
library(caret)
for (i in 1:3) { # Define number of datasets
  # import one imputed dataset (just for outcome)
  Data1imp <- readRDS(file = paste0(dataset1, i, ".rds")) ## Define dataset
  
  ### Start loop for each municipality
  for (k in municipality1) {
    Data1imp_mun <- Data1imp |> 
      filter(gemeentecode_ki == k)
    predMODEL <- predict(get(modelname1), newdata = Data1imp_mun, type='response') ## Define model
    
    ### Start loop for each threshold
    for (j in threshold1) {
      
      fitMODEL <- ifelse(predMODEL >= quantile(predMODEL, j), 1, 0) ## Define model threshold
      outcome1 <- Data1imp_mun$edulate
      
      ## Accuracy
      misClasificErrorMODEL <- mean(fitMODEL != outcome1) ## Define dataset and outcome
    
      ## AUC
      predictionMODEL <- roc(outcome1, predMODEL) ## Define dataset and outcome
      
      ## sensitivity, specificity, PPV, NPV
      mtxMODEL <- confusionMatrix(table(fitMODEL, outcome1))$table ## Define dataset and outcome
      mtxMODEL # columns represent actual outcomes; rows represent predicted outcomes
      mtxMODEL_prop <- prop.table(mtxMODEL, 2)
      
      ## Sensitivity
      sens_errorsMODEL <- sqrt(sensitivity(mtxMODEL) * (1 - sensitivity(mtxMODEL)) / sum(mtxMODEL[,1]))
      
      ## Specificity
      spec_errorsMODEL <- sqrt(specificity(mtxMODEL) * (1 - specificity(mtxMODEL)) / sum(mtxMODEL[,2]))
      
      ## Positive Predictive Values
      ppv_errorsMODEL <- sqrt(posPredValue(mtxMODEL) * (1 - posPredValue(mtxMODEL)) / sum(mtxMODEL[1,]))
      
      ## Negative Predictive Values
      npv_errorsMODEL <- sqrt(negPredValue(mtxMODEL) * (1 - negPredValue(mtxMODEL)) / sum(mtxMODEL[2,]))
      
      ## Summarize in tibble
      tableMODEL_gem <- tableMODEL_gem |> 
        add_row(
          modelname = modelname1,
          outcomename = outcomename1,
          dataset = paste0(dataset1, i),
          municipality = k,
          n = length(Data1imp_mun),
          cases = length(outcome1 == 1),
          ROCAUC = predictionMODEL$auc[1],
          threshold = j,
          true_positive = mtxMODEL[4],
          false_positive = mtxMODEL[2],
          true_negative = mtxMODEL[1],
          false_negative = mtxMODEL[3],
          true_positive_prop = mtxMODEL_prop[4],
          false_positive_prop = mtxMODEL_prop[2],
          true_negative_prop = mtxMODEL_prop[1],
          false_negative_prop = mtxMODEL_prop[3],
          sensitivity = sens_errorsMODEL,
          specificity = spec_errorsMODEL,
          PPV = ppv_errorsMODEL,
          NPV = npv_errorsMODEL
      )
    }
  }
}

### find median, upper, and lower confidence bands across imputed datasets for each threshold
### Find confidence band for each metric (width of confidence band depends on # datasets)
### i.e., 11 datasets = 80% CBs, 21 datasets = 90% CBs, 41 datasets = 95% CBs
## Define lower and upper CB
lowerCB <- 0.10
upperCB <- 0.90

for (k in municipality1) {
  for (j in threshold1) {
    # Median
    tableMODEL_gem <- tableMODEL_gem |> 
      add_row(
        modelname = modelname1,
        outcomename = outcomename1,
        dataset = paste0(dataset1,"1-3 median"), # Define number of datasets
        municipality = k,
        n = as.integer(tableMODEL |> 
          filter(municipality == k & threshold == j) |> 
          summarize(median(true_positive + false_positive + true_negative + false_negative))),
        cases = as.integer(tableMODEL |> 
          filter(municipality == k & threshold == j) |> 
          summarize(median(true_positive + false_negative))),
        ROCAUC = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(median(ROCAUC))),
        threshold = j,
        true_positive = as.integer(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(median(true_positive))),
        false_positive = as.integer(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(median(false_positive))),
        true_negative = as.integer(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(median(true_negative))),
        false_negative = as.integer(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(median(false_negative))),
        true_positive_prop = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(median(true_positive_prop))),
        false_positive_prop = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(median(false_positive_prop))),
        true_negative_prop = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(median(true_negative_prop))),
        false_negative_prop = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(median(false_negative_prop))),
        sensitivity = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(median(sensitivity))),
        specificity = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(median(specificity))),
        PPV = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(median(PPV))),
        NPV = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(median(NPV)))
      )
    # Lower CB
    tableMODEL_gem <- tableMODEL_gem |> 
      add_row(
        modelname = modelname1,
        outcomename = outcomename1,
        dataset = paste0(dataset1,"1-3 ",lowerCB*100,"% band"), # Define number of datasets
        municipality = k,
        n = as.integer(tableMODEL |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(true_positive + false_positive + true_negative + false_negative, probs = lowerCB))),
        cases = as.integer(tableMODEL |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(true_positive + false_negative, probs = lowerCB))),
        ROCAUC = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(ROCAUC, probs = lowerCB))),
        threshold = j,
        true_positive = as.integer(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(true_positive, probs = lowerCB))),
        false_positive = as.integer(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(false_positive, probs = lowerCB))),
        true_negative = as.integer(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(true_negative, probs = lowerCB))),
        false_negative = as.integer(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(false_negative, probs = lowerCB))),
        true_positive_prop = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(true_positive_prop, probs = lowerCB))),
        false_positive_prop = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(false_positive_prop, probs = lowerCB))),
        true_negative_prop = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(true_negative_prop, probs = lowerCB))),
        false_negative_prop = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(false_negative_prop, probs = lowerCB))),
        sensitivity = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(sensitivity, probs = lowerCB))),
        specificity = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(specificity, probs = lowerCB))),
        PPV = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(PPV, probs = lowerCB))),
        NPV = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(NPV, probs = lowerCB)))
      )
    # upper CB
    tableMODEL_gem <- tableMODEL_gem |> 
      add_row(
        modelname = modelname1,
        outcomename = outcomename1,
        dataset = paste0(dataset1,"1-3 ",upperCB*100,"% band"), # Define number of datasets
        municipality = k,
        n = as.integer(tableMODEL |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(true_positive + false_positive + true_negative + false_negative, probs = upperCB))),
        cases = as.integer(tableMODEL |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(true_positive + false_negative, probs = upperCB))),
        ROCAUC = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(ROCAUC, probs = upperCB))),
        threshold = j,
        true_positive = as.integer(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(true_positive, probs = upperCB))),
        false_positive = as.integer(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(false_positive, probs = upperCB))),
        true_negative = as.integer(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(true_negative, probs = upperCB))),
        false_negative = as.integer(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(false_negative, probs = upperCB))),
        true_positive_prop = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(true_positive_prop, probs = upperCB))),
        false_positive_prop = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(false_positive_prop, probs = upperCB))),
        true_negative_prop = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(true_negative_prop, probs = upperCB))),
        false_negative_prop = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(false_negative_prop, probs = upperCB))),
        sensitivity = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(sensitivity, probs = upperCB))),
        specificity = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(specificity, probs = upperCB))),
        PPV = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(PPV, probs = upperCB))),
        NPV = as.numeric(tableMODEL_gem |> 
          filter(municipality == k & threshold == j) |> 
          summarize(quantile(NPV, probs = upperCB)))
      )
  }
}

## Save tibble
saveRDS(tableMODEL_gem, file = "results/tableMODEL_gem.rds")


#####  #####
