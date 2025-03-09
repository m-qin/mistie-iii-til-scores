## Load R packages ----
if (!("data.table" %in% installed.packages())){
  install.packages("data.table")
}
if (!("glmnet" %in% installed.packages())){
  install.packages("glmnet")
}
if (!("pROC" %in% installed.packages())){
  install.packages("pROC")
}

library(data.table)
library(glmnet) # for LASSO
library(pROC) # for AUC


## Clear environment, Read in helper functions and data ----
rm(list = ls())

source(here::here("code/helper-functions.R"))

train_valid <- data.table::fread(here::here("data/private/train_and_valid_std_data_split_by_site_within_continent.csv")) |>
  factor_cat_predictors() |>
  rm_id_var()
train_valid[, sitename := NULL]
train_valid[, glasgow_rankin_0_3_30 := NULL]

train_as_model_matrix <- model.matrix(glasgow_rankin_0_3_365 ~ .,
                                      data = train_valid)


## Set up results folder ----
if (!dir.exists(here::here("results/Day365MRS/Weights/log-odds-scale"))){
  dir.create(here::here("results/Day365MRS/Weights/log-odds-scale"), recursive = TRUE)
}

if (!dir.exists(here::here("results/Day365MRS/Weights/probability-scale"))){
  dir.create(here::here("results/Day365MRS/Weights/probability-scale"), recursive = TRUE)
}

if (!dir.exists(here::here("results/Day365MRS/ROC-curves"))){
  dir.create(here::here("results/Day365MRS/ROC-curves"), recursive = TRUE)
}

if (!dir.exists(here::here("results/Day365MRS/PPVs"))){
  dir.create(here::here("results/Day365MRS/PPVs"), recursive = TRUE)
}


## Set seed for cross-validation (LASSO) ----
set.seed(2025)

## Fit models on training data ----

# Logistic regression
logistic <- glm(glasgow_rankin_0_3_365 ~ .,
                 data = train_valid,
                 family = "binomial")

# LASSO logistic regression
lasso_logistic_cv <- cv.glmnet(x = train_as_model_matrix,
                      y = train_valid$glasgow_rankin_0_3_365,
                      family = "binomial",
                      standardize = FALSE,
                      alpha = 1)
lasso_logistic <- glmnet(x = train_as_model_matrix,
                         y = train_valid$glasgow_rankin_0_3_365,
                         lambda = lasso_logistic_cv$lambda.1se,
                         family = "binomial",
                         standardize = FALSE,
                         alpha = 1)

# # Linear regression
# linear <- lm(glasgow_rankin_0_3_365 ~ .,
#              data = train_valid)

# LASSO linear regression
lasso_linear_cv <- cv.glmnet(x = train_as_model_matrix,
                            y = train_valid$glasgow_rankin_0_3_365,
                            standardize = FALSE,
                            alpha = 1)
lasso_linear <- glmnet(x = train_as_model_matrix,
                       y = train_valid$glasgow_rankin_0_3_365,
                       lambda = lasso_linear_cv$lambda.1se,
                       standardize = FALSE,
                       alpha = 1)

# Bi-directional stepwise logistic regression
intercept_only_logistic <- glm(glasgow_rankin_0_3_365 ~ 1,
                              data = train_valid,
                              family = "binomial")
all_logistic <- glm(glasgow_rankin_0_3_365 ~ .,
                      data = train_valid,
                      family = "binomial")
step_logistic <- step(intercept_only_logistic, direction='both', scope=formula(all_logistic), trace=0)

# Bi-directional stepwise linear regression
intercept_only_linear <- glm(glasgow_rankin_0_3_365 ~ 1,
                            data = train_valid)
all_linear <- glm(glasgow_rankin_0_3_365 ~ .,
                  data = train_valid)
step_linear <- step(intercept_only_linear, direction='both', scope=formula(all_linear), trace=0)


## Get "Weights" for predicting patients' status ----

# logistic
logistic_all_coefs_as_matrix <- summary(logistic)$coefficients
logistic_all_coefs <- data.table(Variable = rownames(logistic_all_coefs_as_matrix)) |>
  cbind(logistic_all_coefs_as_matrix)
fwrite(logistic_all_coefs, here::here("results/Day365MRS/Weights/log-odds-scale/logistic-model.csv"))

# lasso logistic
# coef(lasso_logistic)
lasso_logistic_coefs <- data.table(Variable = rownames(coef(lasso_logistic)),
                                   Coefficient = as.vector(coef(lasso_logistic)))
lasso_logistic_nonzero_coefs <- lasso_logistic_coefs[Coefficient != 0]
fwrite(lasso_logistic_nonzero_coefs, here::here("results/Day365MRS/Weights/log-odds-scale/lasso-logistic-model.csv"))

# lasso linear
# coef(lasso_linear)
lasso_linear_coefs <- data.table(Variable = rownames(coef(lasso_linear)),
                                   Coefficient = as.vector(coef(lasso_linear)))
lasso_linear_nonzero_coefs <- lasso_linear_coefs[Coefficient != 0]
fwrite(lasso_linear_nonzero_coefs, here::here("results/Day365MRS/Weights/probability-scale/lasso-linear-model.csv"))

# bi-directional stepwise logistic
# step_logistic$anova
step_logistic_all_coefs_as_matrix <- summary(step_logistic)$coefficients
step_logistic_all_coefs <- data.table(Variable = rownames(step_logistic_all_coefs_as_matrix)) |>
  cbind(step_logistic_all_coefs_as_matrix)
fwrite(step_logistic_all_coefs, here::here("results/Day365MRS/Weights/log-odds-scale/step-logistic-model.csv"))

# bi-directional stepwise linear
# step_linear$anova
step_linear_all_coefs_as_matrix <- summary(step_linear)$coefficients
step_linear_all_coefs <- data.table(Variable = rownames(step_linear_all_coefs_as_matrix)) |>
  cbind(step_linear_all_coefs_as_matrix)
fwrite(step_linear_all_coefs, here::here("results/Day365MRS/Weights/probability-scale/step-linear-model.csv"))


## Predict on test set, get ROC curves and AUC ----
test <- data.table::fread(here::here("data/private/test_std_data_split_by_site_within_continent.csv")) |>
  factor_cat_predictors(ref_data = train_valid) |>
  rm_id_var()
test[, sitename := NULL]
test[, glasgow_rankin_0_3_30 := NULL]

test_matrix <- model.matrix(glasgow_rankin_0_3_365 ~ .,
                            data = test)

predicted_probs_on_test <- function(model){
  if ("glmnet" %in% class(model)){ # for LASSO models
    return(as.vector(predict(model, newx = test_matrix, type = "response")))
  } else{
    return(predict(model, newdata = test, type = "response"))
  }
}
roc_on_test <- function(probs){
  return(roc(predictor = probs, response = test$glasgow_rankin_0_3_365))
}
auc_on_test <- function(probs){
  return(roc_on_test(probs) |>
    auc() |>
    round(4))
}
save_roc <- function(probs, filepath, title){
  png(here::here(paste0("results/Day365MRS/ROC-curves/", filepath, ".png")))
  plot(roc_on_test(probs), main = paste0(title, ", AUC =", auc_on_test(probs)))
  dev.off()
  return(TRUE) # indicates successful save
}
save_roc_on_test <- function(model, filepath, title){
  probs <- predicted_probs_on_test(model)
  save_roc(probs, filepath, title)
  return(TRUE) # indicates successful save
}
save_ppv_on_test <- function(model, filepath, cutoff){
  ppv <- get_ppv(cutoff,
                 predicted_probs_on_test(model),
                 test$glasgow_rankin_0_3_365) |>
    round(4)
  ppv_as_dt <- data.table(Model = filepath,
                          Cutoff = cutoff,
                          PPV = ppv)
  fwrite(ppv_as_dt, here::here(paste0("results/Day365MRS/PPVs/", filepath)))
  return(TRUE) # indicates successful save
}

save_roc_on_test(logistic, "logistic-model", "Logistic Regression")
save_roc_on_test(lasso_logistic, "lasso-logistic-model", "LASSO Logistic Regression")
save_roc_on_test(lasso_linear, "lasso-linear-model", "LASSO Linear Regression")
save_roc_on_test(step_logistic, "step-logistic-model", "Stepwise Selection Logistic Regression")
save_roc_on_test(step_linear, "step-linear-model", "Stepwise Selection Linear Regression")

preval_in_train <- mean(train_valid$glasgow_rankin_0_3_365) # 0.4530387 is the prevalence in the training data; like an empirical Bayes prior here
save_ppv_on_test(cutoff = preval_in_train, logistic, "logistic-model")
save_ppv_on_test(cutoff = preval_in_train, lasso_logistic, "lasso-logistic-model")
save_ppv_on_test(cutoff = preval_in_train, lasso_linear, "lasso-linear-model")
save_ppv_on_test(cutoff = preval_in_train, step_logistic, "step-logistic-model")
save_ppv_on_test(cutoff = preval_in_train, step_linear, "step-linear-model")

# combine PPV csvs into 1 csv
if (file.exists(here::here("results/Day365MRS/PPVs/all-model-PPVs.csv"))){
  file.remove(here::here("results/Day365MRS/PPVs/all-model-PPVs.csv"))
}
ppv_files <- paste0(here::here("results/Day365MRS/PPVs/"),
                    list.files(here::here("results/Day365MRS/PPVs/")))
ppv_table <- lapply(ppv_files, fread) |>
  rbindlist()
file.remove(ppv_files)
fwrite(ppv_table, here::here("results/Day365MRS/PPVs/all-model-PPVs.csv"))
