## Load R packages ----
if (!("data.table" %in% installed.packages())){
  install.packages("data.table")
}
if (!("lme4" %in% installed.packages())){
  install.packages("lme4")
}
if (!("glmnet" %in% installed.packages())){
  install.packages("glmnet")
}
if (!("randomForest" %in% installed.packages())){
  install.packages("randomForest")
}
if (!("pROC" %in% installed.packages())){
  install.packages("pROC")
}

library(data.table)
library(lme4) # for random effects
library(randomForest)
library(glmnet)
library(pROC) # for AUC

## Clear environment and Read in helper functions and data ----
rm(list = ls())

source(here::here("code/helper-functions.R"))

train_valid <- data.table::fread(here::here("data/private/train_and_valid_std_data_split_by_site_within_continent.csv")) |>
  factor_cat_predictors() |>
  rm_id_var()

train_valid_without_site <- copy(train_valid)
train_valid_without_site[, sitename := NULL]

train_as_model_matrix <- model.matrix(glasgow_rankin_0_3_30 ~ .,
                                      data = train_valid_without_site)


## Set seed for random forest model ----
set.seed(2025)

## Fit models on training data ----

# to do: try interacting blood pressure variables at baseline with each other, and blood pressure variables at D7 with each other (get error?)
logistic_without_site <- glm(glasgow_rankin_0_3_30 ~ .,
                             data = train_valid_without_site,
                             family = "binomial")

logistic_with_site_rand_intercept <- glmer(glasgow_rankin_0_3_30 ~ . + (1|sitename),
                                           data = train_valid,
                                           family = "binomial")

lasso_cv <- cv.glmnet(x = train_as_model_matrix,
                      y = train_valid$glasgow_rankin_0_3_30,
                      family = "binomial",
                      standardize = FALSE,
                      alpha = 1)
lasso_logistic <- glmnet(x = train_as_model_matrix,
                         y = train_valid$glasgow_rankin_0_3_30,
                         lambda = lasso_cv$lambda.1se,
                         family = "binomial",
                         standardize = FALSE,
                         alpha = 1)

# should sitename be removed?
rf <- randomForest(factor(glasgow_rankin_0_3_30) ~ ., # factor() tells randomForest that outcome is binary
                            data = train_valid,
                            mtry = sqrt(ncol(train_valid) - 1),
                            importance = TRUE)

# rf$importance[, "1"] |>
#   sort(decreasing = TRUE) |>
#   round(digits = 3)
# nihss_randomization      ich_deep_location      gcs_randomization 
# 0.1394448892           0.0596282831           0.0250708712 
# stabct_ivh_volume      stabct_ich_volume               sitename 
# 0.0196552207           0.0089296016           0.0071936616

# different seed
# nihss_randomization      ich_deep_location      gcs_randomization 
# 0.130                  0.062                  0.029 
# stabct_ivh_volume      stabct_ich_volume        Day7NEWscore_BP 
# 0.022                  0.020                  0.007 
# eot_less_15    BaselineNEWscore_BP         site_continent 
# 0.005                  0.005                  0.004
# age_at_consent       D7_Hyperglycemia    Baseline_BP_control 
# 0.003                  0.003                  0.003

rf_logistic <- glm(glasgow_rankin_0_3_30 ~ nihss_randomization + ich_deep_location + gcs_randomization +
                     stabct_ivh_volume + stabct_ich_volume + Day7NEWscore_BP + eot_less_15 + BaselineNEWscore_BP + site_continent +
                     age_at_consent + D7_Hyperglycemia + Baseline_BP_control,
                   data = train_valid_without_site,
                   family = "binomial")


## Get "weights" for predicting patients' status ----

# to do: check signif
sort(coef(logistic_without_site))
coef(lasso_logistic) # [coef(lasso_logistic) != 0] # doesn't have variable names
sort(coef(rf_logistic))


## Predict in test set ----
test <- data.table::fread(here::here("data/private/test_std_data_split_by_site_within_continent.csv")) |>
  factor_cat_predictors() |>
  rm_id_var()

test_without_site <- copy(test)
test_without_site[, sitename := NULL]
test_without_site_hypotension_coded_as_binary <- copy(test_without_site)
test_without_site_hypotension_coded_as_binary[, Baseline_Hypotension := fcase(Baseline_Hypotension %in% c("1", "2"), "1",
                                                                              Baseline_Hypotension == "0", "0")]
test_without_site_hypotension_coded_as_binary_matrix <- model.matrix(glasgow_rankin_0_3_30 ~ .,
                                                                     data = test_without_site_hypotension_coded_as_binary)

# logistic regression
logistic_without_site_pred_prob <- predict(logistic_without_site,
                                           newdata = test_without_site_hypotension_coded_as_binary) # log odds scale
logistic_without_site_preds <- predict(logistic_without_site,
                                       newdata = test_without_site_hypotension_coded_as_binary,
                                       type = "response") # predicted probability scale
hist(logistic_without_site_preds) # histogram of predicted probabilities (most are close to 0)
summary(logistic_without_site_preds)

# PPV for different thresholds
get_ppv(pred_vals = logistic_without_site_preds > 0.11, true_vals = test$glasgow_rankin_0_3_30)
get_ppv(pred_vals = logistic_without_site_preds > 0.5, true_vals = test$glasgow_rankin_0_3_30)

# ROC curve
logistic_without_site_roc <- roc(predictor = logistic_without_site_preds, response = test$glasgow_rankin_0_3_30)
plot(logistic_without_site_roc)
auc(logistic_without_site_roc)

# to do: logistic with random intercept (Error in terms.formula(ff) : '.' in formula and no 'data' argument (maybe the error is because the sitenames are new)
# logistic_with_site_rand_intercept_pred_prob <- predict(logistic_with_site_rand_intercept,
#                                                        newdata = test_without_site)

# to do: LASSO logistic (Error: The number of variables in newx must be 59)
# lasso_pred_prob <- predict(lasso_logistic,
#                            newx = test_without_site_hypotension_coded_as_binary_matrix)

# RF logistic
rf_logistic_pred_prob <- predict(rf_logistic,
                                 newdata = test_without_site_hypotension_coded_as_binary)
rf_logistic_preds <- predict(rf_logistic,
                             newdata = test_without_site_hypotension_coded_as_binary,
                             type = "response")

hist(rf_logistic_preds)
summary(rf_logistic_preds)
get_ppv(pred_vals = rf_logistic_preds > 0.11, true_vals = test$glasgow_rankin_0_3_30)
get_ppv(pred_vals = rf_logistic_preds > 0.5, true_vals = test$glasgow_rankin_0_3_30)

rf_logistic_roc <- roc(predictor = rf_logistic_preds, response = test$glasgow_rankin_0_3_30)
plot(rf_logistic_roc)
auc(rf_logistic_roc)
