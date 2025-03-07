## Load R packages ----
if (!("data.table" %in% installed.packages())){
  install.packages("data.table")
}
# if (!("lme4" %in% installed.packages())){
#   install.packages("lme4")
# }
# if (!("gee" %in% installed.packages())){
#   install.packages("gee")
# }
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
# library(lme4) # for random effects
# library(gee)
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

# tried interacting blood pressure variables at baseline with each other, and blood pressure variables at D7 with each other, but get error or NAs?
# Warning message:
# glm.fit: fitted probabilities numerically 0 or 1 occurred 
logistic_without_site <- glm(glasgow_rankin_0_3_30 ~ .,
                             data = train_valid_without_site,
                             family = "binomial")

# # doesn't work for predicting at a new site
# logistic_with_site_rand_intercept <- glmer(glasgow_rankin_0_3_30 ~ . + (1|sitename),
#                                            data = train_valid,
#                                            family = "binomial")

# # original error from including all predictors: rank-deficient model matrix
# # Cgee: error: logistic model for probability has fitted value very close to 1.
# estimates diverging; iteration terminated.
# gee_exchangeable <- gee(formula = glasgow_rankin_0_3_30 ~ nihss_randomization + ich_deep_location + gcs_randomization + stabct_ivh_volume + stabct_ich_volume + age_at_consent,
#                         id = factor(sitename),
#                         data = train_valid,
#                         family = "binomial",
#                         corstr = "exchangeable")

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

ridge_cv <- cv.glmnet(x = train_as_model_matrix,
                      y = train_valid$glasgow_rankin_0_3_30,
                      family = "binomial",
                      standardize = FALSE,
                      alpha = 0)
ridge_logistic <- glmnet(x = train_as_model_matrix,
                         y = train_valid$glasgow_rankin_0_3_30,
                         lambda = lasso_cv$lambda.1se,
                         family = "binomial",
                         standardize = FALSE,
                         alpha = 0)

rf <- randomForest(factor(glasgow_rankin_0_3_30) ~ ., # factor() tells randomForest that outcome is binary
                            data = train_valid_without_site, # to be fair against other models (and realize that sitename may not be helpful for predicting at a new site)
                            mtry = sqrt(ncol(train_valid) - 1),
                            importance = TRUE)

# rf$importance[, "1"] |>
#   sort(decreasing = TRUE) |>
#   round(digits = 3)

# printing the results that were > 0
# nihss_randomization              ich_deep_location 
# 0.144                          0.063 
# gcs_randomization              stabct_ivh_volume 
# 0.030                          0.027 
# stabct_ich_volume                 age_at_consent 
# 0.014                          0.011 
# D7_Hyperglycemia            Baseline_BP_control 
# 0.006                          0.005 
# eot_less_15            BaselineNEWscore_BP 
# 0.005                          0.002 
# site_continent          Baseline_Hyperpyrexia 
# 0.001                          0.001 

# Warning message:
# glm.fit: fitted probabilities numerically 0 or 1 occurred 
rf_logistic <- glm(glasgow_rankin_0_3_30 ~ nihss_randomization + ich_deep_location + gcs_randomization + stabct_ivh_volume + stabct_ich_volume + age_at_consent + # variable importance > 0.1
                     D7_Hyperglycemia + Baseline_BP_control + eot_less_15 + BaselineNEWscore_BP + site_continent + Baseline_Hyperpyrexia, # variable importance > 0
                   data = train_valid_without_site,
                   family = "binomial")

rf_logistic0.1 <- glm(glasgow_rankin_0_3_30 ~ nihss_randomization + ich_deep_location + gcs_randomization + stabct_ivh_volume + stabct_ich_volume + age_at_consent, # variable importance > 0.1
                   data = train_valid_without_site,
                   family = "binomial")

linear_without_site <- lm(glasgow_rankin_0_3_30 ~ .,
                           data = train_valid_without_site)

lasso_linear_cv <- cv.glmnet(x = train_as_model_matrix,
                      y = train_valid$glasgow_rankin_0_3_30,
                      standardize = FALSE,
                      alpha = 1)
lasso_linear <- glmnet(x = train_as_model_matrix,
                         y = train_valid$glasgow_rankin_0_3_30,
                         lambda = lasso_cv$lambda.1se,
                         standardize = FALSE,
                         alpha = 1)

# bi-directional stepwise logistic
intercept_only <- glm(glasgow_rankin_0_3_30 ~ 1,
                      data = train_valid_without_site,
                      family = "binomial")
all <- glm(glasgow_rankin_0_3_30 ~ .,
                      data = train_valid_without_site,
                      family = "binomial")
both <- step(intercept_only, direction='both', scope=formula(all), trace=0)

# bi-directional stepwise linear
intercept_only_linear <- glm(glasgow_rankin_0_3_30 ~ 1,
                      data = train_valid_without_site)
all_linear <- glm(glasgow_rankin_0_3_30 ~ .,
           data = train_valid_without_site)
both_linear <- step(intercept_only_linear, direction='both', scope=formula(all_linear), trace=0)

## Get "weights" for predicting patients' status ----

# logistic
logistic_all_coefs <- summary(logistic_without_site)$coefficients |>
  as.data.table() # oops, no variable names
logistic_nonzero_coefs <- logistic_all_coefs[`Pr(>|z|)` <= 0.05]
sort(coef(logistic_without_site))

# lasso
coef(lasso_logistic) # [coef(lasso_logistic) != 0] # doesn't have variable names
lasso_nonzero_coefs <- data.table(Variable = c("Intercept", "age_at_consent", "nihss_randomization", "stabct_ich_volume", "stabct_ivh_volume", "ich_deep_location", "Day7NEWscore_BP1"),
                                  Coefficient = c(-2.43388827, -0.15972434, -1.36584301, -0.29351679, -0.04133074, -0.53945129, 0.02983276))

# ridge
coef(ridge_logistic) # can compare with LASSO results

# random forest
sort(coef(rf_logistic))

# lasso linear
coef(lasso_linear)

# bi-directional stepwise
both$anova
summary(both)


## Predict in test set ----
test <- data.table::fread(here::here("data/private/test_std_data_split_by_site_within_continent.csv")) |>
  factor_cat_predictors(ref_data = train_valid) |> # oops, this should've been done with the levels in the training data
  rm_id_var()

test_without_site <- copy(test)
test_without_site[, sitename := NULL]

test_without_site_matrix <- model.matrix(glasgow_rankin_0_3_30 ~ .,
                                        data = test_without_site)

## Logistic regression
logistic_without_site_pred_prob <- predict(logistic_without_site,
                                           newdata = test_without_site) # log odds scale
logistic_without_site_preds <- predict(logistic_without_site,
                                       newdata = test_without_site,
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


## LASSO logistic
lasso_probs <- predict(lasso_logistic,
                       newx = test_without_site_matrix)
lasso_preds <- predict(lasso_logistic,
                         newx = test_without_site_matrix,
                         type = "response") |>
  as.vector()

hist(lasso_preds) # histogram of predicted probabilities (most are close to 0)
summary(lasso_preds)

# PPV for different thresholds
get_ppv(pred_vals = lasso_preds > 0.11, true_vals = test$glasgow_rankin_0_3_30)
get_ppv(pred_vals = lasso_preds > 0.5, true_vals = test$glasgow_rankin_0_3_30)

# ROC curve
lasso_roc <- roc(predictor = lasso_preds, response = test$glasgow_rankin_0_3_30)
plot(lasso_roc)
auc(lasso_roc)


## Ridge logistic
ridge_probs <- predict(ridge_logistic,
                       newx = test_without_site_matrix)
ridge_preds <- predict(ridge_logistic,
                       newx = test_without_site_matrix,
                       type = "response") |>
  as.vector()

hist(ridge_preds) # histogram of predicted probabilities (most are close to 0)
summary(ridge_preds)

# PPV for different thresholds
get_ppv(pred_vals = ridge_preds > 0.11, true_vals = test$glasgow_rankin_0_3_30)
get_ppv(pred_vals = ridge_preds > 0.5, true_vals = test$glasgow_rankin_0_3_30)

# ROC curve
ridge_roc <- roc(predictor = ridge_preds, response = test$glasgow_rankin_0_3_30)
plot(ridge_roc)
auc(ridge_roc)


## RF logistic
rf_logistic_pred_prob <- predict(rf_logistic,
                                 newdata = test_without_site)
rf_logistic_preds <- predict(rf_logistic,
                             newdata = test_without_site,
                             type = "response")

hist(rf_logistic_preds)
summary(rf_logistic_preds)
get_ppv(pred_vals = rf_logistic_preds > 0.11, true_vals = test$glasgow_rankin_0_3_30)
get_ppv(pred_vals = rf_logistic_preds > 0.5, true_vals = test$glasgow_rankin_0_3_30)

rf_logistic_roc <- roc(predictor = rf_logistic_preds, response = test$glasgow_rankin_0_3_30)
plot(rf_logistic_roc)
auc(rf_logistic_roc)

rf_logistic0.1_log_odds <- predict(rf_logistic0.1,
                                 newdata = test_without_site)
rf_logistic0.1_probs <- predict(rf_logistic0.1,
                             newdata = test_without_site,
                             type = "response")

hist(rf_logistic0.1_probs)
summary(rf_logistic0.1_probs)
get_ppv(pred_vals = rf_logistic0.1_probs > 0.11, true_vals = test$glasgow_rankin_0_3_30)
get_ppv(pred_vals = rf_logistic0.1_probs > 0.5, true_vals = test$glasgow_rankin_0_3_30)

rf_logistic0.1_roc <- roc(predictor = rf_logistic0.1_probs, response = test$glasgow_rankin_0_3_30)
plot(rf_logistic0.1_roc)
auc(rf_logistic0.1_roc)

## Linear regression
linear_probs <- predict(linear_without_site,
                          newdata = test_without_site) # predicted probability scale
hist(linear_probs) # histogram of predicted probabilities (most are close to 0)
summary(linear_probs)

# PPV for different thresholds
get_ppv(pred_vals = linear_probs > 0.11, true_vals = test$glasgow_rankin_0_3_30)
get_ppv(pred_vals = linear_probs > 0.5, true_vals = test$glasgow_rankin_0_3_30)

# ROC curve
linear_without_site_roc <- roc(predictor = linear_probs, response = test$glasgow_rankin_0_3_30)
plot(linear_without_site_roc)
auc(linear_without_site_roc)

## LASSO linear
lasso_linear_probs <- predict(lasso_linear,
                       newx = test_without_site_matrix) |>
  as.vector()

hist(lasso_linear_probs) # histogram of predicted probabilities (most are close to 0)
summary(lasso_linear_probs)

# PPV for different thresholds
get_ppv(pred_vals = lasso_linear_probs > 0.11, true_vals = test$glasgow_rankin_0_3_30)
get_ppv(pred_vals = lasso_linear_probs > 0.5, true_vals = test$glasgow_rankin_0_3_30)

# ROC curve
lasso_linear_roc <- roc(predictor = lasso_linear_probs, response = test$glasgow_rankin_0_3_30)
plot(lasso_linear_roc, main = "LASSO linear regression")
auc(lasso_linear_roc)

## Bi-directional stepwise
both_step_log_odds <- predict(both,
                              newdata = test_without_site) # log odds scale
both_step_probs <- predict(both,
                           newdata = test_without_site,
                           type = "response") # predicted probability scale
hist(both_step_probs) # histogram of predicted probabilities (most are close to 0)
summary(both_step_probs)

# PPV for different thresholds
get_ppv(pred_vals = both_step_probs > 0.11, true_vals = test$glasgow_rankin_0_3_30)
get_ppv(pred_vals = both_step_probs > 0.5, true_vals = test$glasgow_rankin_0_3_30)

# ROC curve
both_step_roc <- roc(predictor = both_step_probs, response = test$glasgow_rankin_0_3_30)
plot(both_step_roc)
auc(both_step_roc)

## Bi-directional stepwise linear regression
both_step_probs_linear <- predict(both_linear,
                           newdata = test_without_site,
                           type = "response") # predicted probability scale
hist(both_step_probs_linear) # histogram of predicted probabilities (most are close to 0)
summary(both_step_probs_linear)

# PPV for different thresholds
get_ppv(pred_vals = both_step_probs_linear > 0.11, true_vals = test$glasgow_rankin_0_3_30)
get_ppv(pred_vals = both_step_probs_linear > 0.5, true_vals = test$glasgow_rankin_0_3_30)

# ROC curve
both_step_linear_roc <- roc(predictor = both_step_probs_linear, response = test$glasgow_rankin_0_3_30)
plot(both_step_linear_roc)
auc(both_step_linear_roc)



## TO DO: Write helper functions for all of the above

## Scratchwork ----
test_only_weighted_vars <- copy(test)
test_only_weighted_vars[, `:=`(Intercept = 1,
                               Day7NEWscore_BP1 = 1 * (Day7NEWscore_BP == "1"))]
test_only_weighted_vars <- subset(test_only_weighted_vars, select = lasso_nonzero_coefs$Variable)
preds <- as.matrix(test_only_weighted_vars) %*% lasso_nonzero_coefs$Coefficient
head(preds)
head(expit(preds))
