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

library(lme4) # for random effects
library(randomForest)
library(glmnet)

## Clear environment and Read in helper functions and data ----
rm(list = ls())

source(here::here("code/helper-functions.R"))

train_valid <- data.table::fread(here::here("data/private/train_and_valid_std_data_split_by_site_within_continent.csv")) |>
  factor_cat_predictors() |>
  rm_id_var()

train_as_model_matrix <- model.matrix(glasgow_rankin_0_3_30 ~ . -sitename,
                                      data = train_valid)


## Set seed for random forest model ----
set.seed(2025)

## Fit models on training data ----

logistic_without_site <- glm(glasgow_rankin_0_3_30 ~ . -sitename,
                             data = train_valid,
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

rf_logistic <- randomForest(factor(glasgow_rankin_0_3_30) ~ ., # factor() tells randomForest that outcome is binary
                            data = train_valid,
                            mtry = sqrt(ncol(train_valid) - 1),
                            importance = TRUE)


## Get "weights" for predicting patients' status ----

# to do: check signif
sort(coef(logistic_without_site))

sort(rf_logistic$importance[, "1"], decreasing = TRUE)
# nihss_randomization      ich_deep_location      gcs_randomization 
# 0.1394448892           0.0596282831           0.0250708712 
# stabct_ivh_volume      stabct_ich_volume               sitename 
# 0.0196552207           0.0089296016           0.0071936616





