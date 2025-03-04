## Load R packages and helper functions ----
if (!("data.table" %in% installed.packages())){
  install.packages("data.table")
}

## Clear environment and Read in functions and data ----
rm(list = ls())

source(here::here("code/helper-functions.R"))

train_valid <- data.table::fread(here::here("data/private/train_and_valid_std_data_split_by_site_within_continent.csv")) |>
  factor_cat_predictors() |>
  rm_id_var()
# to do: impute missing predictor values

## Set seed for random forest model ----
set.seed(2025)

## Fit models ----

# logistic_model <- glm()

# logistic_model_with_site_rand_eff


