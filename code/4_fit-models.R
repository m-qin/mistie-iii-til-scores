## Load R packages and helper functions ----
if (!("data.table" %in% installed.packages())){
  install.packages("data.table")
}

## Clear environment and Read in functions and data ----
rm(list = ls())

source(here::here("code/helper-functions.R"))

train_valid <- data.table::fread(here::here("data/private/train_and_valid_std_data.csv")) |>
  factor_predictors() |>
  rm_vars(id_var)
# to do: impute missing predictor values




