## Load R packages and helper functions ----
if (!("data.table" %in% installed.packages())){
  install.packages("data.table")
}

## Clear environment and Read in functions and data ----
rm(list = ls())

source(here::here("code/helper-functions.R"))

test <- data.table::fread(here::here("data/private/test_std_data_split_by_site_within_continent.csv")) |>
  factor_cat_predictors() |>
  rm_id_var()
# to do: impute missing predictor values
