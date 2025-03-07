## Load R packages and helper functions ----
if (!("data.table" %in% installed.packages())){
  install.packages("data.table")
}

library(data.table)

## Clear environment and Read in functions and data ----
rm(list = ls())

source(here::here("code/helper-functions.R"))

test <- data.table::fread(here::here("data/private/test_std_data_split_by_site_within_continent.csv")) |>
  factor_cat_predictors() |>
  rm_id_var()

## Load models ----


## Set up data.table to compare results of different models ----
results <- data.table(Model = c("Logistic Regression without Site",
                                "Logistic Regression with Site as Random Effect",
                                "LASSO-penalized Logistic Regression"),
                      PPV_on_test = c(0))

## Calculate PPV on test data ----
# get_ppv()


