## Load R packages ----
if (!("data.table" %in% installed.packages())){
  install.packages("data.table")
}

library(data.table)

## Clear environment and Read in functions and data ----
rm(list = ls())

source(here::here("code/helper-functions.R"))

quant_vars_mean_and_SD <- data.table::fread(here::here("data/private/quant_vars_mean_and_SD.csv"))
train_valid <- data.table::fread(here::here("data/private/train_and_valid_UNSTD_data.csv"))
test <- data.table::fread(here::here("data/private/test_UNSTD_data.csv"))


## Standardize quantitative variables in train/valid AND test datasets according to TRAIN/VALID data ----
train_valid_std <- stdize_quant_vars(train_valid, quant_vars_mean_and_SD)
test_std <- stdize_quant_vars(test, quant_vars_mean_and_SD)

## Save STANDARDIZED datasets ----
data.table::fwrite(train_valid_std, here::here("data/private/train_and_valid_std_data.csv"))
data.table::fwrite(test_std, here::here("data/private/test_std_data.csv"))
