## Load R packages ----
if (!("data.table" %in% installed.packages())){
  install.packages("data.table")
}

library(data.table)

## Clear environment and Read in functions and data ----
rm(list = ls())

source(here::here("code/helper-functions.R"))

quant_vars_mean_and_SD <- data.table::fread(here::here("data/private/quant_vars_mean_and_SD_train_split_by_site_within_continent.csv"))
train_valid <- data.table::fread(here::here("data/private/train_and_valid_UNSTD_data_split_by_site_within_continent.csv"))
test <- data.table::fread(here::here("data/private/test_UNSTD_data_split_by_site_within_continent.csv"))

## Remove patients with any missing covariate data ----
train_valid <- train_valid[complete.cases(train_valid)]
test <- test[complete.cases(test), ]

## Standardize quantitative variables in train/valid AND test datasets according to TRAIN/VALID data ----
train_valid_std <- stdize_quant_vars(train_valid, quant_vars_mean_and_SD)
test_std <- stdize_quant_vars(test, quant_vars_mean_and_SD)

## Save STANDARDIZED datasets ----
data.table::fwrite(train_valid_std, here::here("data/private/train_and_valid_std_data_split_by_site_within_continent.csv"))
data.table::fwrite(test_std, here::here("data/private/test_std_data_split_by_site_within_continent.csv"))
