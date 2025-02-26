## Load R packages ----
if (!("data.table" %in% installed.packages())){
  install.packages("data.table")
}

# library(data.table)

## Clear environment and Read in .csv ----
rm(list = ls())
data_analysis <- data.table::fread(here::here("data/private/data_for_analysis.csv")) # is a data.table

## Set seed and split data ----
set.seed(643)

# randomly sample 62 sites (approx 80% of 78 total sites) for train and validation dataset
train_valid_sites <- sample(data_analysis$sitename, size = 62, replace = FALSE)
train_valid <- data_analysis[sitename %in% train_valid_sites]
test <- data_analysis[!(sitename %in% train_valid_sites)]

# # check sample size and outcome prevalence in train/validation dataset and test dataset
# nrow(train_valid) # 319 (35.3%)
# nrow(test) # 174 (64.7%)
# prop.table(table(train_valid$glasgow_rankin_0_3_30)) # 11.3%
# prop.table(table(test$glasgow_rankin_0_3_30)) # 12.1%

## Save UNSTANDARDIZED train/valid and test datasets ----
data.table::fwrite(train_valid, here::here("data/private/train_and_valid_UNSTD_data_split_by_site.csv"))
data.table::fwrite(test, here::here("data/private/test_UNSTD_data_split_by_site.csv"))
