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

# randomly sample 62 sites (approx 80% of 78 total sites) for train and validation dataset, WITHIN CONTINENT GROUPS
sites <- unique(data_analysis[, .(sitename, site_continent)])
train_valid_sites <- dplyr::slice_sample(sites, by = site_continent, prop = 0.8)
train_valid <- data_analysis[sitename %in% train_valid_sites$sitename]
test <- data_analysis[!(sitename %in% train_valid_sites$sitename)]
rm(sites, train_valid_sites)

# # check site continent, sample size, outcome prevalence in train/validation dataset and test dataset
# table(train_valid$site_continent) # Australia-Asia, Europe, N Am - 12, 65, 302
# table(test$site_continent) # Australia-Asia, Europe, N Am - 2 (that's small), 19, 93
# prop.table(table(train_valid$site_continent)) # Australia-Asia, Europe, N Am - 3.2%, 17.2%, 79.7%
# prop.table(table(test$site_continent)) # Australia-Asia, Europe, N Am - 1.8%, 16.7%, 81.6% (compared to train, more Europe and less N America)
# nrow(train_valid) # 379 (23.1%)
# nrow(test) # 114 (76.9%)
# prop.table(table(train_valid$glasgow_rankin_0_3_30)) # 11.6%
# prop.table(table(test$glasgow_rankin_0_3_30)) # 11.4%

## Save UNSTANDARDIZED train/valid and test datasets ----
data.table::fwrite(train_valid, here::here("data/private/train_and_valid_UNSTD_data_split_by_site_within_continent.csv"))
data.table::fwrite(test, here::here("data/private/test_UNSTD_data_split_by_site_within_continent.csv"))
