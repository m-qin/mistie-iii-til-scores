## Load R packages ----
if (!("data.table" %in% installed.packages())){
  install.packages("data.table")
}

## Clear environment and Read in .csv ----
rm(list = ls())
data_analysis <- data.table::fread(here::here("data/private/data_for_analysis.csv"))

## Set seed and split data ----
set.seed(643)

# randomly sample 400 patients for train and validation dataset, 93 patients for test dataset, keeping outcome prevalence constant
positive_indices <- which(data_analysis$glasgow_rankin_0_3_30 == 1)
negative_indices <- which(data_analysis$glasgow_rankin_0_3_30 == 0)
positive_train_valid <- sample(positive_indices, size = 46, replace = F)
negative_train_valid <- sample(negative_indices, size = 354, replace = F)
positive_test <- positive_indices[!positive_indices %in% positive_train_valid]
negative_test <- negative_indices[!negative_indices %in% negative_train_valid]

# randomize the order of patients (hopefully doesn't matter for any models though)
train_valid_indices <- sample(c(positive_train_valid, negative_train_valid), size = 400, replace = F)
test_indices <- sample(c(positive_test, negative_test), size = 93, replace = F)
train_valid <- data_analysis[train_valid_indices, ]
test <- data_analysis[test_indices, ]

## Standardize quantitative variables according to training data ----
quant_vars <- c("age_at_consent", # integers (but treat as continuous), ranging from 28 to 90 (median 62, mean 61), unimodal, very slightly skew left
                "gcs_randomization", # integers (but treat as continuous), ranging from 3 to 15 (median 10, mean 10.6), bimodal (around 8 and 13); high score is good
                "nihss_randomization", # integers (but treat as continuous), ranging from 1 to 40 (median 19, mean 19), roughly normal
                "stabct_ich_volume", # continuous, ranging from 20.9 to 127.1
                "stabct_ivh_volume") # continuous, ranging from 0 to 61.8

for (var in quant_vars){
  var_mean <- mean(train_valid[[var]])
  var_sd <- sd(train_valid[[var]])
  train_valid[[var]] <- (train_valid[[var]] - var_mean) / var_sd
  test[[var]] <- (test[[var]] - var_mean) / var_sd
}

## Save datasets ----
data.table::fwrite(train_valid, here::here("data/private/train_and_valid_std_data.csv"))
data.table::fwrite(test, here::here("data/private/test_std_data.csv"))
