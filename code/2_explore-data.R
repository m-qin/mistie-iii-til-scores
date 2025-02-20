## Load R packages ----
if (!("data.table" %in% installed.packages())){
  install.packages("data.table")
}

## Read in .csv ----
data_analysis <- data.table::fread(here::here("data/private/data_for_analysis.csv"))

## Explore data ----
# check continent and outcome tables
table(data_analysis$site_continent, useNA = "always") # 395 N America, 84 Europe, 14 Australia-Asia
table(data_analysis$glasgow_rankin_0_3_30) # 436 are 0 (poor patient outcome), only 57 are 1 (good patient outcome)

# # check all variables' missingness, mean, median, range
# summary(data_analysis)

# check GCS vs NIHSS correspondence (seems somewhat correlated but still distinct)
plot(data_analysis$gcs_randomization, data_analysis$nihss_randomization)
cor(data_analysis$gcs_randomization, data_analysis$nihss_randomization) # -0.62

# visualize the continuous random variable distributions
hist(data_analysis$age_at_consent) # unimodal, very slightly skew left
hist(data_analysis$gcs_randomization, main = "Histogram of GCS at randomization", xlab = "GCS at randomization") # bimodal (around 8 and 13)
hist(data_analysis$nihss_randomization) # roughly normal

# check distribution of the only binary predictor
# table(data_analysis$eot_less_15, useNA = "always")
mean(data_analysis$eot_less_15, na.rm = TRUE)
sd(data_analysis$eot_less_15, na.rm = TRUE)

# explore the 4 blood pressure variables
table(data_analysis$Baseline_BP_control, data_analysis$BaselineNEWscore_BP) # ??
table(data_analysis$D7_BP_control, data_analysis$Day7NEWscore_BP) # ??
table(data_analysis$Baseline_Hypotension, data_analysis$D7_Hypotension)
table(data_analysis$Baseline_Hyperpyrexia, data_analysis$D7_Hyperpyrexia)

## Check correlations ----
# note: the code below only makes sense for continuous predictors; need to handle categorical variables (factor them in R)
cor_matrix <- cor(subset(data_analysis, select = predictor_vars),
                  use = "pairwise.complete.obs")
View(cor_matrix)

## Check missingness ----
# verify that variables with 12 NAs are missing the same observations (yes)
# missing_indices <- list()
# for (var in colnames(data_analysis)){
#   if (sum(is.na(data_analysis[[var]])) == 12){
#     missing_indices[[var]] <- which(is.na(data_analysis[[var]]))
#   }
# }

# # check number of missing values in each predictor (mostly 0, Baseline_ICP and Baseline_herniation have 1, eot_less_15, 8 have the same 12 missing)
# missing_indices <- list()
# for (var in colnames(data_analysis)){
#   missing_indices[[var]] <- sum(is.na(data_analysis[[var]]))
# }
# View(missing_indices)

missing_d7 <- data_analysis[which(is.na(data_analysis$Day7NEWscore_BP)), ]
missing_d7$sitename # looks random, not systematic --> okay to impute during modeling
