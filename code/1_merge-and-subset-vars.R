## Load R packages ----
if (!("haven" %in% installed.packages())){
  install.packages("haven")
}

if (!("data.table" %in% installed.packages())){
  install.packages("data.table")
}

## Clear environment and Read in .dta files ----
rm(list = ls())
patients <- haven::read_dta(here::here("data/private/M3_data_499_TIL.dta"))
sites <- haven::read_dta(here::here("data/private/M3_TIL_added_data.dta"))

## Extract variables of interest and Merge ----
cluster_vars <- c("sitename",
                  "site_continent")

outcome_var <- "glasgow_rankin_0_3_30" # 1 if "glasgow_rankin_30" is 0-3 (57 values), 0 if "glasgow_rankin_30" is 4-6 (436 values); 6 NAs

stratifying_vars <- c("age_at_consent", # integers (but treat as continuous), ranging from 28 to 90 (median 62, mean 61), unimodal, very slightly skew left
                      "gcs_randomization", # integers (but treat as continuous), ranging from 3 to 15 (median 10, mean 10.6), bimodal (around 8 and 13); high score is good
                      "nihss_randomization", # integers (but treat as continuous), ranging from 1 to 40 (median 19, mean 19), roughly normal
                      "stabct_ich_volume", # continuous, ranging from 20.9 to 127.1
                      "stabct_ivh_volume", # continuous, ranging from 0 to 61.8
                      "eot_less_15") # binary, 346 values of 0, 148 values of 1; 5 NAs

# Note: These are all categorical; we will treat them as unordered factors in our models, even though they are generally ordered in increasing severity.
predictor_vars <- c("BaselineNEWscore_BP", # ordinal categorical variable (values of 0, 1, 2, 3, 4, 5)
                    "Day7NEWscore_BP",     # ordinal categorical variable (values of 0, 1, 2, 3, 4, 5); 13 NAs
                    "Baseline_BP_control", # to do: treat as factor (4 levels)
                    "D7_BP_control", # to do: treat as factor (4 levels); 13 NAs
                    "Baseline_Hypotension", # 480 values of 0, 17 values of 1, 2 values of 2
                    "D7_Hypotension", # 480 values of 0, 29 values of 1, 13 values of 2; 13 NAs
                    "Baseline_Hyperpyrexia", # values of 0, 1, 2, 3; 13 NAs
                    "Baseline_Hyperglycemia", # values of 0, 1, 2, 3
                    "Baseline_ICP", # 487 values of 0, 8 values of 1, 1 value of 2, 2 values of 3; 1 NA
                    "Baseline_herniation", # 484 values of 0, 8 values of 1, 6 values of 2; 1 NA
                    "Baseline_INR", # 466 values of 0, 24 values of 1, 9 values of 2
                    "D7_Hyperpyrexia", # values of 0, 1, 2, 3; 13 NAs
                    "D7_Hyperglycemia", # values of 0, 1, 2, 3; 13 NAs
                    "D7_ICP", # 454 values of 0, 15 values of 1, 9 values of 2, 7 values of 3, 1 value of 4; 13 NAs
                    "D7_herniation", # 474 values of 0, 6 values of 1, 6 values of 2; 13 NAs
                    "D7_INR", # 462 values of 0, 13 values of 1, 11 values of 2; 13 NAs
                    "D7_DNR") # 489 values of 0, 4 values of 1, 2 values of 2, 4 values of 3

sites_subset <- subset(sites, select = c("new_id", cluster_vars))
patients_subset <- subset(patients, select = c("patientnum_ninds", stratifying_vars, predictor_vars, outcome_var))
data_merged <- merge(sites_subset, patients_subset, by.x = "new_id", by.y = "patientnum_ninds") # "new_id" is kept as a column

## Exclude observations with missing outcome ----
data_analysis <- subset(data_merged, subset = !is.na(data_merged$glasgow_rankin_0_3_30)) # exclude 6 observations with missing outcome

## Save dataset ----
data.table::fwrite(data_analysis, here::here("data/private/data_for_analysis.csv"))


#### If time permits: Stratify by patient's baseline status ----
med_gcs_baseline <- median(data_analysis$gcs_randomization) # 10
data_low_gcs <- data_analysis[data_analysis$gcs_randomization <= med_gcs_baseline, ] # 264 observations
data_high_gcs <- data_analysis[data_analysis$gcs_randomization > med_gcs_baseline, ] # 229 observations
