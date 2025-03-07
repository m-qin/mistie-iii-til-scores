## Load R packages ----
if (!("haven" %in% installed.packages())){
  install.packages("haven")
}

if (!("data.table" %in% installed.packages())){
  install.packages("data.table")
}

library(data.table)

## Clear environment and Read in .dta files ----
rm(list = ls())
patients <- haven::read_dta(here::here("data/private/M3_data_499_TIL.dta"))
sites <- haven::read_dta(here::here("data/private/M3_TIL_added_data.dta"))

## Extract variables of interest and Merge ----
source(here::here("code/helper-functions.R")) # includes groups of variables

sites_subset <- subset(sites, select = c("new_id", cluster_vars))
patients_subset <- subset(patients, select = c("patientnum_ninds", stratifying_vars, orig_predictor_vars, outcome_var))
data_merged <- merge(sites_subset, patients_subset, by.x = "new_id", by.y = "patientnum_ninds") # "new_id" is kept as a column

## Binarize predictors with rare (i.e., <20) non-zero levels (models cannot predict levels they haven't seen) ----
pred_vars_to_binarize <- c("Baseline_Hypotension",
                           "Baseline_ICP",
                           "Baseline_herniation",
                           "D7_DNR")

for (var in pred_vars_to_binarize){
  data_merged[[paste0("Binarized_", var)]] <- 1 * (data_merged[[var]] > 0) # construct the binarized variable
  data_merged[[var]] <- NULL # delete the old variable
}

## Exclude observations with missing outcome ----
data_analysis <- subset(data_merged, subset = !is.na(data_merged$glasgow_rankin_0_3_30)) # exclude 6 observations with missing outcome

## Save dataset ----
data.table::fwrite(data_analysis, here::here("data/private/data_for_analysis.csv"))


#### If time permits: Stratify by patient's baseline status ----
med_gcs_baseline <- median(data_analysis$gcs_randomization) # 10
data_low_gcs <- data_analysis[data_analysis$gcs_randomization <= med_gcs_baseline, ] # 264 observations
data_high_gcs <- data_analysis[data_analysis$gcs_randomization > med_gcs_baseline, ] # 229 observations
