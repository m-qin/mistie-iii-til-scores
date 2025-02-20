
# Read in .dta file ----
data <- haven::read_dta(here::here("data/private/M3_data_499_TIL.dta"))

# Extract variables of interest ----
stratifying_vars <- c("age_at_consent",
                      "gcs_randomization", # check
                      "nihss_randomization", # check
                      "stabct_ich_volume",
                      "stabct_ivh_volume",
                      "eot_less_15")
predictor_vars <- c("BaselineNEWscore_BP", # check
                    "Day7NEWscore_BP",     # check
                    "Baseline_BP_control", # check
                    "D7_BP_control", # check
                    "Baseline_Hypotension",
                    "D7_Hypotension",
                    "Baseline_Hyperpyrexia",
                    "Baseline_Hyperglycemia",
                    "Baseline_ICP",
                    "Baseline_herniation",
                    "Baseline_INR",
                    "D7_Hyperpyrexia",
                    "D7_Hyperglycemia",
                    "D7_ICP",
                    "D7_herniation",
                    "D7_INR", 
                    "D7_DNR")
outcome_var <- "glasgow_rankin_0_3_30" # 1 if "glasgow_rankin_30" is 0-3, 0 if "glasgow_rankin_30" is 4-6; 10 NAs

data_subset <- subset(data, select = c(stratifying_vars, predictor_vars, outcome_var))

## Explore data ----
table(data_subset$glasgow_rankin_0_3_30) # 436 are 0 (poor patient outcome), only 57 are 1 (good patient outcome)!
plot(data_subset$gcs_randomization, data_subset$nihss_randomization)
table(data_subset$Baseline_BP_control, data_subset$BaselineNEWscore_BP) # ??
table(data_subset$D7_BP_control, data_subset$Day7NEWscore_BP) # ??

summary(data_subset)
cor_matrix <- cor(data_subset, use = "pairwise.complete.obs")
View(cor_matrix)
