
# Read in .dta file ----
data <- haven::read_dta(here::here("data/private/M3_data_499_TIL.dta"))

# Extract variables of interest ----
stratifying_vars <- c("age_at_consent", # integers (but treat as continuous), ranging from 28 to 90 (median 62, mean 61)
                      "gcs_randomization", # integers (but treat as continuous), ranging from 3 to 15 (median 10, mean 10.6)
                      "nihss_randomization", # integers (but treat as continuous), ranging from 1 to 40 (median 19, mean 19)
                      "stabct_ich_volume", # continuous, ranging from 20.9 to 127.1
                      "stabct_ivh_volume", # continuous, ranging from 0 to 61.8
                      "eot_less_15") # binary, 346 values of 0, 148 values of 1; 5 NAs

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

outcome_var <- "glasgow_rankin_0_3_30" # 1 if "glasgow_rankin_30" is 0-3 (57 values), 0 if "glasgow_rankin_30" is 4-6 (436 values); 6 NAs

data_subset <- subset(data, select = c(stratifying_vars, predictor_vars, outcome_var))

## Explore data ----
table(data_subset$glasgow_rankin_0_3_30, useNA = "always") # 436 are 0 (poor patient outcome), only 57 are 1 (good patient outcome); 6 NAs
plot(data_subset$gcs_randomization, data_subset$nihss_randomization)
cor(data_subset$gcs_randomization, data_subset$nihss_randomization) # -0.62

hist(data_subset$age_at_consent) # unimodal, very slightly skew left
hist(data_subset$gcs_randomization) # bimodal (around 8 and 13)
hist(data_subset$nihss_randomization) # roughly normal

table(data_subset$Baseline_BP_control, data_subset$BaselineNEWscore_BP) # ??
table(data_subset$D7_BP_control, data_subset$Day7NEWscore_BP) # ??
table(data_subset$Baseline_Hypotension, data_subset$D7_Hypotension)
table(data_subset$Baseline_Hyperpyrexia, data_subset$D7_Hyperpyrexia)

summary(data_subset)
cor_matrix <- cor(data_subset, use = "pairwise.complete.obs")
View(cor_matrix)
