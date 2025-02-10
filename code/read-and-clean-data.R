
# Read in .dta file ----
data <- haven::read_dta(here::here("data/private/M3_data_499_TIL.dta"))

# Clean data ----
data$studyname <- NULL # everything is "MISTIE III"
data$modified_itt <- NULL # everything is 1
# hmm maybe these are issues with reading in the data
data$var30 <- NULL # everything is NA
data$var31 <- NULL # everything is NA
data$var32 <- NULL # everything is NA
data$til_dnr_dni_order_baseline <- NULL # everything is NA because inclusion criteria
data$mis <- NULL # redundant with "treatment_group" column; "mis" = "MISTIE" treatment = "surgical" in treatment_group
data$glasgow_rankin_0_3_30 <- NULL # binarized (incorrectly?) from data$glasgow_rankin_30
data$glasgow_rankin_0_3_180 <- NULL # binarized (incorrectly?) from data$glasgow_rankin_180
data$glasgow_rankin_0_3_365 <- NULL # binarized (incorrectly?) from data$glasgow_rankin_365

# to do: more cleaning

# Explore data (maybe put in another R script) ----

# table(data$ischemic_stroke)
# 0   1 
# 426  73

# note: I forget what the "til_", "D1score", and "D7score" prefixes mean

# table(data$til_dnr_dni_order_d7)
# No Yes 
# 20 457  22 

# table(data$D7_DNR)
# 0   1   2   3 # what do 2 and 3 mean?
# 489   4   2   4 

# Save cleaned dataset (to do) ----
