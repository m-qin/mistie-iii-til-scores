
# read in .dta file
data <- haven::read_dta(here::here("data/private/M3_data_499_TIL.dta"))

# clean data
data$studyname <- NULL # everything is "MISTIE III"
data$modified_itt <- NULL # everything is 1
data$var30 <- NULL # everything is NA
data$var31 <- NULL # everything is NA
data$var32 <- NULL # everything is NA

# "mis" column is the MISTIE treatment (250 treated, 249 control=standard of care)

# to do: more cleaning

# maybe put in another R script: explore data
# table(data$ischemic_stroke)
# 0   1 
# 426  73

# to do: save cleaned dataset