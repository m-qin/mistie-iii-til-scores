
# read in .dta file
data <- haven::read_dta(here::here("data/private/M3_data_499_TIL.dta"))

# clean data
data$studyname <- NULL # everything is "MISTIE III"
data$var30 <- NULL # everything is NA
data$var31 <- NULL # everything is NA
data$var32 <- NULL # everything is NA

