## Load R packages ----
if (!("data.table" %in% installed.packages())){
  install.packages("data.table")
}

library(data.table)

## Clear environment and Read in functions and data ----
rm(list = ls())

source(here::here("code/helper-functions.R"))

train_valid <- data.table::fread(here::here("data/private/train_and_valid_UNSTD_data.csv"))


## Get mean and SD of quantitative variables FROM TRAIN/VALID DATA ----
quant_vars_mean_and_SD <- data.table::data.table(Variable = quant_vars,
                                                 Mean = 0,
                                                 SD = 0)

# to do: consider imputing missing values, if there are any
for (var in quant_vars){
  quant_vars_mean_and_SD[Variable == var, `:=`(Mean = mean(train_valid[[var]], na.rm = TRUE),
                                               SD = sd(train_valid[[var]], na.rm = TRUE))]

}

## Save mean and standard deviation for quantitative variables ----
data.table::fwrite(quant_vars_mean_and_SD, here::here("data/private/quant_vars_mean_and_SD.csv"))


## Do the same for ALTERNATIVE train-test split by site ----
train_valid_split_by_site <- data.table::fread(here::here("data/private/train_and_valid_UNSTD_data_split_by_site.csv"))
for (var in quant_vars){
  quant_vars_mean_and_SD[Variable == var, `:=`(Mean = mean(train_valid_split_by_site[[var]], na.rm = TRUE),
                                               SD = sd(train_valid_split_by_site[[var]], na.rm = TRUE))]
  
}
data.table::fwrite(quant_vars_mean_and_SD, here::here("data/private/quant_vars_mean_and_SD_train_split_by_site.csv"))
