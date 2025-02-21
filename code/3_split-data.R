## Load R packages ----
if (!("data.table" %in% installed.packages())){
  install.packages("data.table")
}

library(data.table)

## Read in .csv ----
data_analysis <- fread(here::here("data/private/data_for_analysis.csv"))

## Set seed and split data 20%/80% within regions ----
set.seed(643)



