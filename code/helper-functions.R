## Classes of variables ----

id_var <- "new_id"

cluster_vars <- c("sitename",
                  "site_continent")

outcome_var <- "glasgow_rankin_0_3_30" # 1 if "glasgow_rankin_30" is 0-3 (57 values), 0 if "glasgow_rankin_30" is 4-6 (436 values); 6 NAs

stratifying_vars <- c("age_at_consent", # integers (but treat as continuous), ranging from 28 to 90 (median 62, mean 61), unimodal, very slightly skew left
                      "gcs_randomization", # integers (but treat as continuous), ranging from 3 to 15 (median 10, mean 10.6), bimodal (around 8 and 13); high score is good
                      "nihss_randomization", # integers (but treat as continuous), ranging from 1 to 40 (median 19, mean 19), roughly normal
                      "stabct_ich_volume", # continuous, ranging from 20.9 to 127.1
                      "stabct_ivh_volume", # continuous, ranging from 0 to 61.8
                      "eot_less_15") # binary, 346 values of 0, 148 values of 1; 5 NAs
quant_vars <- stratifying_vars

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
cat_vars <- predictor_vars


## Functions ----

# Function to factor all categorical variables as unordered
factor_cat_predictors <- function(data){
  for (var in cat_vars){
    data[[var]] <- factor(data[[var]], ordered = FALSE)
  }
  return(data)
}

# Function to standardize all quantitative variables according to train/valid dataset
stdize_quant_vars <- function(data, mean_and_SDs_as_data.table){
  if (!data.table::is.data.table(mean_and_SDs_as_data.table)){
    stop("second argument (mean_and_SDs_as_data.table) needs to be a data.table")
  }
  for (var in quant_vars){
    data[[var]] <- (data[[var]] - mean_and_SDs_as_data.table[Variable == var, Mean]) /
      mean_and_SDs_as_data.table[Variable == var, SD]
  }
  return(data)
}

# Function to remove ID variable(s) from dataset
rm_id_var <- function(data, vars_to_rm = "new_id"){
  for (var in vars_to_rm){
    data[[var]] <- NULL
  }
  return(data)
}

# Function to impute missing predictor values
# TO DO

# Function to calculate PPV from predictions and true values
get_ppv <- function(pred_vals, true_vals){
  n_true_pos <- sum(pred_vals * true_vals)
  n_false_pos <- sum(pred_vals * (1-true_vals))
  ppv <- n_true_pos / (n_true_pos + n_false_pos)
  return(ppv)
}
