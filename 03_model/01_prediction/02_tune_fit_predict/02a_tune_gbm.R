# Proj: Cost Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Tunes GBM over several hyperparameters in the training set

# Start Script -----------------------------------------------------------------
library(lightgbm)
library(foreach)
library(doParallel)
library(tidyr)
library(yaml)
library(purrr)
library(furrr)
library(Matrix)
library(future)

source("../../../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type='character', default = "20pct"),
  make_option(c("-r", "--resp_var"), type='character', default = "mort"),
  make_option(c("-u", "--population"), type='character',
              default = "all")
)
unpack_opt(option_list)

# Additional user inputs
# Increase exportable size max
options(future.globals.maxSize = 1500*1024^2)

# Start log file
file_name <- paste0("log/02a_tune_gbm_", resp_var, "_",
                    population, "_66_90_days_", pct)
start_log_file(file_name)

# Data Read In -----------------------------------------------------------------
message("Reading in data...")
# Sparse matrix with features (sorted in same order as ids)
x <- readRDS(paste0(lib_base_data, "gbm_x_66_train_90_days_", pct, ".rds"))
# data.table of set/fold assignment, keep variables, and response variables
ids <- readRDS(paste0(lib_base_data, "ids_66_train_90_days_", pct, ".rds")) %>%
  .[, keep_all := 1]

# Subset Data ------------------------------------------------------------------
message("Subsetting data...")
# Indicies of training sample for specified population/prediction category
keep_var1 <- paste0("keep_", population)
keep_train <- which(ids[, keep_var1, with = FALSE] == 1 &
                      ids$set == "train")
y <- unlist(ids[keep_train, resp_var, with = F])
x <- x[keep_train, ]

# Tuning Design ----------------------------------------------------------------
message("Preparing tuning grid...")
# .yaml file of hyperparameters
config <- read_yaml("../model_config/hyper_params.yaml")

# Makes data.frame of all combinations of hyperparameters
hyperparameter_grid <- do.call(crossing, config$gbm)

cv_params <- list(
  objective = "regression",
  metric = "l2",
  feature_pre_filter=FALSE
)

lgb_train <- lgb.Dataset(data = x, label = y)

best_params <- NULL
min_loss <- Inf
for (i in 1:nrow(hyperparameter_grid)) {
  print(i)
  current_params <- hyperparameter_grid[i, ]
  current_params <- as.list(current_params)

  cv_results <- lgb.cv(
    params = c(cv_params, current_params),
    data = lgb_train,
    nfold = 5,
    early_stopping_rounds = 10,
    stratified = TRUE,
    verbose = -1
  )

  current_min_loss <- min(cv_results$best_score)
  if (current_min_loss < min_loss) {
    min_loss <- current_min_loss
    best_params <- current_params
  }
}

print(best_params)

# Save Results -----------------------------------------------------------------
message("Saving results...")
result_path <- paste0(lib_base_data, "tune/gbm_tune_", resp_var, "_",
                      population, "_66_90_days_", pct,
                      ".rds")
saveRDS(best_params, result_path, compress = FALSE)

end_log_file()
