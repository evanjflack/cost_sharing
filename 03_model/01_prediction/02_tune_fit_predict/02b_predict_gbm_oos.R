# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Fits GBM model with optimal hyperparameters in training set and
#.      predicts in both ensemble and main analytic samples

# Start Script -----------------------------------------------------------------

library(lightgbm)
library(Matrix)
library(tidyr)

source("../../../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type='character', default = "20pct"),
  make_option(c("-r", "--resp_var"), type='character', default = "mort"),
  make_option(c("-u", "--population"), type='character', default = "all")
)
unpack_opt(option_list)

# Start log file
file_name <- paste0("log/02b_predict_gbm_oos_", resp_var, "_", population,
                    "_66_90_days_", pct)
start_log_file(file_name)

# Data Read In -----------------------------------------------------------------
message("Reading in data...")
ids_train <- readRDS(paste0(lib_base_data,
                            "ids_66_train_90_days_", pct, ".rds")) %>%
  .[, keep_all := 1]
x_train <- readRDS(paste0(lib_base_data,
                          "gbm_x_66_train_90_days_", pct, ".rds"))
y_train <- unlist(ids_train[, resp_var, with = F])

ids_pred <- readRDS(paste0(lib_base_data,
                           "ids_new_enrollee_90_days_20pct.rds"))
x_pred <- readRDS(paste0(lib_base_data,
                         "gbm_x_new_enrollee_90_days_20pct.rds"))

# Data Prep --------------------------------------------------------------------
message("Prepping prediction matrix...")

# Subset training set to population
keep_var1 <- paste0("keep_", population)
keep_ensemble <- which(ids_train[, keep_var1, with = FALSE] == 1 &
                         ids_train$set == "ensemble")
keep_train <- which(ids_train[, keep_var1, with = FALSE] == 1 &
                      ids_train$set == "train")
y_ensemble <- y_train[keep_ensemble]
x_ensemble <- x_train[keep_ensemble, ]
y_train <- y_train[keep_train]
x_train <- x_train[keep_train, ]

pred_cols <- colnames(x_pred) %>%
  .[. %in% colnames(x_train)]
message(paste(ncol(x_pred) - length(pred_cols),
              "columns dropped from new enrollee matrix",
              "because they were not in dual matrix."))

pred_cols1 <- colnames(x_train) %>%
  .[. %in% pred_cols %>% not()]
message(paste(length(pred_cols1), "columns (of 0s) added to new enrolle matrix",
              "because they were in the dual matrix."))

x_pred <- x_pred[, pred_cols]
x_pred0 <- Matrix(0, nrow = dim(x_pred)[1], ncol = length(pred_cols1),
                  sparse = T)
colnames(x_pred0) <- pred_cols1
x_pred <- cbind(x_pred, x_pred0)
x_pred <- x_pred[, colnames(x_train)]
message(paste("Prediction matrix has", ncol(x_pred), "columns."))

# Tuning Results ---------------------------------------------------------------
message("Identifying best hyperparameters...")
result_path <- paste0(lib_base_data, "tune/gbm_tune_", resp_var, "_",
                      population, "_66_90_days_", "20pct",
                      ".rds")
best_params <- readRDS(result_path)
# Always do at least 500 iterations
best_params$num_iterations <- 500
print(best_params)
# Fit GBM ----------------------------------------------------------------------
message("Fitting gbm...")
gbm_train_data <- lgb.Dataset(data = x_train, label = y_train)

params <- list(
  nrounds = best_params$num_iterations,
  num_leaves = best_params$num_leaves,
  bagging_fraction = best_params$bagging_fraction,
  feature_fraction = best_params$feature_fraction,
  objective = "regression",
  metric = "l2",
  nthread = 5,
  verbose = -1
)

gbm <- lgb.train(data = gbm_train_data,
                 params = params)

# Predict ----------------------------------------------------------------------
new_enrollee_pred <- ids_pred %>%
  .[, .(bene_id)] %>%
  .[, gbm_pred := predict(gbm, x_pred)]

ensemble_pred <- ids_train %>%
  .[keep_ensemble, ] %>%
  .[, .(bene_id, rfrnc_yr)] %>%
  .[, y := y_ensemble] %>%
  .[, gbm_pred := predict(gbm, x_ensemble)]

# Save Model -------------------------------------------------------------------
message("Saving results...")
saveRDS(new_enrollee_pred, paste0(lib_base_data, "gbm_pred_new_enrollee_",
                                  resp_var, "_", population,
                                  "_66_90_days_", pct, ".rds"),
        compress = FALSE)

saveRDS(ensemble_pred, paste0(lib_base_data, "gbm_pred_ensemble_",
                              resp_var, "_", population, "_66_90_days_",
                              pct, ".rds"),
        compress = FALSE)

end_log_file()
