# Proj: Cost Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Tunes LASSO for lambda in training set

# Start Script -----------------------------------------------------------------
library(glmnet)
library(foreach)
library(doParallel)
source("../../../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type='character', default = "20pct"),
  make_option(c("-r", "--resp_var"), type='character', default = "mort"),
  make_option(c("-u", "--population"), type='character',
              default = "all")
)
unpack_opt(option_list)

# Start log file
file_name <- paste0("log/01a_tune_lasso_", resp_var, "_",
                    population, "_66_90_days_", pct)
start_log_file(file_name)

# Data Read In -----------------------------------------------------------------
message("Reading in data...")
# Sparse matrix with features (sorted in same order as ids)
x <- readRDS(paste0(lib_base_data, "lasso_x_66_train_90_days_",  pct, ".rds"))
# data.table of set/fold assignment, keep variables, and response variables
ids <- readRDS(paste0(lib_base_data, "ids_66_train_90_days_", pct, ".rds")) %>%
  .[, keep_all := 1]

# Subset Data ------------------------------------------------------------------
message("Subsetting data...")
keep_var1 <- paste0("keep_", population)
keep_train <- which(ids[, keep_var1, with = FALSE] == 1 &
                      ids$set == "train")
y <- unlist(ids[keep_train, resp_var, with = F])
x<- x[keep_train, ]

# Tune Lasso -------------------------------------------------------------------
message("Tuning lasso...")
registerDoParallel(cores = uniqueN(ids[keep_train, fold]))
tuning_result <- cv.glmnet(x = x,
                           y = y,
                           foldid = ids[keep_train, fold],
                           parallel = TRUE,
                           alpha = 1,
                           type.measure = "mse")
stopImplicitCluster()

# Save Model -------------------------------------------------------------------
message("Saving results...")
result_path <- paste0(lib_base_data, "tune/lasso_fit_", resp_var, "_",
                      population, "_66_90_days_", pct, ".rds")
saveRDS(tuning_result, result_path, compress = FALSE)

end_log_file()
