# Proj: Cost-sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Ensembles GBM and Lasso models via no-intercept OLS in ensemble sample,
#       then predicts ensemble model in main analytic sample

# Start Script -----------------------------------------------------------------
library(Matrix)
source("../../../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type='character', default = "20pct"),
  make_option(c("-r", "--resp_var"), type='character', default = "mort"),
  make_option(c("-u", "--population"), type='character', default = "all")
)
unpack_opt(option_list)

# Start log file
file_name <- paste0("log/03_predict_ensemble_oos_", resp_var, "_", population,
                    "_66_90_days_", pct)
start_log_file(file_name)

# Data Read In -----------------------------------------------------------------
message("Reading in data...")
ids_pred <- readRDS(paste0(lib_base_data, "ids_new_enrollee_90_days_", pct,
                           ".rds")) %>%
  .[, keep_all := 1]

lasso_pred <- readRDS(paste0(lib_base_data, "lasso_pred_new_enrollee_",
                             resp_var, "_", population,
                             "_66_90_days_", pct, ".rds"))

gbm_pred <- readRDS(paste0(lib_base_data, "gbm_pred_new_enrollee_",
                           resp_var, "_", population,
                           "_66_90_days_", pct, ".rds"))

lasso_ensemble <- readRDS(paste0(lib_base_data, "lasso_pred_ensemble_",
                             resp_var, "_", population,
                             "_66_90_days_", pct, ".rds"))

gbm_ensemble <- readRDS(paste0(lib_base_data, "gbm_pred_ensemble_",
                           resp_var, "_", population, "_66_90_days_",
                           pct, ".rds")) %>%
  .[, y := NULL]


dt_ensemble <- lasso_ensemble %>%
  merge(gbm_ensemble, on = .(bene_id, rfrnc_yr))

fit <- lm(y ~ lasso_pred + gbm_pred - 1, data = dt_ensemble)

w_lasso <- fit$coefficients[1]
w_gbm <- fit$coefficients[2]

print(fit$coefficients)

pred_subscores <- ids_pred %>%
  merge(lasso_pred, by = "bene_id") %>%
  merge(gbm_pred, by = "bene_id")

# Predict Ensemble -------------------------------------------------------------
message("Predicting Ensemble...")
pred_scores <- pred_subscores %>%
  .[, ensemble_pred := lasso_pred*w_lasso + gbm_pred*w_gbm]

# Save Results -----------------------------------------------------------------
message("Saving results...")
saveRDS(pred_scores, paste0(lib_base_data,
                            "ensemble_pred_new_enrollee_dual_false_", resp_var,
                            "_", population, "_66_90_days_", pct, ".rds"),
        compress = FALSE)

end_log_file()
