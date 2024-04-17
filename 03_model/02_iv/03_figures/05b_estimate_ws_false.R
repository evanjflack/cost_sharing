# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Estimates reduced form of monthly mortality on enrollment month in the
#       main sample but after December year 1

# Start Script -----------------------------------------------------------------

source("../../../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct")
)
unpack_opt(option_list)

library(estimatr)
library(tidyr)
library(broom)
library(foreach)
library(doParallel)
library(stringr)

# Start log file
start_log_file("log/05b_estimate_ws_false")

# Read In Data -----------------------------------------------------------------
message("Reading in data...")

# Monthly mortality outcomes (mort_1 = mortality in june year 1, mort 2 =
# mortality in july year 2... mort 39 = mortaloity in dec year 4)

mort_outcomes <- fread(paste0(lib_base_data, "mort_outcomes_jun_year_1_", pct,
                              ".csv"))

# Main sample
sample_vars <- c("bene_id", "rfrnc_yr", "first_mo", "race", "sex",
                 "initial_cost_90", "cntrct_pbp_rfrnc_yr")
sample <- fread(paste0(lib_base_data, "revision_model_data_", pct,
                       ".csv")) %>%
  .[, sample_vars, with = FALSE]
rm(sample_vars)



# Prep Data --------------------------------------------------------------------
message("Prepping data...")

# Merge variables
DT_model <- sample %>%
  merge(mort_outcomes, by = "bene_id")
rm(sample, mort_outcomes)

# Define the initial spending variable
DT_model %<>%
  .[, spend_pred := initial_cost_90]

# Estimate RF ------------------------------------------------------------------
message("Estimating falsification RF...")

dt_fit <- foreach(month = seq(8, 43),
                  .combine = "rbind") %do%
  {
    print(month)

    # Prep Data ----------------------------------------------------------------
    DT_fit <- copy(DT_model)
    # If the outcome period is past Dec Year 2, remove 2012 enrollees
    if (month > 19) {
      DT_fit %<>%
        .[rfrnc_yr <= 2011, ]
    }
    # If outcome period is past Dec year 3, remove 2011 enrollees
    if (month > 31) {
      DT_fit %<>%
        .[rfrnc_yr <= 2010]
    }
    # Define initial spending bins
    DT_fit %<>%
      .[, pred_cut1 := ifelse(spend_pred <= quantile(spend_pred, .7),
                              1, ifelse(spend_pred <=
                                          quantile(spend_pred, .97), 2, 3)),
        by = first_mo]

    # Define outcome and subset to people alive
    DT_fit %<>%
      .[, y := get(paste0("jun_mort_", month))] %>%
      .[get(paste0("jun_alive_", month)) == 1, ]

    # Number of observations
    obs <- nrow(DT_fit)

    # RF by Initial Spending ---------------------------------------------------
    # Baseline mortality
    base_mort_pred <- calc_cmean(DT_fit, "y", "pred_cut1") %>%
      .[, variable := NULL] %>%
      .[, pred_cut1 := as.character(pred_cut1)]

    # Reduced form
    fit_pred1 <- lm_robust(y ~ first_mo:factor(pred_cut1) + factor(pred_cut1) +
                            factor(race) + factor(sex),
                          data = DT_fit,
                          se_type = "stata",
                          fixed_effects = ~ cntrct_pbp_rfrnc_yr)

    dt_fit_pred1 <- fit_to_dt(fit_pred1, "first_mo", "pred_cut1") %>%
      .[, `:=`(type = "pred", year_cut = "2007-2012", inst = "first_mo")] %>%
      merge(base_mort_pred, by = "pred_cut1")

    # Combine estimates
    dt_fit1 %<>%
      .[, .(inst, type, term, year_cut, pred_cut1, mean, estimate, std.error,
            p.value, lb, ub)] %>%
      .[, `:=`(month = month, obs = obs)]
  }

# Export -----------------------------------------------------------------------
message("Exporting...")

fwrite(dt_fit, paste0(lib_base_data, "ws_false_estimates_", pct,
                      ".csv"))

end_log_file()
