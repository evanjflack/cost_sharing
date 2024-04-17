# Proj: Cost-sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Estimates demand response measures similar to those found in other
#       papers (for appendix table E1)

# Start Script -----------------------------------------------------------------

library(estimatr)
library(foreach)

source("../../../00_pre_process/start_script.R")

option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct")
)
unpack_opt(option_list)

start_log_file("log/11a_estimate_lit_demand")

# Read in Data -----------------------------------------------------------------

model_vars <- c("bene_id", "first_mo", "pred_cut1",
                "rfrnc_yr", "race", "sex", "cntrct_pbp_rfrnc_yr")
DT_model <- fread(paste0(lib_base_data, "revision_model_data_", pct,
                         ".csv")) %>%
  .[, model_vars, with = FALSE]
rm(model_vars)

co_rates <- fread(paste0(lib_base_data, "sample_co_rates_all_", pct,
                         ".csv"))

claims_by_mo <- fread(paste0(lib_base_data,
                             "part_d_claims_pill_days_mo_new_enrollee_",
                             pct, ".csv")) %>%
  .[, .(bene_id, claims_1_12)]

atc_ind <- fread(paste0(lib_base_data, "dec_year1_atc_days_new_enrollee_",
                         pct, ".csv")) %>%
  .[bene_id %chin% DT_model$bene_id, ] %>%
  .[, c("bene_id", paste0("atc3_", c("C10A", "C07A", "C09C", "C09A"), c("_days_1_12")))] %>%
  setnames(names(.), gsub("atc3_", "", names(.))) %>%
  setnames(names(.), gsub("_days_1_12", "", names(.))) %>%
  .[, any_statin := ifelse(C10A > 0, 1, 0)] %>%
  .[, any_beta_blocker := ifelse(C07A > 0, 1, 0)] %>%
  .[, any_ace_arb := ifelse(C09C + C09A > 0, 1, 0)] %>%
  .[, .(bene_id, any_statin, any_beta_blocker, any_ace_arb)]

DT_model %<>%
  merge(co_rates, by = "bene_id") %>%
  merge(claims_by_mo, by = "bene_id") %>%
  merge(atc_ind, by = "bene_id")

DT_model %<>%
  .[pred_cut1 == 2, ]

# Estimate Demand Response -----------------------------------------------------

grid <- data.table(yvar = c("claims_1_12", "any_statin", "any_beta_blocker",
                               "any_ace_arb"),
                   xvar = c("price", "price_statin", "price_beta_blocker",
                            "price_ace_arb"))

dt_fit <- foreach(yvar = grid$yvar,
                  xvar = grid$xvar,
                  .combine = "rbind") %do%
  {
  DT_fit <- copy(DT_model) %>%
    .[, y := get(yvar)] %>%
    .[, x := get(xvar)]

  fit <- iv_robust(y ~ x | first_mo,
                    data = DT_fit, se_type = "stata")

  dt_fit1 <- fit_to_dt(fit, "x") %>%
    .[, yvar := yvar] %>%
    .[, xvar := xvar] %>%
    .[, .(yvar, xvar, estimate, std.error, p.value)]
  }

# Export -----------------------------------------------------------------------

fwrite(dt_fit, paste0(lib_base_data, "lit_demand_estimates.csv"))

end_log_file()
