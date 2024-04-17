# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Formats table to compare demand estimates to other papers
#       (appendix table E1)

# Start Script -----------------------------------------------------------------

library(xtable)

source("../../../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct")
)
unpack_opt(option_list)

# Start log file
start_log_file("log/11b_format_lit_demand_table")

# Read In Estimates ------------------------------------------------------------

dt_est <- fread(paste0(lib_base_data, "lit_demand_estimates.csv"))

model_vars <- c("bene_id", "first_mo", "pred_cut1",
                "rfrnc_yr", "race", "sex", "cntrct_pbp_rfrnc_yr")
DT_model <- fread(paste0(lib_base_data, "revision_model_data_", pct,
                         ".csv")) %>%
  .[, model_vars, with = FALSE]
rm(model_vars)

co_rates <- fread(paste0(lib_base_data, "sample_co_rates_all_", pct,
                         ".csv"))

DT_model %<>%
  merge(co_rates, by = "bene_id")

cost_wide <- fread(paste0(lib_base_data, "sample_spending_by_month_", pct,
                          ".csv")) %>%
  .[, .(bene_id, cum_cost_1_12, cum_plan_oop_cost_1_12)]

DT_model %<>%
  merge(cost_wide, by = "bene_id")

icl_oopt <- fread(paste0(lib_base_data, "icl_oopt_std_amts_sample.csv"))

DT_model %<>%
  merge(icl_oopt, by = "rfrnc_yr")

# Chandra, Gruber, McNight (AER, 2010) -----------------------------------------

# Dep Var: # of Claims, Endog Var: Price ($)

# Our estimate
our_est_cgm <- dt_est[yvar == "claims_1_12" &
                        xvar == "price"] %>%
  .[, .(yvar, estimate, std.error)] %>%
  .[, `:=`(lb = estimate - 1.96*std.error, ub = estimate + 1.96*std.error)] %>%
  .[, lapply(.SD, round, digits = 4), by = yvar] %>%
  .[, our_estimate := paste0(estimate, " (", lb, ", ", ub, ")")] %>%
  .[, our_estimate]

# CGM Estimate (plus ours)
dt_est_cgm <- data.table(copay_ppo = 7.75,
                         util_ppo = -.111,
                         copay_hmo = 6.74,
                         util_hmo = -.261) %>%
  .[, iv_est_ppo := util_ppo/copay_ppo] %>%
  .[, iv_est_hmo := util_hmo/copay_hmo] %>%
  .[, our_est := our_est_cgm] %>%
  .[, outcome := "\\hspace{5mm} Number of claims"] %>%
  .[, .(outcome, our_est_cgm, iv_est_ppo, iv_est_hmo)]



# Class Specific (Choudhry et al, NEJM, 2011; Einave et al. AEJ, 2018 ----------

# Our Estimates
dt_est_class <- dt_est %>%
  .[yvar %chin% c("any_statin", "any_ace_arb", "any_beta_blocker")] %>%
  .[, .(yvar, estimate, std.error)] %>%
  .[, `:=`(lb = estimate - 1.96*std.error, ub = estimate + 1.96*std.error)] %>%
  .[, lapply(.SD, round, digits = 4), by = yvar] %>%
  .[, our_estimate := paste0(estimate, " (", lb, ", ", ub, ")")] %>%
  .[, .(yvar, our_estimate)]

# Choudhry
dt_est_choudhry <- data.table(yvar = c("any_ace_arb", "any_beta_blocker",
                                       "any_statin"),
                              copay = c(13.35, 12.64, 24.98),
                              diff_adh = c(-.056, -.044, -.062),
                              diff_fill = c(-.058, -.04, -.055)) %>%
  .[, iv_est_adh := diff_adh/copay] %>%
  .[, iv_est_fill := diff_fill/copay] %>%
  .[, lapply(.SD, round, digits = 4), by = yvar] %>%
  .[, .(yvar, iv_est_fill)] %>%
  setnames("iv_est_fill", "choudhry_est")

# Einav et al.
# Convert elasticity to derivative (multiply by Q/P)
DT_model %<>%
  .[, pre := ifelse(cum_cost_1_12 <= icl_amt, 1, 0)] %>%
  .[, `:=`(icl = ifelse(cum_cost_1_12 > icl_amt &
                          cum_plan_oop_cost_1_12 <= oopt_amt, 1, 0),
           cat = ifelse(cum_plan_oop_cost_1_12 > oopt_amt, 1, 0))] %>%
  .[, arm := ifelse(icl == 1, "icl", ifelse(cat == 1, "cat", "pre"))]


# Statins
base_price_statin <- mean(DT_model[rfrnc_yr <= 2009 & arm == "pre", price_statin])
base_util_statin <- .3
elasticity_statin <- -.23
iv_est_statin <- elasticity_statin*base_util_statin/base_price_statin

# BEta blockers
base_price_bb <- mean(DT_model[rfrnc_yr <= 2009 & arm == "pre", price_beta_blocker])
base_util_bb <- .22
elasticity_bb <- -.14
iv_est_bb <- elasticity_bb*base_util_bb/base_price_bb

dt_est_einav <- data.table(yvar = c("any_statin", "any_beta_blocker"),
                           einav_est = c(iv_est_statin,
                                         iv_est_bb)) %>%
  .[, einav_est := round(einav_est, 4)]



dt_est_choudhry_einav <- dt_est_class %>%
  merge(dt_est_choudhry, by = "yvar", all.x = T) %>%
  merge(dt_est_einav, by = "yvar", all.x = T) %>%
  .[, yvar := factor(yvar, levels = c("any_ace_arb", "any_beta_blocker",
                                         "any_statin"),
                        labels = c("ACE or ARB", "Beta Blockers", "Statins"))] %>%
  .[order(yvar), ] %>%
  .[, yvar := paste0("\\hspace{5mm} ", yvar)]


# Print Latex Tables -----------------------------------------------------------
message("Printng latex tables...\n")
# CGM
print(xtable(dt_est_cgm,
             caption = "Comparison to Choudry et al.", label = "choudry",
             align = c("l", "l", rep("c", 3)), digits = 4),
      include.rownames = FALSE, caption.placement = "top",
      sanitize.text.function = force)

message("\n")

# Class
print(xtable(dt_est_choudhry_einav,
             caption = "Comparison to Choudry et al.", label = "choudry",
             align = c("l", "l", rep("c", 3)), digits = 4),
      include.rownames = FALSE, caption.placement = "top",
      sanitize.text.function = force)

# End --------------------------------------------------------------------------
end_log_file()
