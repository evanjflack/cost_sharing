# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Estimates regressions for figure 4 (RF estimates over time)

# Start Script -----------------------------------------------------------------
library(estimatr)
library(iterators)
library(foreach)
library(stringr)

source("../../../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct")
)
unpack_opt(option_list)

# Start log file
start_log_file("log/04a_estimate_year_1_2_gap_util_mort_rf")

# Read In Data -----------------------------------------------------------------

initial_spending <- fread(paste0(lib_base_data,
                                 "initial_part_d_spending_new_enrollee_", pct,
                                 ".csv"))

mort_outcomes <- fread(paste0(lib_base_data, "mort_outcomes_jun_year_1_", pct,
                              ".csv")) %>%
  .[, c("bene_id", paste0("jun_mort_", seq(1, 19)))] %>%
  setnames(c("bene_id", paste0("mort_", seq(6, 24)))) %>%
  .[, c("bene_id", paste0("mort_", seq(6, 24))), with = FALSE]


icl_oopt <- fread(paste0(lib_base_data, "icl_oopt_std_amts_sample.csv"))

claims_mo <- fread(paste0(lib_base_data,
                          "part_d_claims_pill_days_mo_new_enrollee_",
                          pct, ".csv")) %>%
  .[, c("bene_id", paste0("pill_days_",
                          c("1_8", "1_9", "1_10", "1_11", "1_12",
                            "2_1", "2_2", "2_3", "2_4", "2_5"))), with = FALSE] %>%
  setnames(c("bene_id", paste0("pill_days_", seq(8, 17))))

cost_wide <- fread(paste0(lib_base_data, "sample_spending_by_month_", pct,
                          ".csv")) %>%
  .[, c("bene_id", paste0("cum_cost_1_", seq(8, 12)),
        paste0("cum_cost_2_", seq(1, 5)),
        paste0("cum_plan_oop_cost_1_", seq(8, 12)),
        paste0("cum_plan_oop_cost_2_", seq(1, 5))), with = FALSE] %>%
  setnames(c(paste0("cum_cost_1_", seq(8, 12)),
             paste0("cum_cost_2_", seq(1, 5))),
           paste0("cum_cost_", seq(8, 17))) %>%
  setnames(c(paste0("cum_plan_oop_cost_1_", seq(8, 12)),
             paste0("cum_plan_oop_cost_2_", seq(1, 5))),
           paste0("cum_plan_oop_cost_", seq(8, 17)))

# Monthly Mortality ------------------------------------------------------------
message("Estimating reduced form...")

instrument <- "first_mo"
month <- 9
dt_fit <- foreach(month = seq(8, 17),
                  .combine = "rbind") %do%
  {

    message(month)

    sample_month <- ifelse(month <= 12, month, 12)
    model_vars <- c("bene_id", "rfrnc_yr", "first_mo", "race", "sex", "cntrct",
                    "pbp")

    if (month < 12) {
      DT_fit <- fread(paste0(lib_base_data, "new_enrollee_sample_month_",
                             sample_month, "_", pct, ".csv")) %>%
        .[, model_vars, with = FALSE]
    } else {
      DT_fit <- fread(paste0(lib_base_data, "revision_model_data_", pct,
                             ".csv")) %>%
        .[, model_vars, with = FALSE]
    }

  DT_fit %<>%
      merge(initial_spending, by = "bene_id") %>%
      merge(mort_outcomes, by = "bene_id") %>%
      merge(claims_mo[, c("bene_id", paste0("pill_days_", month)),
                      with = FALSE], by = "bene_id") %>%
      merge(cost_wide[, c("bene_id", paste0("cum_cost_", month),
                          paste0("cum_plan_oop_cost_", month)),
                      with = FALSE], by = "bene_id")


    # Remove those that die in year 2 (before month)
    if (month >= 13) {
      DT_fit %<>%
        .[, pre_cum_mort := ifelse(rowSums(.SD) > 0, 1, 0),
          .SDcols = paste0("mort_", seq(12, month - 1))] %>%
        .[pre_cum_mort == 0]
    }

    max_first_mo <- month - 3
    DT_fit %<>%
      .[first_mo <= max_first_mo] %>%
      .[, spend_pred := get(paste0("initial_cost_", 90))] %>%
      .[, pred_cut1 := ifelse(spend_pred <= quantile(spend_pred, .7),
                              1, ifelse(spend_pred <=
                                          quantile(spend_pred, .97), 2, 3)),
        by = first_mo] %>%
      .[, cntrct_pbp_rfrnc_yr := paste(cntrct, pbp, rfrnc_yr, sep = "_")]

    DT_fit %<>%
      merge(icl_oopt, by = "rfrnc_yr") %>%
      .[, cum_cost := get(paste0("cum_cost_", month))] %>%
      .[, plan_cost := get(paste0("cum_plan_oop_cost_", month))] %>%
      .[, paste0("arm_", month) := ifelse(cum_cost <= icl_amt, "pre",
                                      ifelse(cum_cost > icl_amt &
                                               plan_cost <= oopt_amt,
                                             "gap", "cat"))] %>%
      .[, icl := ifelse(get(paste0("arm_", month)) == "gap", 1, 0)]

    DT_fit %<>%
      .[, y_mort := get(paste0("mort_", month))] %>%
      .[, y_days := get(paste0("pill_days_", month))] %>%
      .[, z := get(instrument)]

    fit_mort <- lm_robust(y_mort ~ z:factor(pred_cut1) +
                            factor(pred_cut1) +
                            factor(race) + factor(sex),
                          data = DT_fit, se_type = "stata",
                          fixed_effects = ~ cntrct_pbp_rfrnc_yr)

    dt_fit_mort <- fit_to_dt(fit_mort, "z", "pred_cut1") %>%
      .[, .(pred_cut1, estimate, std.error, p.value)] %>%
      .[pred_cut1 == 2, ] %>%
      .[, outcome := "mort"]

    fit_days <- lm_robust(y_days ~ z:factor(pred_cut1) +
                            factor(pred_cut1) +
                            factor(race) + factor(sex),
                          data = DT_fit, se_type = "stata",
                          fixed_effects = ~ cntrct_pbp_rfrnc_yr)

    dt_fit_days <- fit_to_dt(fit_days, "z", "pred_cut1") %>%
      .[, .(pred_cut1, estimate, std.error, p.value)] %>%
      .[pred_cut1 == 2, ] %>%
      .[, outcome := "days"]

    fit_icl <- lm_robust(icl ~ z:factor(pred_cut1) +
                            factor(pred_cut1) +
                            factor(race) + factor(sex),
                          data = DT_fit, se_type = "stata",
                          fixed_effects = ~ cntrct_pbp_rfrnc_yr)

    dt_fit_icl <- fit_to_dt(fit_icl, "z", "pred_cut1") %>%
      .[, .(pred_cut1, estimate, std.error, p.value)] %>%
      .[pred_cut1 == 2, ] %>%
      .[, outcome := "icl"]

    dt_fit1 <- rbind(dt_fit_icl, dt_fit_days, dt_fit_mort) %>%
      .[, month := month] %>%
      .[, inst := instrument] %>%
      .[, obs := nrow(DT_fit)] %>%
      .[, obs1 := nrow(DT_fit[pred_cut1 == 2, ])]
  }

# Export -----------------------------------------------------------------------

fwrite(dt_fit, paste0(lib_base_data, "year_1_2_mort_days_icl_rf_estimates_",
                      pct, ".csv"))

end_log_file()
