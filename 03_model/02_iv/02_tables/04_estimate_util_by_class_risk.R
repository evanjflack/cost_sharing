# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Creates table 4: utilization response by class and risk

# Start Script -----------------------------------------------------------------
source("../../../00_pre_process/start_script.R")

library(estimatr)
library(foreach)
library(xtable)
library(stringr)

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct")
)
unpack_opt(option_list)

# Start log file
start_log_file("log/04_estimate_util_by_class_risk")

# Read In Data -----------------------------------------------------------------

# Model data
model_vars <- c("bene_id", "first_mo", "rfrnc_yr", "pred_cut1",
                "race", "sex", "cntrct_pbp_rfrnc_yr", "med_inc",
                "cov_per_month100")
DT_model <- fread(paste0(lib_base_data, "revision_model_data_", pct,
                         ".csv")) %>%
  .[, model_vars, with = FALSE]
rm(model_vars)

atc_days <- fread(paste0(lib_base_data, "dec_year1_atc_days_new_enrollee_",
                          pct, ".csv")) %>%
  .[, .(bene_id, card_days_1_12, atc2_A10_days_1_12, atc2_R03_days_1_12)] %>%
  setnames(c("bene_id", "card_days", "diab_days", "resp_days"))

claims_mo <- fread(paste0(lib_base_data,
                          "part_d_claims_pill_days_mo_new_enrollee_",
                          pct, ".csv")) %>%
  .[, .(bene_id, pill_days_1_12)]

pred_card <- readRDS( paste0(lib_base_data,
                             "ensemble_pred_new_enrollee_dual_false_",
                             "comp_card", "_", "untreated_any_card", "_",
                             66, "_", 90, "_days_", pct, ".rds")) %>%
  .[, .(bene_id, ensemble_pred)] %>%
  setnames("ensemble_pred", "ensemble_pred_card_90")

pred_diab <- readRDS( paste0(lib_base_data,
                             "ensemble_pred_new_enrollee_dual_false_",
                             "comp_diab", "_", "untreated_any_diab", "_",
                             66, "_", 90, "_days_", pct, ".rds")) %>%
  .[, .(bene_id, ensemble_pred)] %>%
  setnames("ensemble_pred", "ensemble_pred_diab_90")

pred_resp <- readRDS( paste0(lib_base_data,
                             "ensemble_pred_new_enrollee_dual_false_",
                             "comp_resp", "_", "untreated_resp", "_",
                             66, "_", 90, "_days_", pct, ".rds")) %>%
  .[, .(bene_id, ensemble_pred)] %>%
  setnames("ensemble_pred", "ensemble_pred_resp_90")

# icl_cat <- fread(paste0(lib_base_data, "icl_std_amts.csv"))

# Prep Data --------------------------------------------------------------------

DT_model %<>%
  merge(atc_days, by = "bene_id") %>%
  merge(claims_mo, by = "bene_id") %>%
  merge(pred_card, by = "bene_id") %>%
  merge(pred_diab, by = "bene_id") %>%
  merge(pred_resp, by = "bene_id")

# Set the risk for all drugs as cardiovascular
DT_model %<>%
  .[, ensemble_pred_all_90 := ensemble_pred_card_90] %>%
  .[, all_days := pill_days_1_12]

# Utilization by Risk ----------------------------------------------------------

vars <- c("all", "card", "diab", "resp")
dt_fit <- data.table()
for (i in vars) {
  message(i)
  DT_fit <- copy(DT_model) %>%
    .[, y := get(paste0(i, "_days"))] %>%
    .[, risk_pred := get(paste0("ensemble_pred_", i, "_90"))] %>%
    .[, risk_cut1 := ifelse(risk_pred >= quantile(risk_pred, .66), 1, 0),
      by = .(first_mo, pred_cut1)]

  fit_all <- lm_robust(y ~ cov_per_month100:factor(pred_cut1) +
                         factor(pred_cut1) + factor(race) + factor(sex),
                       data = DT_fit, se_type = "stata",
                       fixed_effects = ~ cntrct_pbp_rfrnc_yr)
  dt_fit_all <- fit_to_dt(fit_all, "cov_per_month100", "pred_cut1") %>%
    .[pred_cut1 == 2, ] %>%
    .[, .(estimate, std.error, p.value)] %>%
    .[, mean := mean(DT_fit[pred_cut1 == 2, y])] %>%
    .[, risk_cut1 := "2"] %>%
    .[, inc_cut1 := "2"] %>%
    .[, type := "all"]

  fit_risk <- lm_robust(y ~ cov_per_month100:factor(pred_cut1):factor(risk_cut1) +
                          factor(pred_cut1)*factor(risk_cut1) +
                          factor(race) + factor(sex),
                       data = DT_fit, se_type = "stata",
                       fixed_effects = ~ cntrct_pbp_rfrnc_yr)

  mean_risk <- calc_cmean(DT_fit, "y", c("pred_cut1", "risk_cut1")) %>%
    .[pred_cut1 == 2, ] %>%
    .[, .(risk_cut1, mean)] %>%
    .[, risk_cut1 := as.character(risk_cut1)]

  dt_fit_risk <- fit_to_dt(fit_risk, "cov_per_month100",
                           c("pred_cut1", "risk_cut1")) %>%
    .[pred_cut1 == 2, ] %>%
    .[, .(risk_cut1, estimate, std.error, p.value)] %>%
    .[, inc_cut1 := "2"] %>%
    merge(mean_risk, by = "risk_cut1") %>%
    .[, type := "risk"] %>%
    .[order(risk_cut1), ]

  DT_fit %<>%
    .[!is.na(med_inc), ] %>%
    .[, inc_cut1 := ifelse(med_inc >= quantile(med_inc, .66), 1, 0),
      by = .(first_mo, pred_cut1)]

  fit_inc <- lm_robust(y ~ cov_per_month100:factor(pred_cut1):factor(inc_cut1) +
                          factor(pred_cut1)*factor(inc_cut1) +
                          factor(race) + factor(sex),
                        data = DT_fit, se_type = "stata",
                       fixed_effects = ~ cntrct_pbp_rfrnc_yr)

  mean_inc <- calc_cmean(DT_fit, "y", c("pred_cut1", "inc_cut1")) %>%
    .[pred_cut1 == 2, ] %>%
    .[, .(inc_cut1, mean)] %>%
    .[, inc_cut1 := as.character(inc_cut1)]

  dt_fit_inc <- fit_to_dt(fit_inc, "cov_per_month100",
                           c("pred_cut1", "inc_cut1")) %>%
    .[pred_cut1 == 2, ] %>%
    .[, .(inc_cut1, estimate, std.error, p.value)] %>%
    .[, risk_cut1 := "2"] %>%
    merge(mean_inc, by = "inc_cut1") %>%
    .[, type := "inc"]

  dt_fit1 <- rbind(dt_fit_all, dt_fit_risk) %>%
    rbind(dt_fit_inc) %>%
    .[, class := i]

  dt_fit %<>% rbind(dt_fit1)
}

# Print (Risk) -----------------------------------------------------------------
message("Printing risk...\n")

dt_print_risk <- copy(dt_fit) %>%
  .[type %in% c("all", "risk")] %>%
  .[, .(class, risk_cut1, mean, estimate, std.error, p.value)] %>%
  .[, mean := round(mean, 1)] %>%
  clean_fit_dt(id_vars = c("class", "risk_cut1", "mean")) %>%
  .[, .(class, risk_cut1, mean, est_se)] %>%
  dcast(class ~ risk_cut1, value.var = c("est_se", "mean")) %>%
  .[, c("class", "mean_2", "est_se_2", "mean_0", "est_se_0",
        "mean_1", "est_se_1"), with = FALSE] %>%
  .[, class := factor(class, levels = c("all", "card", "diab", "resp"),
                      labels = c("All Classes", "Cardiovascular", "Diabetes",
                                 "Respiratory"))] %>%
  .[order(class), ] %>%
  .[, class := paste0("\\hspace{5 mm}", class)]

print(xtable(dt_print_risk, digits = 2),
      include.rownames = F, sanitize.text.function = force)

# Print (Income) ---------------------------------------------------------------
message("Printing income...\n")

dt_print_inc <- copy(dt_fit) %>%
  .[type %in% c("all", "inc")] %>%
  .[, .(class, inc_cut1, mean, estimate, std.error, p.value)] %>%
  .[, mean := round(mean, 1)] %>%
  clean_fit_dt(id_vars = c("class", "inc_cut1", "mean")) %>%
  .[, .(class, inc_cut1, mean, est_se)] %>%
  dcast(class ~ inc_cut1, value.var = c("est_se", "mean")) %>%
  .[, c("class", "mean_2", "est_se_2", "mean_0", "est_se_0",
        "mean_1", "est_se_1"), with = FALSE] %>%
  .[, class := factor(class, levels = c("all", "card", "diab", "resp"),
                      labels = c("All Classes", "Cardiovascular", "Diabetes",
                                 "Respiratory"))] %>%
  .[order(class), ] %>%
  .[, class := paste0("\\hspace{5 mm}", class)]

print(xtable(dt_print_inc, digits = 2),
      include.rownames = F, sanitize.text.function = force)

end_log_file()
