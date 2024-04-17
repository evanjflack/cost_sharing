# Proj:Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Makes Table 3 panel C (key falsification estimates)

# Start Script -----------------------------------------------------------------
library(stringr)
library(estimatr)
library(xtable)

source("../../../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type='character', default = "20pct")
)
unpack_opt(option_list)

start_log_file("log/03b_mort_false")

# Main Sample Age 66 -----------------------------------------------------------

model_vars <- c("bene_id", "rfrnc_yr", "first_mo", "race", "sex",
                "cntrct_pbp_rfrnc_yr", "cov_per_month100")
DT_fit <- fread(paste0(lib_base_data, "revision_model_data_", pct,
                       ".csv")) %>%
  .[, model_vars, with = FALSE]

init_cost <-  fread(paste0(lib_base_data, "initial_part_d_spending_new_enrollee_", pct,
                           ".csv")) %>%
  .[, c("bene_id", paste0("initial_cost_", c(90))), with = FALSE]

mort_outcomes <- fread(paste0(lib_base_data, "mort_outcomes_jun_year_1_", pct,
                              ".csv")) %>%
  .[, c("bene_id", paste0("jun_mort_", seq(1, 19)))] %>%
  setnames(c("bene_id", paste0("mort_", seq(6, 24)))) %>%
  .[, c("bene_id", paste0("mort_", seq(6, 24))), with = FALSE] %>%
  .[, mort_pre := rowSums(.SD), .SDcols = paste0("mort_", seq(12, 23))] %>%
  .[, mort_pre := ifelse(mort_pre > 0, 1, 0)] %>%
  .[, .(bene_id, mort_24, mort_pre)]

DT_fit %<>%
  merge(mort_outcomes, by = "bene_id")

DT_fit %<>%
  .[mort_pre == 0, ]

DT_fit %<>%
  merge(init_cost, by = "bene_id") %>%
  .[, pred_cut1 := ifelse(initial_cost_90 <= quantile(initial_cost_90, .7),
                          1, ifelse(initial_cost_90 <=
                                      quantile(initial_cost_90, .97), 2, 3)),
    by = first_mo]

dt_mean_pred <- calc_cmean(DT_fit, "mort_24", c("pred_cut1")) %>%
  .[, `:=`(variable = NULL, pred_cut1 = as.character(pred_cut1))] %>%
  setnames("mean", "base_mort")

# Enrollment month instrument
fit_pred1 <- lm_robust(mort_24 ~ first_mo:factor(pred_cut1) +
                         factor(pred_cut1) +
                         factor(race) + factor(sex),
                       fixed_effects = ~ cntrct_pbp_rfrnc_yr,
                       data = DT_fit, se_type = "stata")

dt_fit1 <- fit_to_dt(fit_pred1, "first_mo", "pred_cut1") %>%
  .[, .(pred_cut1, estimate, std.error, p.value, statistic)] %>%
  .[pred_cut1 == 2, ] %>%
  .[, inst := "first_mo"] %>%
  merge(dt_mean_pred, by = "pred_cut1") %>%
  .[, base_mort := base_mort * 100] %>%
  .[, estimate := estimate * 100] %>%
  .[, std.error := std.error * 100] %>%
  .[, .(inst, base_mort, estimate, std.error, p.value)]

# Pre-donut coverage estimate
fit_pred2 <- lm_robust(mort_24 ~ cov_per_month100:factor(pred_cut1) +
                         factor(pred_cut1) +
                         factor(race) + factor(sex),
                       fixed_effects = ~ cntrct_pbp_rfrnc_yr,
                       data = DT_fit, se_type = "stata")

dt_fit2 <- fit_to_dt(fit_pred2, "cov_per_month100", "pred_cut1") %>%
  .[, .(pred_cut1, estimate, std.error, p.value, statistic)] %>%
  .[pred_cut1 == 2, ] %>%
  .[, inst := "cov_per_month100"] %>%
  merge(dt_mean_pred, by = "pred_cut1") %>%
  .[, base_mort := base_mort * 100] %>%
  .[, estimate := estimate * 100] %>%
  .[, std.error := std.error * 100] %>%
  .[, .(inst, base_mort, estimate, std.error, p.value)]

print(dt_fit2)

dt_fit_ws_66 <- rbind(dt_fit1, dt_fit2) %>%
  .[, type := "main_66"]

# Dual Age 65 ------------------------------------------------------------------

mort_outcomes <- fread(paste0(lib_base_data, "mort_outcomes_jun_year_1_", pct,
                              ".csv")) %>%
  .[, .(bene_id, jun_mort_7)] %>%
  setnames("jun_mort_7", "dec_mort")

sample <- fread(paste0(lib_base_data, "age65_dual_false_sample_",
                       pct, ".csv")) %>%
  .[, .(bene_id, rfrnc_yr, first_mo, race, sex, cntrct, pbp)] %>%
  .[rfrnc_yr <= 2012, ] %>%
  .[first_mo %between% c(2, 9)]

DT_fit <- sample %>%
  merge(mort_outcomes, by = "bene_id") %>%
  merge(init_cost, by = "bene_id") %>%
  .[, pred_cut1 := ifelse(initial_cost_90 <= quantile(initial_cost_90, .7),
                          1, ifelse(initial_cost_90 <=
                                      quantile(initial_cost_90, .97), 2, 3)),
    by = first_mo] %>%
  .[, cntrct_pbp_rfrnc_yr := paste0(cntrct, "_", pbp, "_", rfrnc_yr)]

icl_cat <- fread(paste0(lib_base_data, "icl_std_amts.csv")) %>%
  .[, .(rfrnc_yr, icl_std)]

DT_fit %<>%
  merge(icl_cat, by = "rfrnc_yr")

DT_fit %<>%
  .[, months_remain := 12 - first_mo + 1] %>%
  .[, cov_per_month := icl_std / months_remain] %>%
  .[, cov_per_month100 := cov_per_month / 100]


dt_mean_pred <- calc_cmean(DT_fit, "dec_mort", c("pred_cut1")) %>%
  .[, `:=`(variable = NULL, pred_cut1 = as.character(pred_cut1))] %>%
  setnames("mean", "base_mort")

# Enrollment month instrument
fit_pred1 <- lm_robust(dec_mort ~ first_mo:factor(pred_cut1) +
                         factor(pred_cut1) +
                         factor(race) + factor(sex),
                       fixed_effects = ~ cntrct_pbp_rfrnc_yr,
                       data = DT_fit, se_type = "stata")

dt_fit1 <- fit_to_dt(fit_pred1, "first_mo", "pred_cut1") %>%
  .[, .(pred_cut1, estimate, std.error, p.value, statistic)] %>%
  .[pred_cut1 == 2, ] %>%
  .[, inst := "first_mo"] %>%
  merge(dt_mean_pred, by = "pred_cut1") %>%
  .[, .(inst, base_mort, estimate, std.error, p.value)]

# Pre-donut coverage estimate
fit_pred2 <- lm_robust(dec_mort ~ cov_per_month100:factor(pred_cut1) +
                         factor(pred_cut1) +
                         factor(race) + factor(sex),
                       fixed_effects = ~ cntrct_pbp_rfrnc_yr,
                       data = DT_fit, se_type = "stata")

dt_fit2 <- fit_to_dt(fit_pred2, "cov_per_month100", "pred_cut1") %>%
  .[, .(pred_cut1, estimate, std.error, p.value, statistic)] %>%
  .[pred_cut1 == 2, ] %>%
  .[, inst := "cov_per_month100"] %>%
  merge(dt_mean_pred, by = "pred_cut1") %>%
  .[, .(inst, base_mort, estimate, std.error, p.value)]

dt_fit_dual_65 <- rbind(dt_fit1, dt_fit2) %>%
  .[, base_mort := base_mort * 100] %>%
  .[, estimate := estimate * 100] %>%
  .[, std.error := std.error * 100] %>%
  .[, type := "dual_65"]


# Disabled Age 64 --------------------------------------------------------------

# Jan-Mar total part d spending for each of the three samples above
init_cost <- read_and_combine(lib_base_data, "old_false_init_cost",
                              seq(2007, 2012), pct)

DT_fit <- read_and_combine(lib_base_data, "dual_disabled_sample", seq(2007, 2012),
                           pct) %>%
  .[age1 == 64 & month == 12] %>%
  merge(init_cost, by = c("bene_id", "rfrnc_yr"), all.x = T) %>%
  .[is.na(initial_cost3), initial_cost3 := 0] %>%
  .[birth_mo %in% seq(2, 9)] %>%
  merge(icl_cat, by = "rfrnc_yr")

DT_fit %<>%
  .[, months_remain := 12 - birth_mo + 1] %>%
  .[, cov_per_month := icl_amt / months_remain] %>%
  .[, cov_per_month100 := cov_per_month / 100]

DT_fit %<>%
  .[, pred_cut1 := ifelse(initial_cost3 <= quantile(initial_cost3, .7), 1,
                          ifelse(initial_cost3 <= quantile(initial_cost3,
                                                           .97), 2, 3)),
    by = birth_mo] %>%
  .[, cntrct_pbp_rfrnc_yr := paste(cntrct, pbp, rfrnc_yr, sep = "")]

dt_mean_pred <- calc_cmean(DT_fit, "mort", c("pred_cut1")) %>%
  .[, `:=`(variable = NULL, pred_cut1 = as.character(pred_cut1))] %>%
  setnames("mean", "base_mort")

# Enrollment month instrument
fit_pred1 <- lm_robust( mort ~ birth_mo:factor(pred_cut1) +
                         factor(pred_cut1) +
                         factor(race) + factor(sex),
                       fixed_effects = ~ cntrct_pbp_rfrnc_yr,
                       data = DT_fit, se_type = "stata")

dt_fit1 <- fit_to_dt(fit_pred1, "birth_mo", "pred_cut1") %>%
  .[, .(pred_cut1, estimate, std.error, p.value, statistic)] %>%
  .[pred_cut1 == 2, ] %>%
  .[, inst := "first_mo"] %>%
  merge(dt_mean_pred, by = "pred_cut1") %>%
  .[, .(inst, base_mort, estimate, std.error, p.value)]

# Pre-donut coverage estimate
fit_pred2 <- lm_robust(mort ~ cov_per_month100:factor(pred_cut1) +
                         factor(pred_cut1) +
                         factor(race) + factor(sex),
                       fixed_effects = ~ cntrct_pbp_rfrnc_yr,
                       data = DT_fit, se_type = "stata")

dt_fit2 <- fit_to_dt(fit_pred2, "cov_per_month100", "pred_cut1") %>%
  .[, .(pred_cut1, estimate, std.error, p.value, statistic)] %>%
  .[pred_cut1 == 2, ] %>%
  .[, inst := "cov_per_month100"] %>%
  merge(dt_mean_pred, by = "pred_cut1") %>%
  .[, .(inst, base_mort, estimate, std.error, p.value)]

dt_fit_dis_64 <- rbind(dt_fit1, dt_fit2) %>%
  .[, base_mort := base_mort * 100] %>%
  .[, estimate := estimate * 100] %>%
  .[, std.error := std.error * 100] %>%
  .[, type := "dis_64"]

# Print ------------------------------------------------------------------------

dt_print <- rbind(dt_fit_ws_66, dt_fit_dual_65, dt_fit_dis_64) %>%
  .[, base_mort := round(base_mort, 3)] %>%
  clean_fit_dt(c("type", "inst", "base_mort")) %>%
  dcast(type + base_mort ~ inst, value.var = "est_se") %>%
  .[, type := factor(type, levels = c("main_66", "dual_65", "dis_64"))] %>%
  .[order(type), ] %>%
  .[, .(type, base_mort, first_mo, cov_per_month100)] %>%
  .[, type := c("Main Sample: December, age 66", "Duals: December, age 65",
                "Disabled: December, age 64")]

print(xtable(dt_print), include.rownames = FALSE,
      sanitize.text.function = force)


end_log_file()