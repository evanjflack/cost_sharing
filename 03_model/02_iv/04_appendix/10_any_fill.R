# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Creates table D.3 (dropping all drugs)

# Start Script -----------------------------------------------------------------

source("../../../00_pre_process/start_script.R")

library(estimatr)
library(foreach)
library(xtable)

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct")
)
unpack_opt(option_list)

# Start log file
start_log_file("log/10_any_fill")

# Read In Data -----------------------------------------------------------------

# Model data
model_vars <- c("bene_id", "first_mo", "rfrnc_yr", "pred_cut1",
                "race", "sex", "cntrct_pbp_rfrnc_yr", "med_inc",
                "cov_per_month100")
DT_model <- fread(paste0(lib_base_data, "revision_model_data_", pct,
                         ".csv")) %>%
  .[, model_vars, with = FALSE]
rm(model_vars)

claims_mo <- fread(paste0(lib_base_data,
                          "part_d_claims_pill_days_mo_new_enrollee_",
                          pct, ".csv")) %>%
  .[, .(bene_id, pill_days_1_12)]

init_fills <- fread(paste0(lib_base_data,
                          "initial_part_d_claims_new_enrollee_", pct,
                          ".csv"))
# Prep Data --------------------------------------------------------------------

DT_model %<>%
  merge(init_fills, by = "bene_id") %>%
  merge(claims_mo, by = "bene_id")

# Panel A ----------------------------------------------------------------------
DT_model %<>%
  .[,no_fill := ifelse(pill_days_1_12 == 0, 1, 0)] %>%
  .[, init_cut1 := cut(n_claims_90, c(quantile(n_claims_90, seq(0, .66, .33)), Inf),
                       right = FALSE,
                       labels = seq(1, 3)),
    by = pred_cut1]

fit_cov <- lm_robust(no_fill ~ cov_per_month100:factor(pred_cut1) +
                       factor(pred_cut1) + factor(race) + factor(sex),
                     data = DT_model, se_type = "stata",
                     fixed_effects = ~ cntrct_pbp_rfrnc_yr)

dt_mean <- calc_cmean(DT_model, "no_fill", c("pred_cut1")) %>%
  .[pred_cut1 == 2, ] %>%
  .[, pred_cut1 := as.character(pred_cut1)]

dt_fit1 <- fit_to_dt(fit_cov, "cov_per_month100", c("pred_cut1")) %>%
  .[pred_cut1 == 2, ] %>%
  .[, .(pred_cut1, estimate, std.error, p.value)] %>%
  merge(dt_mean, by = c("pred_cut1")) %>%
  .[, mean := mean * 100] %>%
  .[, estimate := estimate * 100] %>%
  .[, std.error := std.error * 100] %>%
  .[, perc := estimate / mean * 100] %>%
  .[, mean := as.character(round(mean, 1))] %>%
  clean_fit_dt(c("pred_cut1", "mean", "perc")) %>%
  .[, .(pred_cut1, mean, est_se, perc)] %>%
  .[, pred_cut1 := "\\hspace{5 mm} All middle-spenders"]


print(xtable(dt_fit1), sanitize.text.function = force,
      include.rownames = F)

# Panel B ----------------------------------------------------------------------


fit_cov <- lm_robust(no_fill ~ cov_per_month100:factor(pred_cut1):factor(init_cut1) +
                       factor(pred_cut1)*factor(init_cut1) + factor(race) +
                       factor(sex),
                     data = DT_model, se_type = "stata",
                     fixed_effects = ~ cntrct_pbp_rfrnc_yr)

dt_mean <- calc_cmean(DT_model, "no_fill", c("pred_cut1", "init_cut1")) %>%
  .[pred_cut1 == 2, ]


dt_fit1 <- fit_to_dt(fit_cov, "cov_per_month100", c("pred_cut1", "init_cut1")) %>%
  .[pred_cut1 == 2, ] %>%
  .[, .(init_cut1, estimate, std.error, p.value)] %>%
  merge(dt_mean, by = c("init_cut1")) %>%
  .[, mean := mean * 100] %>%
  .[, estimate := estimate * 100] %>%
  .[, std.error := std.error * 100] %>%
  .[, perc := estimate / mean * 100] %>%
  .[, mean := as.character(round(mean, 1))] %>%
  clean_fit_dt(c("init_cut1",  "mean", "perc")) %>%
  .[order(init_cut1), ] %>%
  .[, .(init_cut1, mean, est_se, perc)] %>%
  .[, perc := round(perc, 2)] %>%
  .[, init_cut1 := c("Lowest third", "Middle third", "Top Third")] %>%
  .[, init_cut1 := paste0("\\hspace{5 mm} ", init_cut1)]

print(xtable(dt_fit1), sanitize.text.function = force,
      include.rownames = F)

end_log_file()