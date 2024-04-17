# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Makes appendix table C2 (birth month estimates)

# Start Script -----------------------------------------------------------------

library(estimatr)
library(xtable)

source("../../../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct")
)
unpack_opt(option_list)

# Start log file
start_log_file("log/06_birth_mo")

cut1 <- .7
cut2 <- .97

# Read In Data -----------------------------------------------------------------

# Model data
model_vars <- c("bene_id", "first_mo", "rfrnc_yr", "race", "sex", "birth_mo",
                "cntrct_pbp_rfrnc_yr", "year_cut", "initial_cost_90",
                "cov_per_month100")
DT_model <- fread(paste0(lib_base_data, "revision_model_data_", pct,
                         ".csv")) %>%
  .[, model_vars, with = FALSE]
rm(model_vars)

# December mortality indicators
mort_outcomes <- fread(paste0(lib_base_data, "mort_outcomes_jun_year_1_", pct,
                              ".csv")) %>%
  .[, .(bene_id, jun_mort_7)] %>%
  setnames("jun_mort_7", "dec_mort")

DT_model %<>%
  merge(mort_outcomes, by = "bene_id")

# Those Enrolling in Birth Month -----------------------------------------------


DT_fit <- copy(DT_model) %>%
  .[birth_mo == first_mo] %>%
  .[, pred_cut1 := ifelse(initial_cost_90 <= quantile(initial_cost_90, cut1),
                          1, ifelse(initial_cost_90 <=
                                      quantile(initial_cost_90, cut2), 2, 3)),
    by = birth_mo]

# Sample mean
dt_mean_pred_b <- calc_cmean(DT_fit, "dec_mort", c("pred_cut1")) %>%
  .[, `:=`(variable = NULL, pred_cut1 = as.character(pred_cut1))] %>%
  setnames("mean", "base_mort")

fit_pred_b <- lm_robust(dec_mort ~ birth_mo:factor(pred_cut1) +
                          factor(pred_cut1) +
                          factor(race) + factor(sex),
                        fixed_effects = ~ cntrct_pbp_rfrnc_yr,
                        data = DT_fit,
                        se_type = "stata")

dt_fit1 <- fit_to_dt(fit_pred_b, "birth_mo", "pred_cut1") %>%
  .[, .(pred_cut1, estimate, std.error, p.value)] %>%
  merge(dt_mean_pred_b, by = "pred_cut1") %>%
  .[, type := "same"]

# Entire Sample ----------------------------------------------------------------
DT_fit <- copy(DT_model) %>%
  .[, pred_cut1 := ifelse(initial_cost_90 <= quantile(initial_cost_90, cut1),
                          1, ifelse(initial_cost_90 <=
                                      quantile(initial_cost_90, cut2), 2, 3)),
    by = birth_mo]

# Sample mean
dt_mean_pred_b <- calc_cmean(DT_fit, "dec_mort", c("pred_cut1")) %>%
  .[, `:=`(variable = NULL, pred_cut1 = as.character(pred_cut1))] %>%
  setnames("mean", "base_mort")

fit_pred_b <- lm_robust(dec_mort ~ birth_mo:factor(pred_cut1) +
                          factor(pred_cut1) +
                          factor(race) + factor(sex),
                        fixed_effects = ~ cntrct_pbp_rfrnc_yr,
                        data = DT_fit,
                        se_type = "stata")

dt_fit2 <- fit_to_dt(fit_pred_b, "birth_mo", "pred_cut1") %>%
  .[, .(pred_cut1, estimate, std.error, p.value)] %>%
  merge(dt_mean_pred_b, by = "pred_cut1") %>%
  .[, type := "diff"]

# Print ------------------------------------------------------------------------

dt_fit <- rbind(dt_fit1, dt_fit2) %>%
  .[order(type, pred_cut1), ] %>%
  .[, .(type, pred_cut1, base_mort, estimate, std.error, p.value)] %>%
  .[, `:=`(estimate = estimate*100, std.error = std.error*100,
           base_mort = base_mort*100)] %>%
  clean_fit_dt(c("pred_cut1", "type", "base_mort")) %>%
  .[, base_mort := round(base_mort, 3)] %>%
  .[, pred_cut1 := c("Low-spenders", "Middle-spenders", "High-spenders",
                     "Low-spenders", "Middle-spenders", "High-spenders")] %>%
  .[, pred_cut1 := paste0("\\hspace{5 mm} ", pred_cut1)]

print(xtable(dt_fit, digits = 3),
      include.rownames = F, sanitize.text.function = force)

# Birth Month Means ------------------------------------------------------------

DT_fit <- copy(DT_model) %>%
  .[birth_mo == first_mo] %>%
  .[, pred_cut1 := ifelse(initial_cost_90 <= quantile(initial_cost_90, cut1),
                          1, ifelse(initial_cost_90 <=
                                      quantile(initial_cost_90, cut2), 2, 3)),
    by = birth_mo]

dtp <- calc_cmean(DT_fit, "dec_mort", c("birth_mo", "pred_cut1"), se = T) %>%
  .[pred_cut1 == 2, ] %>%
  .[, mean := round(mean * 100, 3)] %>%
  .[, se := round(se * 100, 3)] %>%
  .[, mean_se := paste0("\\begin{tabular}{@{}c@{}}", mean,
                       "\\\\ (", se,  ")\\end{tabular}")] %>%
  .[order(birth_mo)] %>%
  dcast(pred_cut1 ~ birth_mo, value.var = "mean_se") %>%
  .[, pred_cut1 := NULL]

print(xtable(dtp), sanitize.text.function = force,
      include.rownames = FALSE)

end_log_file()
