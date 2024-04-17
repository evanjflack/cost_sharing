# Header -----------------------------------------------------------------------
# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Makes Table 3, Mortality Effects of Cost-Sharing

# Start Script -----------------------------------------------------------------
library(estimatr)
library(xtable)
library(stringr)
source("../../../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct")
)
unpack_opt(option_list)

# Start log file
start_log_file("log/03a_mort_rf")

# User inputs
instruments <- c("first_mo", "cov_per_month100")

# Read In Data -----------------------------------------------------------------

# Model data
model_vars <- c("bene_id", "first_mo", "rfrnc_yr", "race", "sex", "birth_mo",
                "cntrct_pbp_rfrnc_yr", "year_cut", "initial_cost_90",
                "cov_per_month100", "pred_cut1")
DT_model <- fread(paste0(lib_base_data, "revision_model_data_", pct,
                         ".csv")) %>%
  .[, model_vars, with = FALSE]
rm(model_vars)

# December mortality indicators
mort_outcomes <- fread(paste0(lib_base_data, "mort_outcomes_jun_year_1_", pct,
                              ".csv")) %>%
  .[, .(bene_id, jun_mort_7)] %>%
  setnames("jun_mort_7", "dec_mort")


# Prep Data --------------------------------------------------------------------

DT_model %<>%
  merge(mort_outcomes, by = "bene_id")

rm(mort_outcomes)

# RF by Initial Spending -------------------------------------------------------
message("By initial spending")

# Sample mean
dt_mean_pred <- calc_cmean(DT_model, "dec_mort", c("pred_cut1")) %>%
  .[, `:=`(variable = NULL, pred_cut1 = as.character(pred_cut1))] %>%
  setnames("mean", "base_mort")

# Linear model (with controls + plan FE)
dt_fit_pred <- data.table()
dt_fit_comp <- data.table()
for (i in instruments) {
  message(i)
  DT_model %<>%
    .[, z:= get(i)]
  fit_pred <- lm_robust(dec_mort ~ z:factor(pred_cut1) +
                          factor(pred_cut1) +
                          factor(race) + factor(sex),
                        fixed_effects = ~ cntrct_pbp_rfrnc_yr,
                        data = DT_model, se_type = "stata")

  dt_fit_pred1 <- fit_to_dt(fit_pred, "z", "pred_cut1") %>%
    .[, .(pred_cut1, estimate, std.error, p.value, statistic)] %>%
    merge(dt_mean_pred, by = "pred_cut1") %>%
    .[, type := "2pred"] %>%
    .[order(pred_cut1), ] %>%
    .[, coef := c("1", "2", "3")] %>%
    .[, inst := i] %>%
    .[, .(type, inst, coef, base_mort, estimate, std.error, p.value)]

  dt_fit_pred %<>% rbind(dt_fit_pred1)

  DT_fit <- copy(DT_model) %>%
    .[, pred_cut1 := factor(pred_cut1, levels = c(1, 2, 3))]

  fit_comp1 <- lm_robust(dec_mort ~ z*factor(pred_cut1) +
                          factor(race) + factor(sex),
                        fixed_effects = ~ cntrct_pbp_rfrnc_yr,
                        data = DT_fit, se_type = "stata")

  dt_fit_comp1 <- tidy(fit_comp1) %>%
    as.data.table() %>%
    .[term == "z:factor(pred_cut1)2"] %>%
    .[, comp := "1_2"] %>%
    .[, inst := i]

  DT_fit <- copy(DT_model) %>%
    .[, pred_cut1 := factor(pred_cut1, levels = c(3, 1, 2))]

  fit_comp2 <- lm_robust(dec_mort ~ z*factor(pred_cut1) +
                           factor(race) + factor(sex),
                         fixed_effects = ~ cntrct_pbp_rfrnc_yr,
                         data = DT_fit, se_type = "stata")

  dt_fit_comp2 <- tidy(fit_comp2) %>%
    as.data.table() %>%
    .[term == "z:factor(pred_cut1)2"] %>%
    .[, comp := "3_2"] %>%
    .[, inst := i]

  dt_fit_comp <- rbind(dt_fit_comp, dt_fit_comp1, dt_fit_comp2)

}

message('Printing Panel A rows 1-3\n')

dt_print_pred <- copy(dt_fit_pred) %>%
  .[, `:=`(estimate = estimate*100, std.error = std.error*100,
           base_mort = base_mort*100)] %>%
  .[, base_mort := round(base_mort, 3)] %>%
  clean_fit_dt(c("coef", "type", "inst", "base_mort")) %>%
  dcast(coef + base_mort ~ inst, value.var = "est_se") %>%
  .[order(coef), ] %>%
  .[, coef := c("Low-spenders", "Middle-spenders", "High-spenders")] %>%
  .[, coef := paste0("\\hspace{5 mm} ", coef)] %>%
  .[, .(coef, base_mort, first_mo, cov_per_month100)]

print(xtable(dt_print_pred, digits = 3),
      include.rownames = F, sanitize.text.function = force)

message('Printing Panel A rows 4-5\n')

dt_print_comp <- copy(dt_fit_comp) %>%
  .[, `:=`(estimate = estimate*100, std.error = std.error*100)] %>%
  clean_fit_dt(c("comp", "inst")) %>%
  dcast(comp ~ inst, value.var = "est_se") %>%
  .[order(comp), ] %>%
  .[, comp := c("Difference: Middle vs. Low",
                "Difference: Middle vs. High")] %>%
  .[, base_mort := "-"] %>%
  .[, .(comp, base_mort, first_mo, cov_per_month100)] %>%
  .[, comp := paste0("\\hspace{10 mm} ", comp)]

print(xtable(dt_print_comp, digits = 3),
      include.rownames = F, sanitize.text.function = force)

# Save main est (to compare to falsification distribution)
main_est <- copy(dt_fit_pred) %>%
  .[inst == 'first_mo' & coef == "2"] %>%
  .[, .(estimate, std.error, base_mort)]

fwrite(main_est, paste0(lib_base_data, "main_first_mo_est.csv"))


# RF by Initial Spending + Year ------------------------------------------------
message("By spending/year")

# Sample mean
dt_mean_pred_year <- calc_cmean(DT_model, "dec_mort",
                                c("pred_cut1", "year_cut")) %>%
  .[, `:=`(variable = NULL, pred_cut1 = as.character(pred_cut1))] %>%
  setnames("mean", "base_mort")

# Linear model (with controls + plan FE)
dt_fit_pred_year <- data.table()
dt_fit_comp <- data.table()
for (i in instruments) {
  message(i)

  DT_model %<>%
    .[, z:= get(i)]

  fit_pred_year <- lm_robust(dec_mort ~
                               z:factor(pred_cut1):factor(year_cut) +
                               factor(pred_cut1)*factor(year_cut) +
                               factor(race) + factor(sex),
                             fixed_effects = ~ cntrct_pbp_rfrnc_yr,
                             data = DT_model, se_type = "stata")

  dt_fit_pred_year1 <- fit_to_dt(fit_pred_year, "z",
                                 c("pred_cut1", "year_cut")) %>%
    .[, .(pred_cut1, year_cut, estimate, std.error, p.value)] %>%
    merge(dt_mean_pred_year, by = c("pred_cut1", "year_cut")) %>%
    .[, type := "3pred_year"] %>%
    .[pred_cut1 == 2, ] %>%
    .[order(year_cut), ] %>%
    .[, coef := c("1", "2")] %>%
    .[, inst := i] %>%
    .[, .(type, inst, coef, base_mort, estimate, std.error, p.value)]

  dt_fit_pred_year %<>% rbind(dt_fit_pred_year1)


  DT_fit <- copy(DT_model) %>%
    .[, pred_cut1 := factor(pred_cut1, levels = c(2, 1, 3))] %>%
    .[, year_cut := factor(year_cut, levels = c("2011-2012", "2007-2010"))]

  fit_comp <- lm_robust(dec_mort ~
                          z*factor(pred_cut1)*factor(year_cut) +
                          factor(pred_cut1)*factor(year_cut) +
                          factor(race) + factor(sex),
                        fixed_effects = ~ cntrct_pbp_rfrnc_yr,
                        data = DT_fit, se_type = "stata")

  dt_fit_comp1 <- tidy(fit_comp) %>%
    as.data.table() %>%
    .[term == "z:factor(year_cut)2007-2010"] %>%
    .[, inst := i]

  dt_fit_comp %<>% rbind(dt_fit_comp1)

}

message('Printing Panel B rows 1-2\n')



dt_print_year <- copy(dt_fit_pred_year) %>%
  .[, `:=`(estimate = estimate * 100, std.error = std.error * 100,
           base_mort = base_mort * 100)] %>%
  .[, base_mort := round(base_mort, 3)] %>%
  clean_fit_dt(c("coef", "base_mort", "inst")) %>%
  dcast(coef + base_mort ~ inst, value.var = "est_se") %>%
  .[order(coef), ] %>%
  .[, coef := c("Full Donut Hole (2007-10)", "Closing Donut Hole (2011-12)")] %>%
  .[, .(coef, base_mort, first_mo, cov_per_month100)] %>%
  .[, coef := paste0("\\hspace{5 mm} ", coef)]

print(xtable(dt_print_year, digits = 3),
      include.rownames = F, sanitize.text.function = force)

message('Printing Panel B row 3\n')

dt_print_comp <- copy(dt_fit_comp) %>%
  .[, `:=`(estimate = estimate * 100, std.error = std.error * 100)] %>%
  .[, coef := "Difference: Full vs. Closing"] %>%
  clean_fit_dt(c("coef", "inst")) %>%
  .[, base_mort := "-"] %>%
  dcast(coef + base_mort ~ inst, value.var = "est_se") %>%
  .[, .(coef, base_mort, first_mo, cov_per_month100)] %>%
  .[, coef := paste0("\\hspace{10 mm} ", coef)]

print(xtable(dt_print_comp, digits = 3),
      include.rownames = F, sanitize.text.function = force)

#
# # Birth Month Instrument by Initial Spending -----------------------------------
#
# message("By initial spending")
#
# DT_fit_b <- DT_model[first_mo == birth_mo]
#
# # Sample mean
# dt_mean_pred_b <- calc_cmean(DT_fit_b, "dec_mort", c("pred_cut1")) %>%
#   .[, `:=`(variable = NULL, pred_cut1 = as.character(pred_cut1))] %>%
#   setnames("mean", "base_mort")
#
#
# fit_pred_b <- lm_robust(dec_mort ~ birth_mo:factor(pred_cut1) +
#                         factor(pred_cut1) +
#                         factor(race) + factor(sex),
#                       fixed_effects = ~ cntrct_pbp_rfrnc_yr,
#                       data = DT_fit_b, se_type = "stata")
#
# dt_fit_pred_b <- fit_to_dt(fit_pred_b, "birth_mo", "pred_cut1") %>%
#   .[, .(pred_cut1, estimate, std.error, p.value)] %>%
#   merge(dt_mean_pred_b, by = "pred_cut1") %>%
#   .[, type := "4pred_b"] %>%
#   .[order(pred_cut1), ] %>%
#   .[, coef := c("1", "2", "3")] %>%
#   .[, inst := "birth_mo"] %>%
#   .[, .(type, inst, coef, base_mort, estimate, std.error, p.value)]
#
# dt_print_b <- copy(dt_fit_pred_b) %>%
#   .[, `:=`(estimate = estimate*100, std.error = std.error*100,
#            base_mort = base_mort*100)] %>%
#   .[, base_mort := round(base_mort, 3)] %>%
#   clean_fit_dt(c("coef", "type", "inst", "base_mort"), sig = T) %>%
#   dcast(type + coef + base_mort ~ inst, value.var = "est_se") %>%
#   .[order(type, coef), ] %>%
#   .[, lab := c("1-70th percentile",
#                "71-97th Percentile", "98-100th percentile")] %>%
#   .[, .(lab, base_mort, birth_mo)] %>%
#   .[, lab := paste0("\\hspace{5 mm}", lab)]
#
# print(xtable(dt_print_b, digits = 3),
#       include.rownames = F, sanitize.text.function = force)
#
# # Pairwise Middle --------------------------------------------------------------
#
# dt_fit <- data.table()
# for (inst in instruments) {
#   print(inst)
#   DT_fit <- copy(DT_model) %>%
#     .[, z := get(inst)] %>%
#     .[, pred_cut1 := factor(pred_cut1, levels = c(1, 2, 3))]
#
#   fit_pred1 <- lm_robust(dec_mort ~ z*factor(pred_cut1) +
#                           factor(race) + factor(sex),
#                         fixed_effects = ~ cntrct_pbp_rfrnc_yr,
#                         data = DT_fit, se_type = "stata")
#
#   dt_fit1 <- tidy(fit_pred1) %>%
#     as.data.table() %>%
#     .[term == "z:factor(pred_cut1)2"] %>%
#     .[, inst := inst] %>%
#     .[, comp := "1_2"]
#
#   DT_fit <- copy(DT_model) %>%
#     .[, z := get(inst)] %>%
#     .[, pred_cut1 := factor(pred_cut1, levels = c(3, 1, 2))]
#
#   fit_pred2 <- lm_robust(dec_mort ~ z*factor(pred_cut1) +
#                            factor(race) + factor(sex),
#                          fixed_effects = ~ cntrct_pbp_rfrnc_yr,
#                          data = DT_fit, se_type = "stata")
#
#   dt_fit2 <- tidy(fit_pred2) %>%
#     as.data.table() %>%
#     .[term == "z:factor(pred_cut1)2"] %>%
#     .[, inst := inst] %>%
#     .[, comp := "3_2"]
#
#   dt_fit <- dt_fit %>%
#     rbind(dt_fit1, dt_fit2)
#
# }
#
# dt_print <- copy(dt_fit) %>%
#   .[, `:=`(estimate = estimate * 100, std.error = std.error * 100)] %>%
#   clean_fit_dt(c("inst", "comp")) %>%
#   .[order(comp, inst)]
#
# fit_pred <- lm_robust(dec_mort ~ first_mo*factor(pred_cut1) +
#                         factor(race) + factor(sex),
#                       fixed_effects = ~ cntrct_pbp_rfrnc_yr,
#                       data = DT_model, se_type = "stata")
#
# dt_fit <- tidy(fit_pred) %>%
#   as.data.table() %>%
#   .[term == "first_mo:factor(pred_cut1)2"] %>%
#   .[, estimate := estimate * 100]
#
# DT_fit <- copy(DT_model) %>%
#   .[, pred_cut1 := factor(pred_cut1, levels = c(3, 1, 2))]
#
# fit_pred <- lm_robust(dec_mort ~ first_mo*factor(pred_cut1) +
#                         factor(race) + factor(sex),
#                       fixed_effects = ~ cntrct_pbp_rfrnc_yr,
#                       data = DT_fit, se_type = "stata")
#
# dt_fit <- tidy(fit_pred) %>%
#   as.data.table() %>%
#   .[term == "first_mo:factor(pred_cut1)2"]
#
#
#
# dt_fit <- tidy(fit_pred) %>%
#   as.data.table() %>%
#   .[term == "first_mo:factor(pred_cut1)2"]
#
#
#
#
#
# # Print ------------------------------------------------------------------------
#
# dt_all <- rbind(dt_fit_pred, dt_fit_year, dt_fit_pred_year) %>%
#   .[, `:=`(estimate = estimate*100, std.error = std.error*100,
#            base_mort = base_mort*100)] %>%
#   .[, base_mort := round(base_mort, 3)] %>%
#   clean_fit_dt(c("coef", "type", "inst", "base_mort"), sig = T) %>%
#   dcast(type + coef + base_mort ~ inst, value.var = "est_se") %>%
#   .[order(type, coef), ] %>%
#   .[, lab := c("2007-2010", "2011-2012", "1-70th percentile",
#                "71-97th Percentile", "98-100th percentile",
#                "2007-2010", "2011-2012")] %>%
#   .[, .(lab, base_mort, first_mo, cov_per_month100)] %>%
#   .[, lab := paste0("\\hspace{5 mm}", lab)]
#
# print(xtable(dt_all[3:5, ], digits = 3),
#       include.rownames = F, sanitize.text.function = force)
#
# print(xtable(dt_all[1:2, ], digits = 3),
#       include.rownames = F, sanitize.text.function = force)
#
# print(xtable(dt_all[6:7, ], digits = 3),
#       include.rownames = F, sanitize.text.function = force)
#
# end_log_file()
