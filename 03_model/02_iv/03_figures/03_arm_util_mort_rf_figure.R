# Proj: Cost Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Makes figure 3: reduced form plots

# Start Script -----------------------------------------------------------------

library(ggplot2)
library(estimatr)
library(stringr)

source("../../../00_pre_process/start_script.R")
source("../../../00_pre_process/define_plot_theme.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct")
)
unpack_opt(option_list)

# Additional user inputs
# Color scheme + x-axis labels
pre_col <- "lightblue3"
dh_col <- "red3"
cat_col <- "dodgerblue4"
month_labs <- c("Feb", "May",  "Aug")

# Start log file
start_log_file("log/03_arm_util_rf_figure")

# Read In Data -----------------------------------------------------------------
message("Reading in data...")

# Variables needed from model data
model_vars <- c("bene_id", "first_mo", "pred_cut1", "rfrnc_yr", "race", "sex",
                "cntrct_pbp_rfrnc_yr")
DT_model <- fread(paste0(lib_base_data, "revision_model_data_", pct,
                         ".csv")) %>%
  .[, model_vars, with = FALSE]
rm(model_vars)

cost_wide <- fread(paste0(lib_base_data, "sample_spending_by_month_", pct,
                          ".csv")) %>%
  .[, .(bene_id, cum_cost_1_12, cum_plan_oop_cost_1_12)]

mort_outcomes <- fread(paste0(lib_base_data, "mort_outcomes_jun_year_1_", pct,
                              ".csv")) %>%
  .[, .(bene_id, jun_mort_7)] %>%
  setnames("jun_mort_7", "dec_mort")

claims_mo <- fread(paste0(lib_base_data,
                          "part_d_claims_pill_days_mo_new_enrollee_",
                          pct, ".csv")) %>%
  .[, .(bene_id, pill_days_1_12)]

icl_oopt <- fread(paste0(lib_base_data, "icl_oopt_std_amts_sample.csv"))

# Prep Data --------------------------------------------------------------------

DT_model %<>%
  merge(cost_wide, by = "bene_id") %>%
  merge(icl_oopt, by = "rfrnc_yr") %>%
  merge(claims_mo, by = "bene_id") %>%
  merge(mort_outcomes, by = "bene_id")

# End of year coverage arm
DT_model %<>%
  .[, pre := ifelse(cum_cost_1_12 <= icl_amt, 1, 0)] %>%
  .[, `:=`(icl = ifelse(cum_cost_1_12 > icl_amt &
                          cum_plan_oop_cost_1_12 <= oopt_amt, 1, 0),
           cat = ifelse(cum_plan_oop_cost_1_12 > oopt_amt, 1, 0))] %>%
  .[, arm := ifelse(icl == 1, "icl", ifelse(cat == 1, "cat", "pre"))]

# Monthly pre-gap coverage
DT_model %<>%
  .[, months_remain := 12 - first_mo + 1] %>%
  .[, cov_per_month := icl_amt / months_remain]

# Labels for per month coverage
dtp_cov <- calc_cmean(DT_model, y = "cov_per_month",
                      x = c("first_mo")) %>%
  .[, .(first_mo, mean)] %>%
  .[, mean := round(mean, 0)] %>%
  setnames("mean", "cov_per_month") %>%
  .[order(first_mo), ]

month_labs_cov <- paste0(month_labs, "\n($",
                         dtp_cov[first_mo %in% c(2, 5, 8), cov_per_month], ")")

# End of Year Arm --------------------------------------------------------------

dtp_icl_cat <- calc_cmean(DT_model, c("pre", "icl", "cat"),
                          c("first_mo", "pred_cut1"), se = T) %>%
  .[, arm := factor(variable, levels = c("pre", "icl", "cat"),
                    labels = c("Initial Coverage", "Donut Hole",
                               "Catastrophic"))] %>%
  .[, `:=`(mean = mean*100, se = se*100)] %>%
  .[, `:=`(lb = mean - 1.96*se, ub = mean + 1.96*se)] %>%
  .[, pred_cut1 := factor(pred_cut1, labels = c("Low-Spenders",
                                                "Middle-Spenders",
                                                "High-Spenders"))]

p_arm <- ggplot(dtp_icl_cat[arm != "Initial Coverage"]) +
  aes(x = first_mo, y = mean, color = factor(arm), ymin = lb, ymax = ub) +
  geom_line() +
  geom_point() +
  geom_errorbar(width = 0, alpha = .5) +
  facet_wrap(~ pred_cut1, nrow = 1) +
  scale_color_manual(values = c(dh_col, pre_col)) +
  scale_x_continuous(breaks = c(2, 5, 8),
                     labels = month_labs) +
  labs(x = "Enrollment Month ($ Pre-Gap Coverage per Month)",
       y = "\n% of Enrollees in Coverage Arm") +
  my_theme_paper +
  theme(legend.title = element_blank(),
        legend.position = c(.16, .5),
        legend.background = element_rect(fill = "transparent"))

ggsave(paste0(lib_base, "plots/03a_eoy_coverage_arm.png"), p_arm,
       width = 6, height = 2.3)

fit_arm <- lm_robust(icl ~ first_mo:factor(pred_cut1) + factor(pred_cut1),
                     data = DT_model)

icl_est <- fit_to_dt(fit_arm, "first_mo", "pred_cut1") %>%
  .[pred_cut1 == 2, estimate]

message("Donut hole per enrollment month: ", round(icl_est * 100, 3), " p.p.")

# Drug Days --------------------------------------------------------------------

fit_days_pred <- lm(pill_days_1_12 ~ first_mo*factor(pred_cut1),
                    data = DT_model)
dtp_days <- calc_cmean(DT_model, "pill_days_1_12",
                            c("first_mo", "pred_cut1"), se = T) %>%
  .[, pred := predict(fit_days_pred, .)] %>%
  .[, pred_se := predict(fit_days_pred, ., se.fit = T)$se.fit] %>%
  .[, pred_lb := pred - 1.96*pred_se] %>%
  .[, pred_ub := pred + 1.96*pred_se]

p_days <- ggplot(dtp_days) +
  aes(x = first_mo, y = mean, ymin = lb, ymax = ub,
      color = factor(pred_cut1), fill = factor(pred_cut1)) +
  geom_point() +
  geom_errorbar() +
  geom_line(aes(y = pred)) +
  geom_ribbon(aes(ymin = pred_lb, ymax = pred_ub), color = NA, alpha = .5) +
  facet_wrap(~ pred_cut1, ncol = 4) +
  scale_x_continuous(breaks = c(2, 5, 8),
                     labels = month_labs) +
  scale_y_continuous(limits = c(0, 205)) +
  scale_color_manual(values = c(pre_col, dh_col, cat_col)) +
  scale_fill_manual(values = c(pre_col, dh_col, cat_col)) +
  labs(x = "Enrollment Month", y = "Days Filled in December") +
  my_theme_paper +
  theme(legend.position = "none",
        strip.text = element_blank(),
        axis.title.x = element_blank())

ggsave(paste0(lib_base, "plots/03b_dec_days.png"), p_days,
       width = 6, height = 2)

# Mortality  -------------------------------------------------------------------

fit_mort <- lm_robust(dec_mort ~ first_mo*factor(pred_cut1),
                      data = DT_model, se_type = "stata")

dtp_mort <- calc_cmean(DT_model, "dec_mort",
                       c("first_mo", "pred_cut1"), se = T) %>%
  .[, `:=`(mean = mean*100, se = se*100)] %>%
  .[, `:=`(lb = mean - 1.96*se, ub = mean + 1.96*se)] %>%
  .[, pred_cut1 := as.character(pred_cut1)] %>%
  .[, `:=`(pred = predict(fit_mort, .)*100,
           pred_se = predict(fit_mort, ., se.fit = T)$se.fit*100)] %>%
  .[, `:=`(pred_lb = pred - 1.96*pred_se,
           pred_ub = pred + 1.96*pred_se)]

dtp_mort %<>%
  .[ub >= .7 , ub := .7]

# Mortality
p_mort <- ggplot(dtp_mort) +
  aes(x = first_mo, y = mean, ymin = lb, ymax = ub,
      color = factor(pred_cut1), fill = factor(pred_cut1)) +
  geom_point() +
  geom_errorbar(width = 0) +
  geom_line(aes(y = pred)) +
  geom_ribbon(aes(ymin = pred_lb, ymax = pred_ub), color = NA, alpha = .5) +
  facet_wrap(~ pred_cut1) +
  scale_x_continuous(breaks = c(2, 5, 8),
                     labels = month_labs_cov) +
  scale_color_manual(values = c(pre_col, dh_col, cat_col)) +
  scale_fill_manual(values = c(pre_col, dh_col, cat_col)) +
  labs(x = "Enrollment Month ($ Monthly Pre-Donut Hole Budget)",
       y = "December Mortality (p.p)") +
  my_theme_paper +
  theme(legend.position = "none",
        strip.text = element_blank())

ggsave(paste0(lib_base, "plots/03c_dec_mort.png"), p_mort,
       width = 6, height = 2.8)

end_log_file()
