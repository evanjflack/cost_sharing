# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Makes fine binned "first stage" plot of EOY coinsurance rate on
#       enrollment month

# Start Script -----------------------------------------------------------------

library(ggplot2)
library(estimatr)

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

# Start log file
start_log_file("log/03_fine_binned_price_fs")

# Read In Data -----------------------------------------------------------------

# Main data set
model_vars <- c("bene_id", "first_mo", "initial_cost_90",
                "rfrnc_yr", "race", "sex", "cntrct_pbp_rfrnc_yr")
DT_model <- fread(paste0(lib_base_data, "revision_model_data_", pct,
                         ".csv")) %>%
  .[, model_vars, with = FALSE]
rm(model_vars)

cost_wide <- fread(paste0(lib_base_data, "sample_spending_by_month_", pct,
                          ".csv")) %>%
  .[, .(bene_id, cum_cost_1_12, cum_plan_oop_cost_1_12)]

icl_oopt <- fread(paste0(lib_base_data, "icl_oopt_std_amts_sample.csv"))

DT_model %<>%
  merge(cost_wide, by = "bene_id") %>%
  merge(icl_oopt, by = "rfrnc_yr")

DT_model %<>%
  .[, pre := ifelse(cum_cost_1_12 <= icl_amt, 1, 0)] %>%
  .[, `:=`(icl = ifelse(cum_cost_1_12 > icl_amt &
                          cum_plan_oop_cost_1_12 <= oopt_amt, 1, 0),
           cat = ifelse(cum_plan_oop_cost_1_12 > oopt_amt, 1, 0))] %>%
  .[, arm := ifelse(icl == 1, "icl", ifelse(cat == 1, "cat", "pre"))]

# Prep Data --------------------------------------------------------------------
cut1 <- .7
DT_model %<>%
  .[, pred_cut2 := cut(initial_cost_90,
                       breaks = c(-Inf, quantile(initial_cost_90,
                                                 c(seq(.2, cut1, .1),
                                                   seq(cut1 + .01, 1, .01)),
                                  Inf)),
                       labels = c(seq(20, cut1*100, 10),
                                  seq(cut1 * 100 + 1, 100, 1))),
    by = first_mo]

# Estimate Fine-Binned RF ------------------------------------------------------

fit <- lm_robust(icl ~ first_mo:factor(pred_cut2) +
                   factor(pred_cut2),
                 data = DT_model, se_type = "stata")

dt_fit <- fit_to_dt(fit, "first_mo",
                                 "pred_cut2") %>%
  .[, `:=`(estimate = estimate * 100,
           std.error = std.error * 100)] %>%
  .[, lb := estimate - 1.96 * std.error] %>%
  .[, ub := estimate + 1.96 * std.error] %>%
  .[, pred_cut2 := as.numeric(as.character(pred_cut2))] %>%
  .[, type := "December Coinsurance Rate"] %>%
  .[, arm := ifelse(pred_cut2 <= 70, "pre",
                    ifelse(pred_cut2 <= 97, "gap", "cat"))]

# Plot -------------------------------------------------------------------------

p_fs <- ggplot(dt_fit) +
  aes(x = pred_cut2, y = estimate, ymin = lb, ymax = ub, color = factor(arm)) +
  geom_point(size= .5) +
  geom_errorbar(alpha = .5) +
  geom_hline(yintercept = 0, linetype = 3, alpha = .5) +
  scale_color_manual(values = c(cat_col, dh_col, pre_col)) +
  labs(x = "Initial 90-Day Total Spending Percentile",
       y = "Donut Hole Entry\nReduced Form Estimate (p.p.)") +
  my_theme_paper +
  scale_x_continuous(breaks = seq(10, 90, 20)) +
  theme(legend.position = "none")

# Export -----------------------------------------------------------------------

ggsave(paste0(lib_base, "plots/appendix/a3_fine_binned_rf.png"), p_fs,
       width = 6, height = 3)

end_log_file()
