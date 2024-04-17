# Header -----------------------------------------------------------------------
# Proj: Cost-Sharing
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Plots reduced form estimates from year 1 and 2

# Start Script -----------------------------------------------------------------
library(ggplot2)
source("../../../00_pre_process/start_script.R")
source("../../../00_pre_process/define_plot_theme.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct")
)
unpack_opt(option_list)

# Start log file
start_log_file("log/04b_plot_year_1_2_gap_util_mort_rf")

dt_fit <- fread(paste0(lib_base_data, "year_1_2_mort_days_icl_rf_estimates_",
                       pct, ".csv"))

month_labs <- c("Aug\n(Y1)", "Sep\n(Y1)", "Oct\n(Y1)", "Nov\n(Y1)", "Dec\n(Y1)",
                "Jan\n(Y2)", "Feb\n(Y2)", "Mar\n(Y2)", "Apr\n(Y2)", "May\n(Y2)")

# Coverage Gap -----------------------------------------------------------------
dt_icl <- dt_fit %>%
  .[outcome == "icl", ] %>%
  .[, `:=`(estimate = estimate * 100, std.error = std.error * 100)] %>%
  .[, `:=`(lb = estimate - 1.96*std.error, ub = estimate + 1.96*std.error)] %>%
  .[, dec := ifelse(month == 12, 1, 0)]

p_icl <- ggplot(dt_icl) +
  aes(x = month, y = estimate, ymin = lb, ymax = ub) +
  geom_point(aes(color = factor(dec))) +
  geom_line(linetype = 2) +
  geom_errorbar(width = 0, linewidth = 5, alpha = .5, aes(color = factor(dec))) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 12.5, linetype = 3) +
  labs(x = "Calendar Month", y = "Donut Hole Enrollment\nMonth Estimate (p.p.)") +
  scale_x_continuous(breaks = seq(8, 17, 1),
                     labels = month_labs) +
  scale_color_manual(values = c("dodgerblue4", "red3")) +
  annotate(geom = "text", x = 10, y = -2, label = "Year 1", size = 4) +
  annotate(geom = "text", x = 15, y = -2, label = "Year 2", size = 4) +
  my_theme_paper +
  theme(legend.position = "none")

ggsave(paste0(lib_base, "plots/04a_year_1_2_icl_rf.png"), p_icl,
       width = 6, height = 2.3)

# Utilization ------------------------------------------------------------------

dt_util <- dt_fit %>%
  .[outcome == "days", ] %>%
  .[, `:=`(lb = estimate - 1.96*std.error, ub = estimate + 1.96*std.error)] %>%
  .[, dec := ifelse(month == 12, 1, 0)]

p_util <- ggplot(dt_util) +
  aes(x = month, y = estimate, ymin = lb, ymax = ub) +
  geom_point(aes(color = factor(dec))) +
  geom_line(linetype = 2) +
  geom_errorbar(width = 0, linewidth = 2, alpha = .5, aes(color = factor(dec))) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 12.5, linetype = 3) +
  labs(x = "Calendar Month", y = "Days Filled Enrollment\nMonth Estimate") +
  scale_x_continuous(breaks = seq(8, 17, 1),
                     labels = month_labs) +
  scale_y_continuous(limits = c(-4, 4),
                     breaks = seq(-4, 4, 2),
                     labels = paste0("    ", seq(-4, 4, 2))) +
  scale_color_manual(values = c("dodgerblue4", "red3")) +
  annotate(geom = "text", x = 10, y = -3, label = "Year 1", size = 4) +
  annotate(geom = "text", x = 15, y = -3, label = "Year 2", size = 4) +
  my_theme_paper +
  theme(legend.position = "none")

ggsave(paste0(lib_base, "plots/04b_year_1_2_util_rf.png"), p_util,
       width = 6, height = 2.3)

# Percent change from nov to dec
nov_est <- dt_util[month == 11, estimate]
dec_est <- dt_util[month == 12, estimate]
perc_diff <- (dec_est - nov_est) / nov_est

message("December estimate is ", round(perc_diff * 100, 1),
        " % larger than November")

# Mortality --------------------------------------------------------------------

dt_mort <- dt_fit %>%
  .[outcome == "mort", ] %>%
  .[, `:=`(estimate = estimate*100, std.error = std.error*100)] %>%
  .[, `:=`(lb = estimate - 1.96*std.error, ub = estimate + 1.96*std.error)] %>%
  .[, dec := ifelse(month == 12, 1, 0)]

p_mort <- ggplot(dt_mort) +
  aes(x = month, y = estimate, ymin = lb, ymax = ub) +
  geom_point(aes(color = factor(dec))) +
  geom_line(linetype = 2) +
  geom_errorbar(width = 0, linewidth = 2, alpha = .5, aes(color = factor(dec))) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 12.5, linetype = 3) +
  scale_color_manual(values = c("dodgerblue4", "red3")) +
  labs(x = "Calendar Month", y = "Mortality Enrollment\nMonth Estimate (p.p)") +
  scale_x_continuous(breaks = seq(8, 17, 1),
                     labels = month_labs) +
  annotate(geom = "text", x = 10, y = -.02, label = "Year 1", size = 4) +
  annotate(geom = "text", x = 15, y = -.02, label = "Year 2", size = 4) +
  my_theme_paper +
  theme(legend.position = "none")

ggsave(paste0(lib_base, "plots/04c_year_1_2_mort_rf.png"), p_mort,
       width = 6, height = 2.3)

# End --------------------------------------------------------------------------

end_log_file()
