# Header -----------------------------------------------------------------------
# Proj: Cost Sharing
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Plots falsification CDFs

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
start_log_file("log/05d_plot_false_dist")

# Read In Data -----------------------------------------------------------------

dt_fit_non_dual <- fread(paste0(lib_base_data,
                                "oos_false_estimates_non_dual_birth_mo_20pct.csv")) %>%
  .[type == "pred" & pred_cut1 == "2", ] %>%
  .[, .(estimate, std.error, base_mort)] %>%
  .[, sample := "old_dis_dual"]

dt_fit_dis <- fread(paste0(lib_base_data,
                           "oos_false_estimates_dis_birth_mo_20pct.csv")) %>%
  .[type == "pred" & pred_cut1 == "2", ] %>%
  .[, .(estimate, std.error, base_mort)] %>%
  .[, sample := "old_dis_dual"]

dt_fit_dual <- fread(paste0(lib_base_data,
                            "oos_false_estimates_dual_birth_mo_20pct.csv")) %>%
  .[type == "pred" & pred_cut1 == "2", ] %>%
  .[, .(estimate, std.error, base_mort)] %>%
  .[, sample := "old_dis_dual"]

dt_fit_dual_sub <- fread(paste0(lib_base_data,
                            "dual_sub_sample_rf_estimates.csv")) %>%
  .[, .(estimate, std.error, base_mort)] %>%
  .[, sample := 'dual_sub']

dt_fit_ws <- fread(paste0(lib_base_data, "ws_false_estimates_", pct,
                          ".csv")) %>%
  setnames("mean", "base_mort") %>%
  .[month >= 8, ] %>%
  .[inst == "first_mo"] %>%
  .[type == "pred" & pred_cut1 == "2" ] %>%
  .[, .(estimate, std.error, base_mort)] %>%
  .[, sample := 'year_2_5']

main_est <- fread(paste0(lib_base_data, "main_first_mo_est.csv")) %>%
  .[, sample := "main"] %>%
  .[, type := 1]

dt_fit_all <- rbind(dt_fit_dis, dt_fit_dual, dt_fit_non_dual,
                    dt_fit_dual_sub, dt_fit_ws) %>%
  .[, type := 0] %>%
  rbind(main_est) %>%
  .[, perc := estimate / base_mort] %>%
  .[, tstat := estimate / std.error] %>%
  .[, perc := perc * 100]

round(mean(abs(dt_fit_all[type == 1, perc]) >= abs(dt_fit_all[type !=1, perc])),
      3)

round(mean(dt_fit_all[type == 1, perc] <= dt_fit_all[type !=1, perc]), 3)

n_abs_large <- sum(abs(dt_fit_all[type == 1, perc]) >= abs(dt_fit_all[type !=1, perc]))

message(n_abs_large, '/', nrow(dt_fit_all[type != 1]))

n_small <- sum(dt_fit_all[type == 1, perc] <= dt_fit_all[type !=1, perc])

message(n_small, '/', nrow(dt_fit_all[type != 1]))

median(dt_fit_all[type !=1, perc])

# Percentage Estimate ----------------------------------------------------------

dt_fit_all %<>%
  .[order(perc), ] %>%
  .[, ord := seq(1, .N)] %>%
  .[, cdf := ord/nrow(.)]

lines <- quantile(dt_fit_all[type != 2, perc],c(.025, .975))

ggplot(dt_fit_all) +
  aes(x = perc, y = cdf) +
  geom_point(aes(size = factor(type), color = factor(type),
                 shape = factor(type))) +
  geom_line(alpha = .2) +
  geom_vline(xintercept = lines, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 3) +
  scale_size_manual(values = c(.5, 3)) +
  scale_color_manual(values = c("gray", "red3")) +
  scale_alpha_manual(values  = c(.5, 1)) +
  scale_shape_manual(values = c(1, 16)) +
  scale_x_continuous(limits = c(-17, 17)) +
  annotate(geom = "text", x = -10.2, y = .12, label = "Main Est.") +
  labs(x = "% Enrollment Month Estimate",
       y = "P(% Estimate \u2264 x)") +
  my_theme_paper +
  guides(size = guide_legend(override.aes = list(linetype = 0))) +
  theme(legend.title = element_blank(),
        legend.position = "none")

ggsave(paste0(lib_base, "plots/05_all_false_perc.png"),
       width = 6, height = 3)

 # T-statistic
dt_fit_all %<>%
  .[order(tstat), ] %>%
  .[, ord := seq(1, .N)] %>%
  .[, cdf := ord/nrow(.)]

lines <- quantile(dt_fit_all[type != 2, tstat],c(.025, .975))

ggplot(dt_fit_all) +
  aes(x = tstat, y = cdf) +
  geom_point(aes(size = factor(type), color = factor(type),
                 shape = factor(type))) +
  geom_line(alpha = .2) +
  geom_vline(xintercept = lines, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 3) +
  scale_size_manual(values = c(.5, 3)) +
  scale_color_manual(values = c("gray", "red3")) +
  scale_alpha_manual(values  = c(.5, 1)) +
  scale_shape_manual(values = c(1, 16)) +
  scale_x_continuous(limits = c(-4, 4),
                     breaks = seq(-4, 4)) +
  annotate(geom = "text", x = -3.2, y = .1, label = "Main T-Stat") +
  labs(x = "Enrollment Month T-Statistic",
       y = "P(T-Statistic \u2264 x)") +
  my_theme_paper +
  guides(size = guide_legend(override.aes = list(linetype = 0))) +
  theme(legend.title = element_blank(),
        legend.position = "none")

ggsave(paste0(lib_base, "plots/appendix/c2_all_false_tstat.png"),
       width = 6, height = 3)

# Older Sample Distribution ----------------------------------------------------
message("Plotting out of sample CDFs...")

dtp_old <- dt_fit_all %>%
  .[sample %in% c("old_dis_dual", "main")] %>%
  .[order(perc), ] %>%
  .[, ord := seq(1, .N)] %>%
  .[, cdf := ord/nrow(.)]

lines <- quantile(dtp_old[sample!= "main", perc],
                         c(.025, .975))

ggplot(dtp_old) +
  aes(x = perc, y = cdf) +
  geom_point(aes(size = factor(type), color = factor(type),
                 shape = factor(type))) +
  geom_line(alpha = .2) +
  geom_vline(xintercept = lines, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 3) +
  scale_size_manual(values = c(.5, 3)) +
  scale_color_manual(values = c("gray", "red3")) +
  scale_alpha_manual(values  = c(.5, 1)) +
  scale_shape_manual(values = c(1, 16)) +
  annotate(geom = "text", x = -12, y = .1, label = "Main Est.") +
  labs(x = "% Enrollment Month Estimate",
       y = "P(% Estimate \u2264 x)") +
  my_theme_paper +
  guides(size = guide_legend(override.aes = list(linetype = 0))) +
  theme(legend.title = element_blank(),
        legend.position = "none")

ggsave(paste0(lib_base, "plots/appendix/c1c_old_dual_dis_false.png"),
       width = 6, height = 3)

# Dual Sub-Sample --------------------------------------------------------------

dtp_dual_sub <- dt_fit_all %>%
  .[sample %in% c("dual_sub", "main")] %>%
  .[order(perc), ] %>%
  .[, ord := seq(1, .N)] %>%
  .[, cdf := ord/nrow(.)]

lines <- quantile(dtp_dual_sub[sample!= "main", perc],
                  c(.025, .975))

ggplot(dtp_dual_sub) +
  aes(x = perc, y = cdf) +
  geom_point(aes(size = factor(type), color = factor(type),
                 shape = factor(type))) +
  geom_line(alpha = .2) +
  geom_vline(xintercept = lines, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 3) +
  scale_size_manual(values = c(.5, 3)) +
  scale_color_manual(values = c("gray", "red3")) +
  scale_alpha_manual(values  = c(.5, 1)) +
  scale_shape_manual(values = c(1, 16)) +
  annotate(geom = "text", x = -10, y = .1, label = "Main Est.") +
  labs(x = "% Enrollment Month Estimate",
       y = "P(% Estimate \u2264 x)") +
  scale_x_continuous(limits = c(-12, 12)) +
  my_theme_paper +
  guides(size = guide_legend(override.aes = list(linetype = 0))) +
  theme(legend.title = element_blank(),
        legend.position = "none")

ggsave(paste0(lib_base, "plots/appendix/c1b_dual_sub_false.png"),
       width = 6, height = 3)

# Within-Sample ----------------------------------------------------------------
message("Plotting within-sample other months CDFs...")

dtp_year2<- dt_fit_all %>%
  .[sample %in% c("year_2_5", "main")] %>%
  .[order(perc), ] %>%
  .[, ord := seq(1, .N)] %>%
  .[, cdf := ord/nrow(.)]

lines <- quantile(dtp_year2[sample!= "main", perc],
                  c(.025, .975))

ggplot(dtp_year2) +
  aes(x = perc, y = cdf) +
  geom_point(aes(size = factor(type), color = factor(type),
                 shape = factor(type))) +
  geom_line(alpha = .2) +
  geom_vline(xintercept = lines, linetype = 2) +
  geom_vline(xintercept = 0, linetype = 3) +
  scale_size_manual(values = c(.5, 3)) +
  scale_color_manual(values = c("gray", "red3")) +
  scale_alpha_manual(values  = c(.5, 1)) +
  scale_shape_manual(values = c(1, 16)) +
  annotate(geom = "text", x = -10.5, y = .12, label = "Main Est.") +
  labs(x = "% Enrollment Month Estimate",
       y = "P(% Estimate \u2264 x)") +
  my_theme_paper +
  guides(size = guide_legend(override.aes = list(linetype = 0))) +
  theme(legend.title = element_blank(),
        legend.position = "none")

ggsave(paste0(lib_base, "plots/appendix/c1a_year_2_5_false.png"),
       width = 6, height = 3)

# End --------------------------------------------------------------------------

end_log_file()
