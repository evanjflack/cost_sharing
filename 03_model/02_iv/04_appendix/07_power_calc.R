# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Makes appendix figure C.3 (power relative to December year 1)

# Start Script -----------------------------------------------------------------

library(ggplot2)
library(pwrss)

source("../../../00_pre_process/start_script.R")
source("../../../00_pre_process/define_plot_theme.R")


# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct")
)

unpack_opt(option_list)


# Start log file
start_log_file("log/07_power_calc")

# Read In Data -----------------------------------------------------------------

dt_fit <- fread(paste0(lib_base_data, "year_1_2_mort_days_icl_rf_estimates_",
                       pct, ".csv")) %>%
  .[outcome == "days", ]

# Main model data
model_vars <- c("bene_id", "first_mo", "rfrnc_yr", "pred_cut1")
DT_model <- fread(paste0(lib_base_data, "revision_model_data_", pct,
                         ".csv")) %>%
  .[, model_vars, with = FALSE]
rm(model_vars)

mort_outcomes <- fread(paste0(lib_base_data, "mort_outcomes_jun_year_1_", pct,
                              ".csv")) %>%
  .[, .(bene_id, jun_mort_7)] %>%
  setnames("jun_mort_7", "dec_mort")


DT_model %<>%
  merge(mort_outcomes, by = "bene_id")


# Power Calc -------------------------------------------------------------------

fit <- lm_robust(dec_mort ~ first_mo,
                 data = DT_model[pred_cut1 == 2],
                 se_type = "stata")

r2 <- summary(fit)$r.squared
beta <- fit$coefficients[2]
sdx = sd(DT_model[pred_cut1 == 2, first_mo])
sdy = sd(DT_model[pred_cut1 == 2, dec_mort])


dtp <- copy(dt_fit) %>%
  .[, .(month, obs, obs1, estimate)] %>%
  # .[, estimate := estimate / 100] %>%
  .[, dec_est := .[month == 12, estimate]] %>%
  .[, scale  := (estimate / dec_est)] %>%
  .[, scale_beta := scale * beta]



get_n <- function(x, beta) {
  n <- pwrss.t.reg(beta1 = beta, k = 3, r2 = r2, sdx = sdx, sdy = sdy,
                   power = x, alpha = 0.05, alternative = "not equal")$n

  return(n)
}

dtp[, power := .1]
for (i in 1:nrow(dt_fit)) {

  dt_power <- data.table()
  for (x in seq(0.1, .99, .01)) {
    dt_power1 <- data.table(power = x, n = get_n(x, beta = dtp[i, scale_beta]))
    dt_power %<>% rbind(dt_power1)
  }

  ret <- abs(dtp[i, obs1] - dt_power$n)

  ind <- which(ret == min(ret))

  dtp[i, power := dt_power$power[ind]]
}

dec_power <- dtp[month == 12, power]
dtp[, norm_power := power / dec_power]

dtp[power == .1, norm_power := .1]


ggplot(dtp) +
  aes(x = month, y = norm_power) +
  geom_point() +
  geom_line(linetype = 2) +
  labs(x = "Calendar Month", y = "Power to Detect Mortality Effect\n(relative to December,\nscaled by days filled estimate)") +
  scale_x_continuous(breaks = seq(8, 17, 1),
                     labels = c("Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May")) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(.1, .9, .2),
                     labels = c("< 0.1", "0.3", "0.5", "0.7", "0.9")) +
  geom_vline(xintercept = 12.5, linetype = 3) +
  my_theme_paper

ggsave(paste0(lib_base, "plots/appendix/c3_power.png"),
       width = 6, height = 3)

end_log_file()