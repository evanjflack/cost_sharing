# Header -----------------------------------------------------------------------
# Author: Evan Flack (flack@stanford.edu)
# Desc: Makes Table 2 Panel A (balance on demographics/utilization)

# Start Script -----------------------------------------------------------------
source("../../../00_pre_process/start_script.R")

library(estimatr)
library(xtable)
library(stringr)

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct")
)
unpack_opt(option_list)

options(scipen = 999)

start_log_file("log/02a_dem_balance.R")

# Functions --------------------------------------------------------------------

round_sum_stats <- function(x) {
  ifelse(x > 100, format(round(x, 0), digits = 4, big.mark = ","),
         ifelse(x > 50, format(round(x, 1), digits = 4, big.mark = ","),
                format(signif(x, digits = 3)))) %>%
    as.character()
}

# Read in Data -----------------------------------------------------------------

# Final sample
model_vars <- c("bene_id", "race", "sex", "first_mo", "pred_cut1")
DT_model <- fread(paste0(lib_base_data, "revision_model_data_", pct,
                         ".csv")) %>%
  .[, model_vars, with = FALSE]
rm(model_vars)

# Predicted moratlity
pred_mort <- readRDS(paste0(lib_base_data,
                            "ensemble_pred_new_enrollee_dual_false_",
                            "mort", "_", "all", "_", 66, "_", 90,
                            "_days_", pct, ".rds")) %>%
  .[, .(bene_id, ensemble_pred)] %>%
  setnames("ensemble_pred", "ensemble_pred_mort")

# Claims in first 90 days
initial_claims <- fread(paste0(lib_base_data,
                               "initial_part_d_claims_new_enrollee_", pct,
                               ".csv")) %>%
  .[, .(bene_id, n_claims_90)]

# Merge and define indicators
DT_model %<>%
  merge(pred_mort, by = "bene_id") %>%
  merge(initial_claims, by = "bene_id") %>%
  .[, white := ifelse(race == 1, 1, 0)] %>%
  .[, female := ifelse(sex == 2, 1, 0)]

# Estimate Balance -------------------------------------------------------------

bal_vars <- c("white", "female",  "n_claims_90", "ensemble_pred_mort")

bal_labs <- c("White (\\%)", "Female (\\%)",
              "Initial 90-day fills (count)",
              "Predicted mortality (p.p)")

dt_fit <- data.table()
for (i in bal_vars) {
  message(i)
  DT_fit <- copy(DT_model) %>%
    .[, y := get(i)]

  fit_all <- lm_robust(y ~ first_mo, data = DT_fit, se_type = "stata")

  dt_fit_all <- fit_to_dt(fit_all, "first_mo") %>%
    .[, .(estimate, std.error, p.value)] %>%
    .[, var := i] %>%
    .[, type := "all"] %>%
    .[, mean := mean(DT_fit$y, na.rm = T)]

  fit_mid <- lm_robust(y ~ first_mo, data = DT_fit[pred_cut1 == 2],
                       se_type = "stata")

  dt_fit_mid <- fit_to_dt(fit_mid, "first_mo") %>%
    .[, .(estimate, std.error, p.value)] %>%
    .[, var := i] %>%
    .[, type := "mid"] %>%
    .[, mean := mean(DT_fit[pred_cut1 == 2, y], na.rm = T)]

  dt_fit <- dt_fit %>%
    rbind(dt_fit_mid) %>%
    rbind(dt_fit_all)
}

# Format and Print -------------------------------------------------------------

dt_print <- copy(dt_fit) %>%
  .[var != "n_claims_90", `:=`(mean = mean*100, estimate = estimate*100,
                               std.error = std.error*100)] %>%
  .[, var := factor(var, levels = bal_vars, labels = bal_labs)] %>%
  .[, mean := sapply(mean, round_sum_stats)] %>%
  .[, .(var, type, mean, estimate, std.error, p.value)] %>%
  .[order(var, type), ] %>%
  clean_fit_dt(c("var", "type", "mean"), dig = 3) %>%
  dcast(var ~ type, value.var = c("mean", "est_se")) %>%
  .[, .(var, mean_all, est_se_all, mean_mid, est_se_mid)] %>%
  .[order(var), ] %>%
  .[, var := paste0("\\hspace{5 mm}", var)]

print(xtable(dt_print), include.rownames = FALSE,
      sanitize.text.function = force)

end_log_file()
