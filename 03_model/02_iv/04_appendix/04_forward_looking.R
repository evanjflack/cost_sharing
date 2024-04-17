# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Make appendix table A2 (Forward looking behavior)

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

# Start log file
start_log_file("log/04_forward_looking.R")

# Read in Data -----------------------------------------------------------------

# Main data
model_vars <- c("bene_id", "race", "sex", "cntrct",
                "first_mo", "pred_cut1", "rfrnc_yr", "cntrct_pbp_rfrnc_yr")
DT_model <- fread(paste0(lib_base_data, "revision_model_data_", pct,
                         ".csv")) %>%
  .[, model_vars, with = FALSE]
rm(model_vars)

init_cost <- fread(paste0(lib_base_data,
                          "initial_part_d_spending_new_enrollee_", pct,
                          ".csv")) %>%
  .[, c("bene_id", paste0("initial_cost_", seq(30, 270, 30))), with = FALSE]

# Prep Data --------------------------------------------------------------------

DT_model %<>%
  merge(init_cost, by = "bene_id")

DT_model %<>%
  .[, cost_30 := initial_cost_30]

for (i in seq(60, 120, 30)) {
  DT_model %<>%
    .[, paste0("cost_", i) := get(paste0("initial_cost_", i)) -
        get(paste0("initial_cost_", i - 30))]
}

# Estimate RF ------------------------------------------------------------------

dt_fit <- data.table()
for (i in seq(30, 120, 30)) {
  print(i)

  DT_fit <- copy(DT_model) %>%
    # .[first_mo <= max_first_mo] %>%
    .[, y1 := get(paste0("cost_", i))] %>%
    .[, y2 := get(paste0("initial_cost_", i))]

  fit1 <- lm_robust(y1 ~ first_mo,
                    data = DT_fit,
                    se_type = "stata")

  dt_fit1 <- tidy(fit1) %>%
    as.data.table() %>%
    .[term == "first_mo"] %>%
    .[, type := "30"] %>%
    .[, mean := mean(DT_fit$y1)]

  fit2 <- lm_robust(y2 ~ first_mo,
                    data = DT_fit)
  dt_fit2 <- tidy(fit2) %>%
    as.data.table() %>%
    .[term == "first_mo"] %>%
    .[, type := "cum"] %>%
    .[, mean := mean(DT_fit$y2)]

  dt_fit3 <- rbind(dt_fit1, dt_fit2) %>%
    .[, days := i] %>%
    .[, obs := nrow(DT_fit)] %>%
    .[, .(days, type, obs, mean, estimate, std.error, p.value)]

  dt_fit %<>% rbind(dt_fit3)
}

dt_fit %<>%
  .[order(type, days)]

# Preint -----------------------------------------------------------------------

dt_print <- copy(dt_fit) %>%
  .[, mean := as.character(round(mean, 1))] %>%
  clean_fit_dt(c('type', "days", "mean")) %>%
  .[, days := factor(days, labels = c("1-30", "31-60", "61-90", "91-120"))] %>%
  dcast(days ~ type, value.var = c("mean", "est_se")) %>%
  .[, days_cum := c("1-30", "1-60", "1-90", "1-120")] %>%
  .[, .(days, mean_30, est_se_30, days_cum, mean_cum, est_se_cum)]

print(xtable(dt_print, digits = 3),
      include.rownames = F, sanitize.text.function = force)

end_log_file()
