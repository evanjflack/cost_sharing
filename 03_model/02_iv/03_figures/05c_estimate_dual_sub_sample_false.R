# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Estimates falsification reduced form in non-random subsets of older dual
#.      sample

# Start Script -----------------------------------------------------------------
source("../../../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type='character', default = "20pct"),
  make_option(c("-f", "--first_year"), type='integer', default = 2007),
  make_option(c("-l", "--last_year"), type='integer', default = 2012)
)
unpack_opt(option_list)

# Start log file
start_log_file("log/05c_estimate_dual_sub_sample_false")

library(stringr)
library(estimatr)
library(foreach)

# Read in Data -----------------------------------------------------------------
message("Reading in data...")

# Dual 66-85 sample
dual_sample <- read_and_combine(lib_base_data, "dual_older_sample", years, pct)

init_cost <- read_and_combine(lib_base_data, "old_false_init_cost", years, pct)

DT_model <- dual_sample %>%
  .[month == 12, ] %>%
  .[birth_mo %in% seq(2, 9)] %>%
  merge(init_cost, by = c("bene_id", "rfrnc_yr"), all.x = T) %>%
  .[is.na(initial_cost3), initial_cost3 := 0] %>%
  .[, .(bene_id, rfrnc_yr, age1, mort, birth_mo, initial_cost3, race, sex,
        state_cd, cntrct, pbp)]

DT_model %<>%
  .[, pred_cut2 := cut(initial_cost3,
                       breaks = c(-Inf, quantile(initial_cost3, seq(.2, .8, .2)), Inf),
                       labels = as.character(seq(1, 5)))]

top_states <- DT_model[, .(.N), by = state_cd] %>%
  .[order(-N), ] %>%
  .[, state_rank := seq(1, 51)] %>%
  .[N >= 10000, ] %>%
  .[, N := NULL]

DT_model %<>%
  merge(top_states, by = "state_cd")

DT_model %<>%
  .[, white := ifelse(race == 1, 1, 0)] %>%
  .[, female := ifelse(sex == 2, 1, 0)]

state_grid <- data.table(var = "state_rank",
                         cond = seq(1, nrow(top_states)))
spend_grid <- data.table(var = "pred_cut2",
                         cond = c(1, 2, 3, 4, 5))
sex_grid <- data.table(var = "female",
                       cond = c(0, 1))
race_grid <- data.table(var = "white",
                        cond = c(0, 1))
grid <- rbind(state_grid, spend_grid, sex_grid,
              race_grid)

dt_fit <- foreach(var = grid$var,
                  cond = grid$cond,
                  .combine = 'rbind') %do%
  {
    DT_fit <- copy(DT_model) %>%
      .[, var := get(var)] %>%
      .[var == cond, ]

    fit <- lm_robust(mort ~ birth_mo + factor(rfrnc_yr),
                     data = DT_fit, se_type = "stata")

    dt_fit1 <- fit_to_dt(fit, "birth_mo") %>%
      .[, base_mort := mean(DT_fit$mort)] %>%
      .[, obs := nrow(DT_fit)] %>%
      .[, var := var] %>%
      .[, cond := cond] %>%
      .[, .(var, cond, obs, base_mort, estimate, std.error, p.value)]
  }

# Export -----------------------------------------------------------------------

fwrite(dt_fit, paste0(lib_base_data, "dual_sub_sample_rf_estimates.csv"))

end_log_file()
