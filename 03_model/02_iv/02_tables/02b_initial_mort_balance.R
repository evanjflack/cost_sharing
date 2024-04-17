# Proj: Cost-sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Estimates reduced form of mortality on enrollment month in the first 90
#       days of enrollment

# Start Script -----------------------------------------------------------------
library(estimatr)
library(xtable)
library(foreach)
library(stringr)

source("../../../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type='character',
              default = "20pct")
)
unpack_opt(option_list)

options(scipen = 999)

# Start log file
start_log_file("log/02b_initial_mort_balance")

# Read in Data -----------------------------------------------------------------

# Sample of those in their enrollment month
first_mo_sample <- fread(paste0(lib_base_data, "enrollment_month_sample_", pct,
                                ".csv")) %>%
  .[, .(bene_id, first_mo, rfrnc_yr)]

# Mortality from enrollment
initial_mort_outcomes <- fread(paste0(lib_base_data,
                                      "initial_mortality_outcomes_", pct,
                                      ".csv")) %>%
  .[, first_mo := NULL]

# Initial 30-day spending
initial_spending <- fread(paste0(lib_base_data,
                                 "initial_part_d_spending_new_enrollee_", pct,
                                 ".csv")) %>%
  .[, .(bene_id, initial_cost_30)]

# Prep Data --------------------------------------------------------------------

DT_first <- first_mo_sample %>%
  merge(initial_mort_outcomes, by = "bene_id") %>%
  merge(initial_spending, by = "bene_id")

# Indicators for being alive after the first 30 days
DT_first %<>%
  .[, alive_60 := ifelse(cum_initial_mort_30 == 0, 1, 0)]

DT_first %<>%
  .[, year_cut := ifelse(rfrnc_yr <= 2010, "2007-2010", "2011-2012")]

# Define spending bin
DT_first %<>%
  .[, spend_pred := get(paste0("initial_cost_", 30))] %>%
  .[, pred_cut1 := ifelse(spend_pred <= quantile(spend_pred, .7),
                          1, ifelse(spend_pred <=
                                      quantile(spend_pred, .97), 2, 3)),
    by = first_mo]

DT_first %<>%
  .[, cum_initial_mort_60_90 := ifelse(initial_mort_60 + initial_mort_90 > 0,
                                       1, 0)]

# Entire Sample ----------------------------------------------------------------

days <- c(30, 60, 90)
dt_fit_all <- foreach(days = days,
                  .combine = "rbind") %do%
  {
    message(days)
    DT_fit <- copy(DT_first) %>%
      .[, y := get(paste0("cum_initial_mort_", days))]

    # Everyone
    fit_all <- lm_robust(y ~ first_mo,
                         data = DT_fit, se_type = "stata")
    dt_fit1 <- fit_to_dt(fit_all, "first_mo") %>%
      .[, .(estimate, std.error, p.value)] %>%
      .[, base_mort := mean(DT_fit$y)] %>%
      .[, `:=`(days = days)] %>%
      .[, pred_cut1 := "1-3"] %>%
      .[, obs := nrow(DT_fit)]
  }



# By Initial Spending ----------------------------------------------------------

vars <- c("initial_mort_60", "cum_initial_mort_60_90")
dt_fit_pred <- foreach(i = vars,
                      .combine = "rbind") %do%
  {
    message(i)
    DT_fit <- copy(DT_first) %>%
      .[alive_60 == 1, ] %>%
      .[, y := get(i)]

    # Everyone
    base_mort <- mean(DT_fit[pred_cut1 == 2, y])
    fit_pred <- lm_robust(y ~ first_mo:factor(pred_cut1) + factor(pred_cut1),
                          data = DT_fit, se_type = "stata")
    dt_fit1 <- fit_to_dt(fit_pred, "first_mo", "pred_cut1") %>%
      .[, .(estimate, std.error, p.value, pred_cut1)] %>%
      .[, `:=`(var = i, base_mort = base_mort)] %>%
      .[pred_cut1 == 2, ]
  }

dt_fit_pred %<>%
  .[, days := ifelse(var == "initial_mort_60", "60", "90")] %>%
  .[, var := NULL]

# Print ------------------------------------------------------------------------

dt_clean <- dt_fit_all %>%
  .[, obs:= NULL] %>%
  rbind(dt_fit_pred) %>%
  .[, days := as.character(days)] %>%
  .[, `:=`(estimate = estimate*100, std.error = std.error*100,
           base_mort = base_mort*100)] %>%
  .[, base_mort := round(base_mort, 4)] %>%
  .[, .(days, pred_cut1, base_mort, estimate, std.error, p.value)] %>%
  clean_fit_dt(c("days", "pred_cut1", "base_mort"), dig = 2) %>%
  .[order(pred_cut1, days), ] %>%
  dcast(days ~ pred_cut1, value.var = c("base_mort", "est_se")) %>%
  .[, days := paste0("\\hspace{5mm}", days, " days from enrollment")] %>%
  .[, .(days, `base_mort_1-3`, `est_se_1-3`, base_mort_2, est_se_2)]

print(xtable(dt_clean, digits = 3), include.rownames = FALSE,
      sanitize.text.function = force)

end_log_file()
