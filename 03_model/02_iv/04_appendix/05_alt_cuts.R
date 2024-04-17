# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: makes appendix table C1 (alternative middle spender definitions)

# Start Script -----------------------------------------------------------------

library(estimatr)
library(xtable)

source("../../../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct")
)
unpack_opt(option_list)

# Start log file
start_log_file("log/05_alt_cuts")

# User inputs
instruments <- c("first_mo")

# Read In Data -----------------------------------------------------------------

# Model data
model_vars <- c("bene_id", "first_mo", "rfrnc_yr", "race", "sex", "birth_mo",
                "cntrct_pbp_rfrnc_yr", "year_cut", "initial_cost_90",
                "cov_per_month100")
DT_model <- fread(paste0(lib_base_data, "revision_model_data_", pct,
                         ".csv")) %>%
  .[, model_vars, with = FALSE]
rm(model_vars)

# December mortality indicators
mort_outcomes <- fread(paste0(lib_base_data, "mort_outcomes_jun_year_1_", pct,
                              ".csv")) %>%
  .[, .(bene_id, jun_mort_7)] %>%
  setnames("jun_mort_7", "dec_mort")

DT_model %<>%
  merge(mort_outcomes, by = "bene_id")

rm(mort_outcomes)



# Estimate ---------------------------------------------------------------------

dt_fit <- data.table()
for (j in seq(.6, .7, .05)) {
  for (i in seq(.95, .97, .01)) {
    message(i, j)
    DT_fit <- copy(DT_model) %>%
      .[, pred_cut1 := ifelse(initial_cost_90 <= quantile(initial_cost_90, j),
                              1, ifelse(initial_cost_90 <=
                                          quantile(initial_cost_90, i), 2, 3)),
        by = first_mo]

    fit_pred <- lm_robust(dec_mort ~ first_mo:factor(pred_cut1) +
                            factor(pred_cut1) +
                            factor(race) + factor(sex),
                          fixed_effects = ~ cntrct_pbp_rfrnc_yr,
                          data = DT_fit, se_type = "stata")

    dt_fit1 <- fit_to_dt(fit_pred, "first_mo", "pred_cut1") %>%
      .[, .(pred_cut1, estimate, std.error, p.value, statistic)] %>%
      .[pred_cut1 == 2, ] %>%
      .[, cut1 := j] %>%
      .[, cut2 := i] %>%
      .[, estimate := estimate * 100] %>%
      .[, std.error := std.error * 100] %>%
      .[, .(cut1, cut2, estimate, std.error, p.value)]

    dt_fit %<>% rbind(dt_fit1)
  }
}

# Print ------------------------------------------------------------------------

dt_print <- dt_fit %>%
  copy() %>%
  .[order(-cut1, -cut2)] %>%
  # remove main spec
  .[!(cut1 == .70 & cut2 == .97)] %>%
  .[, cut1 := as.character(cut1*100)] %>%
  .[, cut2 := as.character(cut2*100)] %>%
  clean_fit_dt(c("cut1", "cut2"), dig = 3)

print(xtable(dt_print, digits = 5),
      include.rownames = F, sanitize.text.function = force)

end_log_file()