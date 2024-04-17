# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Makes Table 1 : Sample Summary Statistics

# Start Script -----------------------------------------------------------------
library(xtable)
library(stringr)
source("../../../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct")
)
unpack_opt(option_list)

options(scipen = 999)

start_log_file("log/01_sum_stats")

# Functions --------------------------------------------------------------------

round_sum_stats <- function(x) {
  ifelse(x > 100, format(round(x, 0), digits = 4, big.mark = ","),
         ifelse(x > 50, format(round(x, 1), digits = 4, big.mark = ","),
                format(signif(x, digits = 3)))) %>%
    as.character()
}

mean_sd <- function(x) {
  c(mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
}

# Read in Data -----------------------------------------------------------------

# Main data
model_vars <- c("bene_id", "race", "sex", "cntrct", "pred_cut1")

DT_model <- fread(paste0(lib_base_data, "revision_model_data_", pct,
                         ".csv")) %>%
  .[, model_vars, with = FALSE]
rm(model_vars)

# One-year mortality (from dec year 1)
mort_outcomes <- fread(paste0(lib_base_data, "mort_outcomes_jun_year_1_", pct,
                              ".csv")) %>%
  .[, c("bene_id", paste0("jun_mort_", seq(7, 18))), with = FALSE] %>%
  .[, one_year_mort := ifelse(rowSums(.SD) > 0, 1, 0),
    .SDcols = paste0("jun_mort_", seq(7, 18))] %>%
  .[, .(bene_id, one_year_mort)]

# One-year total cost (from day of enrollment)
one_year_cost <- fread(paste0(lib_base_data,
                              "sample_first_year_total_spending_",
                              pct, ".csv"))

# ATC indicators for first 90 days of enrollment
initial_atc <- fread(paste0(lib_base_data, "initial_atc_", 90,
                            "_days_new_enrollee_", pct, ".csv")) %>%
  .[bene_id %chin% DT_model$bene_id, ] %>%
  .[, c("bene_id", grep("atc3_", names(.), value = T)), with = FALSE]

# ATC Labels
# atc_labs <- fread(paste0(lib_base_data, "../drug_info/atc_labs.csv"))

initial_claims <- fread(paste0(lib_base_data,
                               "initial_part_d_claims_new_enrollee_", pct,
                               ".csv")) %>%
  .[, .(bene_id, n_claims_90)]

# Prep Data --------------------------------------------------------------------

DT_model %<>%
  merge(initial_atc, by = "bene_id") %>%
  merge(mort_outcomes, by = "bene_id") %>%
  merge(one_year_cost, by = "bene_id") %>%
  merge(initial_claims, by = "bene_id")

# Demographic indicators
DT_model %<>%
  .[, pdp := ifelse(substr(cntrct, 1, 1) == "S", 1, 0)] %>%
  .[, white := ifelse(race == 1, 1, 0)] %>%
  .[, female := ifelse(sex == 2, 1, 0)]


# Panel A ----------------------------------------------------------------------

sum_vars <- c("white", "female", "pdp", "n_claims_90", "one_year_cost",
                     "one_year_mort")
sum_labs <- c("White (\\%)", "Female (\\%)", "Standalone PDP (\\%)",
              "Initial 90-day fills", "One-year total spending (\\$)",
              "One-year mortality (p.p)")

make_sum_stats_a <- function(DT, sum_vars, sum_labs) {
  dt_sum_a <- DT %>%
    .[, lapply(.SD, mean_sd), .SDcols = sum_vars] %>%
    .[, measure := c("mean", "sd")] %>%
    melt(measure.vars = sum_vars) %>%
    dcast(variable ~ measure, value.var = "value") %>%
    .[!(variable %in% c("one_year_cost", "med_inc", "n_claims_90")),
      `:=`(mean = mean*100, sd = sd*100)] %>%
    .[, variable := factor(variable, levels = sum_vars, labels = sum_labs)] %>%
    .[, mean := sapply(mean, round_sum_stats)] %>%
    .[, sd := sapply(sd, round_sum_stats)] %>%
    .[, mean_sd := paste0(mean, " (", sd, ")")] %>%
    .[, .(variable, mean_sd)]


  return(dt_sum_a)
}

dt_sum_a_all <- make_sum_stats_a(DT_model, sum_vars, sum_labs) %>%
  setnames("mean_sd", "all")

dt_sum_a_1 <- make_sum_stats_a(DT_model[pred_cut1 == 1, ], sum_vars,
                                sum_labs) %>%
  setnames("mean_sd", "init")

dt_sum_a_2 <- make_sum_stats_a(DT_model[pred_cut1 == 2, ], sum_vars,
                               sum_labs) %>%
  setnames("mean_sd", "dh")

dt_sum_a_3 <- make_sum_stats_a(DT_model[pred_cut1 == 3, ], sum_vars,
                               sum_labs) %>%
  setnames("mean_sd", "cat")

dt_sum_a <- dt_sum_a_all %>%
  merge(dt_sum_a_1, by = "variable") %>%
  merge(dt_sum_a_2, by = "variable") %>%
  merge(dt_sum_a_3, by = "variable") %>%
  .[, variable := factor(variable, levels = sum_labs)] %>%
  .[order(variable)] %>%
  .[, variable := paste0("\\hspace{5 mm} ", variable)]

# Panel B ----------------------------------------------------------------------

my_atc_codes <- c("C10A", "C09A", "C07A", "C03A", "N06A", "D07A", "A02B",
                  "S01A", "A10B", "R01A")
my_atc_labs <- c("Lipid modifiers", "ACE inhibitors",
                 "Beta blockers", "Thiazide diuretics", "Antidepressants",
                 "Corticosteroids", "Acid blockers (GERD)", "Anti-iinfectives",
                 "Hypoglycemics (oral)", "Decongestants")

# Find top 10 ATC Classes
make_sum_stats_b <- function(DT) {
  dt_sum_b <- DT[, grep("atc3_", names(initial_atc), value = T),
                 with = FALSE] %>%
    .[, lapply(.SD, mean_sd)] %>%
    .[, measure := c("mean", "sd")] %>%
    melt(measure.vars = grep("atc3_", names(.), value = T)) %>%
    dcast(variable ~ measure, value.var = "value") %>%
    .[order(-mean), ] %>%
    .[variable %in% paste0("atc3_", my_atc_codes)] %>%
    .[, atc_code := gsub("atc3_", "", variable)] %>%
    .[, atc_lab1 := factor(atc_code, levels = my_atc_codes,
                           labels = my_atc_labs)] %>%
    .[order(-mean), ] %>%
    .[, mean := round(mean, 3)*100] %>%
    .[, sd := round(sd, 3)*100] %>%
    .[, mean_sd := paste0(mean, " (", sd, ")")] %>%
    .[, .(atc_lab1, mean_sd)] %>%
    setnames(c("variable", "mean_sd"))
  return(dt_sum_b)
}

dt_sum_b_all <- make_sum_stats_b(DT_model) %>%
  setnames("mean_sd", "all")

dt_sum_b_1 <- make_sum_stats_b(DT_model[pred_cut1 == 1, ]) %>%
  setnames("mean_sd", "init")

dt_sum_b_2 <- make_sum_stats_b(DT_model[pred_cut1 == 2, ]) %>%
  setnames("mean_sd", "dh")

dt_sum_b_3 <- make_sum_stats_b(DT_model[pred_cut1 == 3, ]) %>%
  setnames("mean_sd", "cat")

dt_sum_b <- dt_sum_b_all %>%
  merge(dt_sum_b_1, by = "variable") %>%
  merge(dt_sum_b_2, by = "variable") %>%
  merge(dt_sum_b_3, by = "variable") %>%
  .[, variable := factor(variable, levels = my_atc_labs)] %>%
  .[order(variable), ] %>%
  .[, variable := paste0("\\hspace{5 mm} ", variable)]

# Print ------------------------------------------------------------------------

print(xtable(dt_sum_a), include.rownames = FALSE,
      sanitize.text.function = force)

print(xtable(dt_sum_b), include.rownames = FALSE,
      sanitize.text.function = force)

dt_obs <- DT_model[, .(.N), by = pred_cut1]

end_log_file()
