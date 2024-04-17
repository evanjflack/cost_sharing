# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Creates table 4: utilization response by risk

# Start Script -----------------------------------------------------------------

source("../../../00_pre_process/start_script.R")

library(estimatr)
library(foreach)
library(xtable)

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct")
)
unpack_opt(option_list)

# Start log file
start_log_file("log/09_class_specific_response.R")

# Read In Data -----------------------------------------------------------------

# Model data
model_vars <- c("bene_id", "first_mo", "rfrnc_yr", "pred_cut1",
                "race", "sex", "cntrct_pbp_rfrnc_yr", "med_inc")
DT_model <- fread(paste0(lib_base_data, "revision_model_data_", pct,
                         ".csv")) %>%
  .[, model_vars, with = FALSE]
rm(model_vars)

claims_mo <- fread(paste0(lib_base_data,
                          "part_d_claims_pill_days_mo_new_enrollee_",
                          pct, ".csv")) %>%
  .[, .(bene_id, pill_days_1_12)]

init_fills<- fread(paste0(lib_base_data,
                          "initial_part_d_claims_new_enrollee_", pct,
                          ".csv"))

atc_days <- fread(paste0(lib_base_data, "dec_year1_atc_days_new_enrollee_",
                         pct, ".csv")) %>%
  .[, c("bene_id", grep("atc3_", names(.), value = T)), with = FALSE]
atc_vars <- names(atc_days)[2:26]
atc_classes <- gsub("_days_1_12", "", atc_vars)

init_atc <- fread(paste0(lib_base_data, "initial_atc_", 90,
                          "_days_new_enrollee_", pct, ".csv"))

init_atc %<>%
  .[, c("bene_id", atc_classes), with = FALSE]

atc_labs <- c("Statins", "ACE Inhibitors", "Beta Blockers",
              "Thiazides", "Antidepressants", "Corticosteroids",
              "GORD Drugs", "Antiinfectives", "Oral Diabetes",
              "Decongestants", "Thyroid Preparations",
              "Other Analgesics", "ARBs", "CCBs", "Inhalants",
              "Antiinflamatory", "Corticosteroids",
              "Hemorrhoid treatments", "Cough suppressants",
              "Antiinflamatory Agents", "Prophylactic Agents", "Opiods",
              "Intestinal Antiinflamatory", "Corticosteroids",
              "Quinolone Antibacterials")

icl_cat <- fread(paste0(lib_base_data, "icl_std_amts.csv"))

# Prep Data --------------------------------------------------------------------

DT_model %<>%
  merge(atc_days, by = "bene_id") %>%
  merge(init_atc, by = "bene_id") %>%
  merge(icl_cat, by = "rfrnc_yr") %>%
  merge(init_fills, by = "bene_id") %>%
  merge(claims_mo, by = "bene_id")

# Pre-gap coverage
DT_model %<>%
  .[, months_remain := 12 - first_mo + 1] %>%
  .[, cov_per_month := icl_std / months_remain] %>%
  .[, cov_per_month100 := cov_per_month / 100]



# Utilization by Risk ----------------------------------------------------------

dt_fit <- data.table()
for (i in atc_vars[1:25]) {
  message(i)
  DT_fit <- copy(DT_model) %>%
    .[, y := get(i)] %>%
    .[, y1 := get(gsub("_days_1_12", "", i))]

  fit_cov <- lm_robust(y ~ cov_per_month100:factor(pred_cut1) +
                         factor(pred_cut1) + factor(race) + factor(sex),
                       data = DT_fit, se_type = "stata",
                       fixed_effects = ~ cntrct_pbp_rfrnc_yr)

  dt_fit1 <- fit_to_dt(fit_cov, "cov_per_month100", "pred_cut1") %>%
    .[pred_cut1 == 2, ] %>%
    .[, .(estimate, std.error, p.value)] %>%
    .[, class := i] %>%
    .[, mean := mean(DT_fit[pred_cut1 == 2, y])] %>%
    .[, mean1 := mean(DT_fit[pred_cut1 == 2, y1])]

  dt_fit %<>% rbind(dt_fit1)

}

# Print ------------------------------------------------------------------------

dt_print <- copy(dt_fit) %>%
  .[, lab := atc_labs] %>%
  .[order(-mean1), ] %>%
  .[, perc := estimate / mean] %>%
  .[, perc := as.character(round(perc * 100, 1))] %>%
  .[, mean := as.character(round(mean, 1))] %>%
  .[, mean1 := as.character(round(mean1, 3) * 100)] %>%
  .[, class := gsub("atc3_", "", class)] %>%
  .[, class := gsub("_days_1_12", "", class)] %>%
  clean_fit_dt(c("class", "lab", "mean1", "mean", "perc")) %>%
  .[, .(class, lab, mean1,  mean, est_se, perc)]

print(xtable(dt_print, digits = 3),
      include.rownames = F, sanitize.text.function = force)

end_log_file()
