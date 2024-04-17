# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Calibrates risk predictions in main sample

# start Script -----------------------------------------------------------------

library(ggplot2)

source("../../../00_pre_process/start_script.R")
source("../../../00_pre_process/define_plot_theme.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct")
)
unpack_opt(option_list)

# Additional User Inuts
card_codes <- paste0("atc3_", c("C10A", "C09A", "C03A", "C07A", "C09C",
                                "C08C"))
diab_codes <- c("atc2_A10")
resp_codes <- c("atc2_R03")

start_log_file("log/08_risk_calib")

# Read In Data -----------------------------------------------------------------
message("Reading in data...")

# model Data
model_vars <- c("bene_id", "cntrct")
DT_model <- fread(paste0(lib_base_data, "revision_model_data_", pct,
                         ".csv")) %>%
  .[, model_vars, with = FALSE]
rm(model_vars)

# Acute event indicators
acute_event <- fread(paste0(lib_base_data, "new_enrollee_acute_event_", pct,
                            ".csv"))

pred_card <- readRDS( paste0(lib_base_data, "ensemble_pred_new_enrollee_dual_false_",
                             "comp_card", "_", "untreated_any_card", "_", 66, "_", 90,
                             "_days_", pct, ".rds")) %>%
  .[, .(bene_id, ensemble_pred)] %>%
  setnames("ensemble_pred", "ensemble_pred_card_90")

pred_diab <- readRDS( paste0(lib_base_data, "ensemble_pred_new_enrollee_dual_false_",
                             "comp_diab", "_", "untreated_any_diab", "_", 66, "_", 90,
                             "_days_", pct, ".rds")) %>%
  .[, .(bene_id, ensemble_pred)] %>%
  setnames("ensemble_pred", "ensemble_pred_diab_90")

pred_resp <- readRDS( paste0(lib_base_data, "ensemble_pred_new_enrollee_dual_false_",
                             "comp_resp", "_", "untreated_resp", "_", 66, "_", 90,
                             "_days_", pct, ".rds")) %>%
  .[, .(bene_id, ensemble_pred)] %>%
  setnames("ensemble_pred", "ensemble_pred_resp_90")

# Indicators for atc fills in first 90 days
initial_atc <- fread(paste0(lib_base_data, "initial_atc_", 90,
                            "_days_new_enrollee_", pct, ".csv"))

# Prep Data --------------------------------------------------------------------
message("Prepping data...")

initial_treatment <- initial_atc %>%
  .[, c("bene_id", card_codes, diab_codes, resp_codes), with = FALSE] %>%
  .[, any_card := rowSums(.SD), .SDcols = card_codes] %>%
  .[, any_card := ifelse(any_card > 0, 1, 0)] %>%
  .[, any_diab := atc2_A10] %>%
  .[, any_resp := atc2_R03] %>%
  .[, .(bene_id, any_card, any_diab, any_resp)]

DT_model %<>%
  merge(pred_card, by = "bene_id") %>%
  merge(pred_diab, by = "bene_id") %>%
  merge(pred_resp, by = "bene_id") %>%
  merge(acute_event, by = "bene_id") %>%
  merge(initial_treatment, by = "bene_id")

# Only those in non-MA plans
DT_model %<>%
  .[substr(cntrct, 1, 1) == "S", ]

risk_types <- c("card", "diab", "resp")
risk_labs <- c("Cardiovascular", "Diabetes", "Respiratory")

# Risk Calibration -------------------------------------------------------------
message("Maing risk calibration plot...")
dtp <- data.table()
for (i in risk_types) {
  print(i)
  DT_model %<>%
    .[, risk := get(paste0("ensemble_pred_", i, "_90"))] %>%
    .[, treated := get(paste0("any_", i))] %>%
    .[, risk_cut := bin_variable(risk, quant = 10), by = treated]


  dtp1 <- calc_cmean(DT_model, y = paste0("comp_", i),
                     x = c("risk_cut", "treated")) %>%
    .[, type := i]
  dtp %<>% rbind(dtp1)
}

dtp %<>%
  .[, treated := factor(treated, levels = c(0, 1),
                        labels = c("Untreated", "Treated"))] %>%
  .[, type := factor(type, levels = risk_types, labels = risk_labs)]

ggplot(dtp) +
  aes(x = risk_cut, y = mean, shape = factor(treated),
      linetype = factor(treated)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ type, scales = "free_y") +
  labs(x = "Predicted Risk Decile", y = "Actual Event Rate") +
  scale_x_continuous(breaks = seq(2, 10, 2)) +
  my_theme_paper +
  theme(legend.title = element_blank(),
        legend.position = c(.1, .8))

ggsave(paste0(lib_base, "plots/appendix/d1a_risk_calib.png"),
       width = 6, height = 3.5)

# Risk Capture -----------------------------------------------------------------
message("Making risk capture plot...")
dtp <- data.table()
for (i in c("card", "diab", "resp")) {
  for (j in seq(0, 1, .1)) {
    DT_model %<>%
      .[, risk := get(paste0("ensemble_pred_", i, "_90"))] %>%
      .[, high_risk := ifelse(risk >= quantile(risk, j), 1, 0),
        by = treated] %>%
      .[, treated := get(paste0("any_", i))]

    dtp1 <- DT_model[, .(events = sum(get(paste0("comp_", i)))),
                     by = .(high_risk, treated)] %>%
      .[, total := sum(events), by = treated] %>%
      .[, perc := events/total] %>%
      .[, risk_type := i] %>%
      .[, risk_cut := j]

    dtp %<>% rbind(dtp1)
  }
}

dtp %<>%
  .[, risk_type := factor(risk_type, levels = risk_types,
                          labels = risk_labs)] %>%
  .[, treated := factor(treated, levels = c(0, 1),
                        labels = c("Untreated", "Treated"))] %>%
  .[, risk_cut := risk_cut * 100] %>%
  .[, perc := perc * 100]

ggplot(dtp[high_risk == 1]) +
  aes(x = abs(100 - risk_cut), y = perc, shape = factor(treated),
      linetype = factor(treated)) +
  geom_point() +
  geom_line() +
  scale_linetype_manual(values = c(1, 2)) +
  geom_line(aes(x = 100 - risk_cut, y = 100 - risk_cut), linetype = 3) +
  facet_wrap(~ risk_type) +
  scale_x_continuous(breaks = seq(0, 100, 20)) +
  labs(x = "% of Sample (Highest Predicted Risk)", y = "% of Actual Events") +
  my_theme_paper +
  theme(legend.title = element_blank()) +
  theme(legend.position = c(.85, .2))

ggsave(paste0(lib_base, "plots/appendix/d1b_risk_capture.png"),
       width = 6, height = 3.5)

# End --------------------------------------------------------------------------
end_log_file()
