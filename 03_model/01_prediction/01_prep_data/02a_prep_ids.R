# Proj: Cost Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Preps IDs (id variables, response variables, set/fold assignment,
#       population "keep" criteria) of dual samples for risk prediction

# Start Script -----------------------------------------------------------------
library(stringr)
library(yaml)
source("../../../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct"),
  make_option(c("-s", "--sample"), type='character', default = "dual_pred"),
  make_option(c("-f", "--first_year"), type='integer', default = 2007),
  make_option(c("-l", "--last_year"), type='integer', default = 2012),
  make_option(c("-d", "--initial_days"), type='integer', default = 90)
)
unpack_opt(option_list)

# Additional User Inputs
# Percent of entire sample in each subsample (train_perc will be 1 -
# ensemble_perc - test_perc)
ensemble_perc <- .1
test_perc <- .2
# Number of folds
nfolds <- 5

id_vars <- c("bene_id", "rfrnc_yr")

# Start Log File
file_name <- paste0("log/02a_prep_ids_", sample , "_", initial_days, "_days_",
                    pct)
start_log_file(file_name = file_name)

# Data Read In -----------------------------------------------------------------
message("Resding in data...")

# Sample
DT_sample <- fread(paste0(lib_base_data, sample, "_sample_", pct, ".csv"))

# Initial treamtnet indicators
initial_treatment <- fread(paste0(lib_base_data, "initial_treatment_", sample,
                                  "_", initial_days, "_days_", pct, ".csv"))

acute_events <- fread(paste0(lib_base_data,"dual_pred_acute_event_", pct,
                             ".csv"))

mort <- fread(paste0(lib_base_data, "dual_pred_mortality_outcomes_",
                     pct, ".csv")) %>%
  .[, first_mo := NULL]

acute_events %<>%
  merge(mort, by = c("bene_id", "rfrnc_yr"))

ids <- DT_sample %>%
  .[, c("bene_id", "rfrnc_yr", "pdp"), with = FALSE]

# Assigning Sets/Folds ---------------------------------------------------------
message =("Assigning sets/folds...")
# Random seeding
set.seed(712)

N <- uniqueN(ids$bene_id)
ensemble_N <- floor(ensemble_perc*N)
test_N <- floor(test_perc*N)
train_N <- N - ensemble_N - test_N
sets <- c(rep("ensemble", ensemble_N), rep("test", test_N),
          rep("train", train_N))

set_fold <- ids %>%
  .[, .(bene_id)] %>%
  unique() %>%
  .[, set := sample(sets, N)] %>%
  .[set == "train", fold := sample(1:nfolds,
                                   train_N, replace = T)] %>%
  .[is.na(fold), fold := 0]

ids %<>%
  merge(set_fold, by = "bene_id")

# Population Criteria ----------------------------------------------------------
message("Defining population criteria...")

# ids %<>%
#   .[, keep_all := 1] %>%
#   .[, keep_risk := 1]

ids %<>%
  merge(initial_treatment, by = id_vars) %>%
  .[, keep_untreated_any_card := ifelse(any_card == 0 & pdp == 1, 1, 0)] %>%
  .[, keep_untreated_any_diab := ifelse(any_diab == 0 & pdp == 1, 1, 0)] %>%
  .[, keep_untreated_resp := ifelse(atc2_r03 == 0 & pdp == 1, 1, 0)]

keep_vars <- grep("keep", names(ids), value = T)

ids %<>%
  .[, c(id_vars, "set", "fold", keep_vars), with = F]

# Outcomes ---------------------------------------------------------------------
message("Merging in outcomes...")
ids %<>%
  merge(acute_events, by = id_vars)


# Export -----------------------------------------------------------------------
message("Exporting...")
fwrite(ids, paste0(lib_base_data, "ids_prelim_", sample, "_", initial_days,
                   "_days_", pct, ".csv"))

end_log_file()
