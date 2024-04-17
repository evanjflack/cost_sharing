# Proj: Cost Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Combines all features into predictor matrices for new enrollee main
#       and falsification sample.

# Start Script -----------------------------------------------------------------
library(Matrix)
library(stringr)
library(yaml)
source("../../../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct"),
  make_option(c("-f", "--first_year"), type='integer', default = 2007),
  make_option(c("-l", "--last_year"), type='integer', default = 2012),
  make_option(c("-d", "--initial_days"), type='integer', default = 90))
unpack_opt(option_list)

# Start Log File
file_name <- paste0("log/02b_prep_new_enrollee_model_data_", initial_days,
                    "_days_", pct)
start_log_file(file_name = file_name)

# Sample -----------------------------------------------------------------------
message("Reding in sample...")
DT_all <- fread(paste0(lib_base_data, "revision_model_data_", pct,
                       ".csv")) %>%
  .[, .(bene_id)]

n1 <- nrow(DT_all)

# Features ---------------------------------------------------------------------
message("Reading in features...")
message("Initial spending")
DT_init_cost <- fread(paste0(lib_base_data,
                             "DT_init_cost_new_enrollee_",
                             initial_days, "_days_", pct, ".csv"))
DT_all <- merge(DT_all, DT_init_cost, by = "bene_id")
remove(DT_init_cost)

message("Demographics")
DT_dem <- fread(paste0(lib_base_data, "DT_dem_new_enrollee_", pct,
                       ".csv"))
DT_all <- merge(DT_all, DT_dem, by = "bene_id")
remove(DT_dem)

message("Age")
DT_age <- fread(paste0(lib_base_data, "DT_age_new_enrollee_", pct,
                       ".csv"))
DT_all <- merge(DT_all, DT_age, by = "bene_id")
remove(DT_age)

message("State")
DT_state <- fread(paste0(lib_base_data, "DT_state_new_enrollee_",
                         pct, ".csv"))
DT_all <- merge(DT_all, DT_state, by = "bene_id")
remove(DT_state)

message("ATC")
initial_atc <- fread(paste0(lib_base_data, "initial_atc_", initial_days,
                            "_days_new_enrollee_", pct, ".csv"))
DT_all <- merge(DT_all, initial_atc, by = "bene_id")
remove(initial_atc)

setnames(DT_all, names(DT_all), tolower(names(DT_all)))

n2 <- nrow(DT_all)

message("Intial rows: ", n1, ", final rows: ", n2)
message(n1 == n2)

# IDS --------------------------------------------------------------------------
message("Prepping labels...")
ids <- DT_all %>%
  .[, .(bene_id)]
saveRDS(ids, paste0(lib_base_data, "ids_new_enrollee_", initial_days,
                    "_days_", pct, ".rds"))
remove(ids)

# LASSO ------------------------------------------------------------------------
message("Lasso")
sex_vars <- "female"
race_vars <- grep("race_", names(DT_all), value = T)
age_vars <- paste0("age", seq(1, 3))
cost_vars <- grep("init_cost_", names(DT_all), value = T)
state_vars <- grep("state_", names(DT_all), value = T) %>%
  .[str_length(.) <= 10]
atc_vars <- c(grep("atc1", names(DT_all), value = T),
              grep("atc2", names(DT_all), value = T),
              grep("atc3", names(DT_all), value = T))

lasso_vars <- c(sex_vars, race_vars, age_vars, cost_vars,
                state_vars, atc_vars)

lasso_x <- sparsify(DT_all[, lasso_vars, with = F])

saveRDS(lasso_x, paste0(lib_base_data, "lasso_x_new_enrollee_",
                        initial_days, "_days_", pct, ".rds"))
remove(lasso_x, lasso_vars)

# GBM
message("GBM")
sex_vars <- "female"
race_vars <- grep("race_", names(DT_all), value = T)
age_vars <- paste0("age", seq(1, 3))
cost_vars <- "init_cost"
state_vars <- grep("state_", names(DT_all), value = T) %>%
  .[str_length(.) <= 10]
atc_vars <- c(grep("atc1", names(DT_all), value = T),
              grep("atc2", names(DT_all), value = T),
              grep("atc3", names(DT_all), value = T))
gbm_vars <- c(sex_vars, race_vars, age_vars, cost_vars, state_vars,
              atc_vars)

gbm_x <- sparsify(DT_all[, gbm_vars, with = F])

saveRDS(gbm_x, paste0(lib_base_data, "gbm_x_new_enrollee_",
                      initial_days, "_days_", pct, ".rds"))
remove(gbm_x, gbm_vars)

remove(DT_all)

# Done -------------------------------------------------------------------------
end_log_file()
