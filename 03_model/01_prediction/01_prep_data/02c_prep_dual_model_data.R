# Proj: Cost Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Combines all features for the dual 66+ sample into predictor matrices.

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
  make_option(c("-d", "--initial_days"), type='integer', default = 90)
)
unpack_opt(option_list)

# Start Log File
file_name <- paste0("log/02c_prep_dual_model_data_", initial_days,
                    "_days_", pct)
start_log_file(file_name = file_name)

# Ids --------------------------------------------------------------------------
message("Reading in IDs...")
# Ids (bene_id, rfrnc_yr, set, fold, outomces...)
DT_all <- fread(paste0(lib_base_data, "ids_prelim_dual_pred_", initial_days, "_days_",
                       pct, ".csv"))
vars_for_ids <- names(DT_all)

# Features ---------------------------------------------------------------------
message("Reading in features...")

message("Initial spending")
DT_init_cost <- fread(paste0(lib_base_data, "DT_init_cost_dual_pred_",
                             initial_days, "_days_", pct, ".csv"))
DT_all <- merge(DT_all, DT_init_cost, by = c("bene_id", "rfrnc_yr"))
remove(DT_init_cost)

message("Demographics")
DT_dem <- fread(paste0(lib_base_data, "DT_dem_dual_pred_", pct, ".csv"))
DT_all <- merge(DT_all, DT_dem, by = c("bene_id", "rfrnc_yr"))
remove(DT_dem)

message("Age")
DT_age <- fread(paste0(lib_base_data, "DT_age_dual_pred_", pct, ".csv"))
DT_all <- merge(DT_all, DT_age, by = c("bene_id", "rfrnc_yr"))
remove(DT_age)

message("State")
DT_state <- fread(paste0(lib_base_data, "DT_state_dual_pred_", pct, ".csv"))
DT_all <- merge(DT_all, DT_state, by = c("bene_id", "rfrnc_yr"))
remove(DT_state)

message("ATC")
initial_atc <- read_and_combine(lib_base_data, paste0("initial_atc_dual"),
                                years,
                                pct)

DT_all <- merge(DT_all, initial_atc, by = c("bene_id", "rfrnc_yr"))
remove(initial_atc)

setnames(DT_all, names(DT_all), tolower(names(DT_all)))

# IDS --------------------------------------------------------------------------
message("Prepping labels...")
ids <- DT_all %>%
  .[, vars_for_ids, with = F]
saveRDS(ids, paste0(lib_base_data, "ids_66_train_", initial_days, "_days_", pct,
                    ".rds"))
remove(ids)

# LASSO ------------------------------------------------------------------------
message("Prepping Lasso features matrix...")
sex_vars <- "female"
race_vars <- grep("race_", names(DT_all), value = T)
age_vars <- paste0("age", c(1, 2, 3))
cost_vars <- grep("init_cost_", names(DT_all), value = T)
state_vars <- grep("state_", names(DT_all), value = T) %>%
  .[str_length(.) <= 10]
atc_vars <- c(grep("atc3", names(DT_all), value = T))

lasso_vars <- c(sex_vars, race_vars, age_vars, cost_vars,
                state_vars, atc_vars)

lasso_x <- DT_all %>%
  .[, lasso_vars, with = F]

lasso_vars1 <- names(lasso_x)[sapply(lasso_x, function(v) var(v) != 0)]

lasso_x <- sparsify(lasso_x[, lasso_vars1, with = F])

message(paste(length(lasso_vars) - length(lasso_vars1), "of",
              length(lasso_vars),
              "variables were constant and dropped.\nlasso_x now has",
              dim(lasso_x)[2], "predictors."))

saveRDS(lasso_x, paste0(lib_base_data, "lasso_x_66_train_", initial_days,
                        "_days_",pct, ".rds"))
remove(lasso_x, lasso_vars, lasso_vars1)

# GBM --------------------------------------------------------------------------
message("Prepping GBM features matrix...")
sex_vars <- "female"
race_vars <- grep("race_", names(DT_all), value = T)
age_vars <- "age1"
cost_vars <- "init_cost"
state_vars <- grep("state_", names(DT_all), value = T) %>%
  .[str_length(.) <= 10]
atc_vars <- c(grep("atc3", names(DT_all), value = T))
gbm_vars <- c(sex_vars, race_vars, age_vars, cost_vars, state_vars,
              atc_vars)

gbm_x <- DT_all %>%
  .[, gbm_vars, with = F]
gbm_vars1 <- names(gbm_x)[sapply(gbm_x, function(v) var(v) != 0)]

gbm_x <- sparsify(gbm_x[, gbm_vars1, with = F])

message(paste(length(gbm_vars) - length(gbm_vars1), "of",
              length(gbm_vars),
              "variables were constant and dropped.\ngbm_x now has",
              dim(gbm_x)[2], "predictors."))

saveRDS(gbm_x, paste0(lib_base_data, "gbm_x_66_train_", initial_days,
                      "_days_", pct, ".rds"))
remove(gbm_x, gbm_vars, gbm_vars1)

remove(DT_all)

# Done -------------------------------------------------------------------------
end_log_file()
