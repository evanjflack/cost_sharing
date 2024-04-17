# Proj: Cost Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Preps initial spending features

# Start Script -----------------------------------------------------------------
library(fastDummies)
source("../../../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type='character', default = "20pct"),
  make_option(c("-s", "--sample"), type='character',
              default = "dual_pred"),
  make_option(c("-f", "--first_year"), type='integer', default = 2007),
  make_option(c("-l", "--last_year"), type='integer', default = 2012),
  make_option(c("-d", "--initial_days"), type='integer', default = 90)
)
unpack_opt(option_list)

# Start log file
file_name <- paste0("log/01b_prep_initial_spending_", sample, "_", initial_days,
                    "_days_", pct)
start_log_file(file_name = file_name)

# Data Read In -----------------------------------------------------------------
message("Reading in Data...")
# Sample

if (sample == 'dual_pred') {
  DT_sample <- fread(paste0(lib_base_data, sample, "_sample_", pct, ".csv"))
} else {
  DT_sample <- fread(paste0(lib_base_data, "revision_model_data_", pct,
                         ".csv"))
}

n1 <- nrow(DT_sample)

# Initial spending (from ../../02_features)
if (sample == "dual_pred") {
  initial_spending <- read_and_combine(lib_base_data,
                                       "initial_part_d_cost_dual", years,
                                       pct)
  id_vars <- c("bene_id", "rfrnc_yr")
} else {
  initial_spending <- fread(paste0(lib_base_data,
                                   "initial_part_d_spending_new_enrollee_", pct,
                                   ".csv"))
  id_vars <- "bene_id"
}

initial_spending %<>%
  .[, c(id_vars, paste0("initial_cost_", 90)), with = FALSE] %>%
  merge(DT_sample[, id_vars, with = FALSE], by = id_vars)

# Initial Spending -------------------------------------------------------------
message("Making initial spending dummies...")
DT_init_cost <- initial_spending %>%
  setnames(paste0("initial_cost_", initial_days), "init_cost") %>%
  .[, init_cost_cut := bin_variable(init_cost, min = 0, max = 10000,
                                    int = 100)] %>%
  dummy_cols(select_columns = "init_cost_cut") %>%
  .[, init_cost_cut := NULL] %>%
  setnames(names(.), gsub("_cut_", "_", names(.))) %>%
  setnames(names(.), gsub("-", "n", names(.)))

# Export -----------------------------------------------------------------------
message("Exporting...")
fwrite(DT_init_cost, paste0(lib_base_data, "DT_init_cost_", sample, "_",
                            initial_days, "_days_", pct, ".csv"))

end_log_file()
