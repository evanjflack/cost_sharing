# Proj: Cost Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Identifies who is "treated" by a drug class (having a claim for that
#       class in an initial period)

# Start Script -----------------------------------------------------------------
library(Matrix)
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
card_codes <- c("atc4_c10aa", "atc4_c09aa", "atc4_c09ca", "atc4_c08ca",
                "atc4_c07ab", "atc4_c03aa")
diab_codes <- c("atc3_a10a", "atc3_a10b")
resp_codes <- c("atc2_r03")
all_codes <- c(card_codes, diab_codes, resp_codes)

# Start Log File
start_log_file(file_name = paste0("log/01d_id_treated_", sample, "_",
                                  initial_days, "_days_", pct))

# Data Read In -----------------------------------------------------------------

# Sample
DT_sample <- fread(paste0(lib_base_data, sample, "_sample_", pct, ".csv")) %>%
  .[, .(bene_id, rfrnc_yr)]

# Initial ATC Indicators
if (sample == "dual_pred") {
  id_vars <- c("bene_id", "rfrnc_yr")
  initial_atc <- read_and_combine(lib_base_data, paste0("initial_atc_dual"),
                                  years, pct) %>%
    .[, c(id_vars, card_codes, diab_codes, resp_codes), with = F]
}

# Initial Card -----------------------------------------------------------------
message("Counting initial treatments...")

initial_treatment <- initial_atc %>%
  .[, num_card_drugs := rowSums(.SD), .SDcols = card_codes] %>%
  .[, any_card := ifelse(num_card_drugs > 0, 1, 0)] %>%
  .[, num_diab_drugs := rowSums(.SD), .SDcols = diab_codes] %>%
  .[, any_diab := ifelse(num_diab_drugs > 0, 1, 0)] %>%
  merge(DT_sample[, id_vars, with = FALSE], by = id_vars)

# Export -----------------------------------------------------------------------
fwrite(initial_treatment, paste0(lib_base_data, "initial_treatment_", sample,
                                 "_", initial_days, "_days_", pct, ".csv"))

end_log_file()
