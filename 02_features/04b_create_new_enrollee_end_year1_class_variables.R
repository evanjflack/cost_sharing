# Header -----------------------------------------------------------------------
# Proj: Cost-Sharing
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Makes counts of fills in each drug class at the end of year 1 and
#       beginning of year 2

# Start Script -----------------------------------------------------------------
library(lubridate)
library(stringr)
library(tidyr)
source("../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct"),
  make_option(c("-f", "--first_year"), type = 'integer', default = 2007),
  make_option(c("-l", "--last_year"), type = 'integer', default = 2013)
)
unpack_opt(option_list)

# Start log file
start_log_file(paste0("log/04b_create_new_enrollee_end_year1_class_variables_",
                      pct))

# Data Read In -----------------------------------------------------------------
message("Reading in data...")

# Codes
top_atc3 <- fread(paste0(lib_base_data,
                         "top_atc3_initial_90_days_20pct.csv")) %>%
  .[order(-mean), ] %>%
  .[1:50, ] %>%
  .[, atc]
# Key cardiovascular, respiratory, and diabetes ATC codes
card_codes <- paste0("atc3_", c("C10A", "C09A", "C03A", "C07A", "C09C",
                                "C08C"))
diab_codes <- c("atc2_A10")
resp_codes <- "atc2_R03"
anti_codes <- "atc2_J01"
opi_codes <- "atc3_N02A"
all_codes <- unique(c(top_atc3, card_codes, diab_codes, resp_codes, anti_codes,
                      opi_codes))

all_codes1 <- c(card_codes, diab_codes, resp_codes, anti_codes, opi_codes)

# PDE claims of sample
pde <- read_and_combine(lib_base_data, "sample_pde", years, pct) %>%
  .[, .(bene_id, rfrnc_yr, srvc_mo, srvc_dt, lab_prod, dayssply)]

# Indicators for atc codes for all ndc9 (lab_prod)
atc_ind <- fread(paste0(lib_base_data, "atc_indicators_", "20pct", ".csv"))

# Beneficiaries (that could be) in the sample
sample_benes <- fread(paste0(lib_base_data, "new_enrollee_sample_", pct,
                          ".csv")) %>%
  .[, .(bene_id, join_year)]

# Prep Data --------------------------------------------------------------------
message("Prepping data...")

# Prep ATC indicators
# Format ndc9 (lab_prod) variables. Make an indicator for "life saving drugs",
# any drug in the card/resp/diab codes. Subset to only ls claims. Make
# indicators for cardiovascular, and hypertension claims. Rename variables.
atc_ind %<>%
  .[, lab_prod := str_pad(lab_prod, 9, pad = "0")] %>%
  .[, c("lab_prod", all_codes), with = FALSE] %>%
  .[, any_ind := ifelse(rowSums(.SD) > 0, 1, 0), .SDcols = all_codes] %>%
  .[any_ind == 1, ] %>%
  .[, any_ind := NULL] %>%
  .[, ls_ind := ifelse(rowSums(.SD) > 0, 1, 0),
    .SDcols = all_codes] %>%
  .[, card_ind := ifelse(rowSums(.SD) > 0, 1, 0), .SDcols = card_codes] %>%
  .[, hyper_ind := ifelse(rowSums(.SD) > 0, 1, 0),
    .SDcols = card_codes[card_codes != "atc3_C10A"]] %>%
  setnames(all_codes, paste0(all_codes, "_ind"))

# Prep PDE
# Subset to claims of those in sample, between Aug. year 1 and Apr. year 2.
# Define first/second half of the month. format ndc9 (lab_prod). Rename the
# month variable. Keep only the id, time and ndc9 variables.
year1_pde <- pde %>%
  merge(sample_benes, by = "bene_id") %>%
  .[, year := rfrnc_yr - join_year + 1] %>%
  .[year == 1 & srvc_mo == 12, ] %>%
  .[, srvc_dt := mdy(srvc_dt)] %>%
  .[, lab_prod := str_pad(lab_prod, 9, pad = "0")] %>%
  setnames("srvc_mo", "month") %>%
  .[, .(bene_id, year, month, lab_prod, dayssply)]

# Merge in atc indicators to PDE, remove the ndc9 variable
year1_pde %<>%
  merge(atc_ind, by = "lab_prod") %>%
  .[, lab_prod := NULL]

class_vars <- names(year1_pde) %>%
  .[!(. %in% c("bene_id", "year", "month", "dayssply"))]

ind_days <- year1_pde$dayssply*year1_pde[, class_vars, with = FALSE] %>%
  setnames(names(.), gsub("ind", "days", names(.)))

year1_pde %<>%
  cbind(ind_days) %>%
  .[, dayssply := NULL]

remove(pde, atc_ind)

vars <- c("bene_id", "year", "month",
          grep("_days", names(year1_pde) ,value = T))

atc_days_dec <- year1_pde %>%
  .[, vars, with = FALSE] %>%
  .[year == 1 & month == 12, ] %>%
  .[, `:=`(year = NULL, month = NULL)] %>%
  .[, lapply(.SD, sum), by = .(bene_id)] %>%
  setnames(names(.)[-1], paste0(names(.)[-1], "_1_12")) %>%
  fill_in_zeros(sample_benes, "bene_id")

# Export -----------------------------------------------------------------------

fwrite(atc_days_dec, paste0(lib_base_data, "dec_year1_atc_days_new_enrollee_",
                            pct, ".csv"))

end_log_file()
