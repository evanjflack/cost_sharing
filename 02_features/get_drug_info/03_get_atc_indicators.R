# Proj: Cost-sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Creates ATC indicators for each unique ndc code from the ndc to atc4
#       xwalk

# Start Script -----------------------------------------------------------------
library(stringr)
library(fastDummies)
source("../../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type='character', default = "20pct")
)
unpack_opt(option_list)

# Start log file
start_log_file(paste0("log/03_create_atc_indicators_", pct))

# Data Read In -----------------------------------------------------------------
message("Reading in data...")

# NDC9 -> atc4 xwalk
ndc9_atc_xwalk <- fread(paste0(lib_base_data, "ndc9_atc4_xwalk_", pct,
                               ".csv")) %>%
  .[, lab_prod := str_pad(lab_prod, 9, pad = "0")]


# Transform Multiple ATC to Long -----------------------------------------------
message("Tranforming obs with multiple atc to long...")

atc <- str_split_fixed(ndc9_atc_xwalk$atc4, ";", 50) %>%
  as.data.table() %>%
  cbind(ndc9_atc_xwalk[, .(lab_prod)], .) %>%
  melt(id.var = "lab_prod",
       value.name = "atc4") %>%
  .[atc4 != "", ] %>%
  .[, atc4 := gsub(" ", "", atc4)] %>%
  .[, variable := NULL] %>%
  .[, atc1 := substr(atc4, 1, 1)] %>%
  .[, atc2 := substr(atc4, 1, 3)] %>%
  .[, atc3 := substr(atc4, 1, 4)] %>%
  .[order(lab_prod), ]

# Make Indicators --------------------------------------------------------------
message("Making Indicators (by level)...")

atc1_ind <- atc[, .(lab_prod, atc1)] %>%
  dummy_cols(select_columns = "atc1") %>%
  .[, atc1 := NULL] %>%
  .[, lapply(.SD, max), by = lab_prod, .SDcols = grep("atc", names(.))]

atc2_ind <- atc[, .(lab_prod, atc2)] %>%
  dummy_cols(select_columns = "atc2") %>%
  .[, atc2 := NULL] %>%
  .[, lapply(.SD, max), by = lab_prod, .SDcols = grep("atc", names(.))]

atc3_ind <- atc[, .(lab_prod, atc3)] %>%
  dummy_cols(select_columns = "atc3") %>%
  .[, atc3 := NULL] %>%
  .[, lapply(.SD, max), by = lab_prod, .SDcols = grep("atc", names(.))]

atc4_ind <- atc[, .(lab_prod, atc4)] %>%
  dummy_cols(select_columns = "atc4") %>%
  .[, atc4 := NULL] %>%
  .[, lapply(.SD, max), by = lab_prod, .SDcols = grep("atc", names(.))]

# Combine all levels
atc_ind <- atc1_ind %>%
  merge(atc2_ind, by = "lab_prod") %>%
  merge(atc3_ind, by = "lab_prod") %>%
  merge(atc4_ind, by = "lab_prod")

# Export -----------------------------------------------------------------------
message("Exporting...")

fwrite(atc_ind, paste0(lib_base_data, "atc_indicators_", pct, ".csv"))

end_log_file()