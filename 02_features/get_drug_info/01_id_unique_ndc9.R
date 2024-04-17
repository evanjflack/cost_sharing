# Proj: Cost-sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Identifies all unique ndc9 codes in pde claims of the sample. To be used
#       to query RxNav APIs for additional drug information.

# Start Script -----------------------------------------------------------------
library(stringr)
library(httr)
source("../../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type='character', default = "20pct"),
  make_option(c("-f", "--first_year"), type='integer', default = 2007),
  make_option(c("-l", "--last_year"), type='integer', default = 2013)
)
unpack_opt(option_list)

# Start log file
start_log_file(paste0("log/01_id_unique_ndc9_", pct))

# Data Read In -----------------------------------------------------------------
message("Reading in data...")

# Sample pde claims
pde <- read_and_combine(lib_base_data, "sample_pde", years, pct)

# Unique NDCs ------------------------------------------------------------------
message("Finding Unique NDCs...")
unique_ndc9 <- pde %>%
  .[, .(prdsrvid)] %>%
  .[, ndc := str_pad(prdsrvid, 11, pad = "0")] %>%
  .[, lab_prod := substr(ndc, 1, 9)] %>%
  .[, .SD[1], by = lab_prod] %>%
  .[, .(lab_prod, ndc)]

message(nrow(unique_ndc9), " unique ndc9 codes.")

# Export -----------------------------------------------------------------------
fwrite(unique_ndc9, paste0(lib_base_data, "unique_ndc9_", pct, ".csv"))

end_log_file()
