# Header -----------------------------------------------------------------------
# Proj: Cost-Sharing
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Determines most popular ATC3 classes
# ------------------------------------------------------------------------------

# Start Script -----------------------------------------------------------------
package_list <- c("lubridate", "stringr", "tidyr")
source("../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct")
)
unpack_opt(option_list)

# Start log file
start_log_file(paste0("log/04a_determine_most_common_atc_classes_", pct))

# Read in Data -----------------------------------------------------------------

# Main analytic sample
sample <- fread(paste0(lib_base_data, "new_enrollee_sample_", pct, ".csv")) %>%
  .[first_mo %between% c(2, 9), ] %>%
  .[keep_join_month == 1, ] %>%
  .[, .(bene_id)]

# Indicators for atc classes filled in first 90 days
initial_atc <- fread(paste0(lib_base_data, "initial_atc_", 90,
                            "_days_new_enrollee_", pct, ".csv"))

# Determine Most Filled Classes ------------------------------------------------

# Keep only ATC3 variables, subset to individuals in sample, find mean of each
# variable
top_atc <- initial_atc %>%
  .[, c("bene_id", grep("atc3_", names(.), value = T)), with = FALSE] %>%
  .[bene_id %chin% sample$bene_id, ] %>%
  .[, bene_id := NULL] %>%
  .[, lapply(.SD, mean)]

# Reshape to long and order (descending)
top_atc <- suppressWarnings(melt(top_atc, variable.name = "atc",
                                 value.name = "mean")) %>%
  .[order(-mean), ]

# Export -----------------------------------------------------------------------

fwrite(top_atc, paste0(lib_base_data, "top_atc3_initial_90_days_", pct, ".csv"))

end_log_file()
