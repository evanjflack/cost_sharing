# ------------------------------------------------------------------------------
# Proj: Cost-sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Makes mortality outcomes in dual sample

# Start Script -----------------------------------------------------------------
library(lubridate)
source("../../../00_pre_process/start_script.R")

option_list = list(
  make_option(c("-p", "--pct"), type='character', default = "20pct"),
  make_option(c("-f", "--first_year"), type='integer', default = 2007),
  make_option(c("-l", "--last_year"), type='integer', default = 2013)
)
unpack_opt(option_list)

# Start log file
file_name <- paste0("log/01f_make_dual_mort_outcomes_", pct)
start_log_file(file_name = file_name)

# Data Read In -----------------------------------------------------------------
message("Reading in data...")

bsf <- fread(paste0(lib_base_data, "dual_pred_bsf_", pct, ".csv"))

sample <- fread(paste0(lib_base_data, "dual_pred_sample_",
                       pct, ".csv"))

# Identifying Death Dates ------------------------------------------------------
message("Identifying death dates...")
deaths <- bsf %>%
  .[, .(bene_id, death_dt)] %>%
  .[death_dt != "", ] %>%
  .[, .SD[1], by = bene_id] %>%
  .[, death_dt := mdy(death_dt)] %>%
  .[, `:=`(death_yr = year(death_dt), death_mo = month(death_dt))]

# Mortality Indicators ---------------------------------------------------------
message("Creating mortality indicator...")
mortality_outcomes <- sample %>%
  merge(deaths, by = "bene_id", allow.cartesian = T) %>%
  .[, death_dt := ymd(death_dt)] %>%
  .[, day1 := mdy(paste0(first_mo, "-01-", rfrnc_yr))] %>%
  .[, diff := as.numeric(death_dt - day1)] %>%
  .[diff %between% c(100, 360), ] %>%
  .[, mort := 1] %>%
  .[, .(bene_id, rfrnc_yr, mort)] %>%
  merge(sample, by = c("bene_id", "rfrnc_yr"), all.y = T)

mortality_outcomes[is.na(mortality_outcomes)] <- 0

dtp <- calc_cmean(mortality_outcomes, "mort", "first_mo") %>%
  .[order(first_mo)]

# Export -----------------------------------------------------------------------
message("Exporting...")
fwrite(mortality_outcomes, paste0(lib_base_data, "dual_pred_mortality_outcomes_",
                                  pct, ".csv"))

end_log_file()
