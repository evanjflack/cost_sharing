# Header -----------------------------------------------------------------------
# Proj: Cost-Sharing
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Creates indicators for acute cardiac, respritory, and diabetes related
#       events from inpatient (ip) claims (in dual prediction sample) in the
#       dual 66+ sample. Used as the outcome in the risk prediction.

# Start Script -----------------------------------------------------------------
library(lubridate)
library(stringr)

source("../../../00_pre_process/start_script.R")
option_list = list(
  make_option(c("-p", "--pct"), type='character', default = "20pct"),
  make_option(c("-f", "--first_year"), type='integer', default = 2007),
  make_option(c("-l", "--last_year"), type='integer', default = 2013)
)
unpack_opt(option_list)

# Additional User Inputs
ami_codes <- as.character(seq(410, 411))
stroke_codes <- as.character(seq(433, 435))
suicide_codes <- paste0("E95", seq(0, 9))
od_codes <- as.character(seq(960, 979))
od_e_codes <- paste("E9", seq(30, 49))
source("01e_list_diab_codes.R")

# Start log file
start_log_file(paste0("log/01e_id_dual_acute_outcomes_", pct))

# Data Read In -----------------------------------------------------------------
message("Reading in data...")

# List of all benes in the dual 66+ sample
dual_sample <- fread(paste0(lib_base_data, "dual_pred_sample_", pct, ".csv"))

# Dual sample inpatient claims
ip <- read_and_combine(lib_base_data, "dual_ip", years, pct)

# Prep Data --------------------------------------------------------------------
message("Preping data...")

# Keep first 6 procedures and diagnoses
ip %<>%
  .[clm_ln == 1, ]

message("Prepping data...")
ip_list <- unpack_ip(ip, 10, 6)
ip_diag <- ip_list$ip_diag
ip_prcdr <- ip_list$ip_prcdr

remove(ip, ip_list)

# Subset Time ------------------------------------------------------------------
message("Subsetting based on when events occured...")

ip_diag %<>%
  merge(dual_sample, by = "bene_id", allow.cartesian = T) %>%
  .[order(bene_id, clm_id), ] %>%
  .[, from_dt := mdy(from_dt)] %>%
  .[, day1 := mdy(paste0(first_mo, "-01-", rfrnc_yr))] %>%
  .[, diff := as.numeric(from_dt - day1)] %>%
  .[diff %between% c(90, 360)]

ip_prcdr %<>%
  merge(dual_sample, by = "bene_id", allow.cartesian = T) %>%
  .[order(bene_id, clm_id), ] %>%
  .[, prcdrdt := mdy(prcdrdt)] %>%
  .[, day1 := mdy(paste0(first_mo, "-01-", rfrnc_yr))] %>%
  .[, diff := as.numeric(prcdrdt - day1)] %>%
  .[diff %between% c(90, 360)]

# Acute Diagnosis --------------------------------------------------------------
message("Creating acute diagnosis indicators...")

acute_diag <- create_diag_indicators(DT = ip_diag,
                                     DT_id = dual_sample,
                                     id_var = c("bene_id", "rfrnc_yr"),
                                     ami_codes = ami_codes,
                                     stroke_codes = stroke_codes,
                                     diab_codes = all_diab_codes,
                                     suicide_codes = suicide_codes,
                                     od_codes = od_codes,
                                     od_e_codes = od_e_codes)

# Acute Procedures -------------------------------------------------------------
message("Creating acute procedure indicators...")

acute_prcdr <- create_prcdr_indicators(DT = ip_prcdr,
                                       DT_id = dual_sample,
                                       id_var = c("bene_id", "rfrnc_yr"))

# Combine ----------------------------------------------------------------------
message("Combine procedures and diagnosis...")

acute <- acute_diag %>%
  merge(acute_prcdr, by = c("bene_id", "rfrnc_yr")) %>%
  .[, comp_card := ifelse(ami == 1 | stroke == 1, 1, 0)] %>%
  .[, comp_resp := ifelse(resp_fail == 1 | resp_arr == 1 | tube == 1 |
                            vent == 1, 1, 0)] %>%
  .[, comp_mental := ifelse(od == 1 | suicide == 1, 1, 0)]

# Print summary statistics
print(acute[, lapply(.SD, mean), .SDcols = names(acute)[-c(1, 2)]])

# Export -----------------------------------------------------------------------
message("Exporting...")

fwrite(acute, paste0(lib_base_data, "dual_pred_acute_event_", pct, ".csv"))

end_log_file()

