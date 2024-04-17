# ------------------------------------------------------------------------------
# Proj: Behavioral Hazard
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Identifies unique plans in sample (cntrc/pbp/rfrnc_yr combinations)
#       This is then used to subset pde claims to all people in these plans and
#       calculate plan level cost sharing measures.
# ------------------------------------------------------------------------------

# Start Script -----------------------------------------------------------------
source("../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type='character', default = "20pct")
)
unpack_opt(option_list)

# Start log file
start_log_file(paste0("log/05a_id_sample_plans_", pct))

# Data Read In -----------------------------------------------------------------
message("Reading in data...")

# Month level bsf data (from 03_transform bsf_to_long.R)
DT_all <- fread(paste0(lib_base_data, "month_level_bsf_", pct, ".csv"))

DT_plans <- DT_all %>%
  .[month == first_mo &
      rfrnc_yr <= 2012, .(cntrct, pbp, rfrnc_yr)] %>%
  .[substr(cntrct, 1, 1) %in% c("S", "H", "R")] %>%
  unique()

# Export -----------------------------------------------------------------------
message("Exporting...")

fwrite(DT_plans, paste0(lib_base_data, "new_enrollee_plans_", pct, ".csv"))

end_log_file()
