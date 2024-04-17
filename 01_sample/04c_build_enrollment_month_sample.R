# Proj: Cost-sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Similar to analytic sample but allowed to die before December year 1,
#       so we can estimate the relationship between mortality and enrollment
#       month in the first 3 months of enrollment

# Start Script -----------------------------------------------------------------
package_list <- c("stringr")
source("../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type='character', default = "20pct")
)
unpack_opt(option_list)

# Start log file
start_log_file(paste0("log/04c_build_enrollment_month_sample_", pct))

# Data Read In -----------------------------------------------------------------
message("Reading in data...")

# Main sample

# Month level bsf data (from 03_transform bsf_to_long.R)
DT_all <- fread(paste0(lib_base_data, "month_level_bsf_", pct, ".csv"))

# Identify Early Deaths --------------------------------------------------------
message("Identifying early deaths...")

subset_vars <- c("keep_year", "keep_month", "part_d", "alive",
                 "keep_dual_cstshr_first", "keep_plan_type_first",
                 "keep_drug_benefit_type_first", "keep_no_ded_first",
                 "keep_non_snp_first", "keep_join_month",
                 "keep_icl_first", "keep_us")

DT_all %<>%
  .[, keep_month := ifelse(month == first_mo, 1, 0)]

subset_list <- subset_sample(DT = DT_all,
                             subset_vars = subset_vars,
                             progress = F)

DT_sample_first <-subset_list$DT_subset %>%
  .[first_mo %between% c(2, 9)]

# Export -----------------------------------------------------------------------
message("Exporting...")

fwrite(DT_sample_first, paste0(lib_base_data, "enrollment_month_sample_", pct,
                               ".csv"))

end_log_file()
