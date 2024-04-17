# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Subsets month level bsf data to the dual age 65 falsification sample

# Start Script -----------------------------------------------------------------
library(stringr)
source("../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type='character', default = "20pct")
)
unpack_opt(option_list)

# Start log file
start_log_file(paste0("log/04a_build_new_enrollee_sample_", pct))

# Data Read In -----------------------------------------------------------------
message("Reading in data...")

# Month level bsf data (from 03_transform bsf_to_long.R)
DT_all <- fread(paste0(lib_base_data, "month_level_bsf_", pct, ".csv"))

# Subset to Sample -------------------------------------------------------------
# Keep criteria
subset_vars <- c("keep_year",
                 "keep_month",
                 "part_d",
                 "keep_dual_cstshr_first",
                 "keep_no_ded_first",
                 "keep_icl_first",
                 "keep_plan_type_first",
                 "keep_drug_benefit_type_first",
                 "keep_non_snp_first",
                 "keep_us",
                 "alive")

subset_list <- subset_sample(DT = DT_all,
                             subset_vars = subset_vars,
                             progress = F)

DT_sample <- subset_list$DT_subset

# Export -----------------------------------------------------------------------

fwrite(DT_sample, paste0(lib_base_data, "new_enrollee_sample_", pct, ".csv"))

end_log_file()
