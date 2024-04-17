# Proj: Cost-sharing
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Builds analytic sample i the same way as the main december sample but
#       for June-November of year 1

# Start Script -----------------------------------------------------------------
library(stringr)
source("../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type='character', default = "20pct")
)
unpack_opt(option_list)

# Start log file
start_log_file(paste0("log/04b_build_pre_dec_samples_", pct))

# Data Read In -----------------------------------------------------------------
message("Reading in data...")

# Month level bsf data (from 03_transform bsf_to_long.R)
DT_all <- fread(paste0(lib_base_data, "month_level_bsf_", pct, ".csv"))

# Identify Early Deaths --------------------------------------------------------
message("Identifying early deaths...")

subset_vars <- c("keep_year", "keep_month", "part_d", "alive",
                 "keep_dual_cstshr_first", "keep_plan_type_first",
                 "keep_join_month",
                 "keep_drug_benefit_type_first", "keep_no_ded_first",
                 "keep_non_snp_first", "keep_icl_first", "keep_us")

for (i in c(6, 7, 8, 9, 10, 11)) {
  message(i)

  DT_all %<>%
    .[, keep_month := ifelse(month == i, 1, 0)]

  subset_list <- subset_sample(DT = DT_all,
                               subset_vars = subset_vars,
                               progress = F)

  DT_sample <- subset_list$DT_subset %>%
    .[first_mo %between% c(2, 9), ]

  fwrite(DT_sample, paste0(lib_base_data, "new_enrollee_sample_month_", i, "_",
                           pct, ".csv"))
}

end_log_file()
