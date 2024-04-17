# Proj: Cost-Sharing
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Subsets month level bsf data to the dual falsifications sample

# Start Script -----------------------------------------------------------------
library(stringr)
source("../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type='character', default = "20pct")
)
unpack_opt(option_list)

# Start log file
start_log_file(paste0("log/04d_build_dual_65_sample_", pct))

# Data Read In -----------------------------------------------------------------
message("Reading in data...")

# Month level bsf data (from 03_transform bsf_to_long.R)
DT_all <- fread(paste0(lib_base_data, "month_level_bsf_", pct, ".csv"))


# Subset to Sample -------------------------------------------------------------
message("Subsetting to dual false sample...")


subset_vars <- c("keep_year", "keep_month", "part_d",
                 "keep_dual_cstshr_false_first",
                 "keep_icl_first",
                 "keep_plan_type_first", "keep_drug_benefit_type_first",
                 "keep_non_snp_first", "keep_us",
                 "alive")

subset_list <- subset_sample(DT = DT_all,
                             subset_vars = subset_vars,
                             progress = F)

DT_sample <- subset_list$DT_subset

DT_sample %<>%
  .[first_mo %between% c(2, 9)]

# Export -----------------------------------------------------------------------
message("Exporting...")

fwrite(DT_sample, paste0(lib_base_data, "age65_dual_false_sample_",
                         pct, ".csv"))

end_log_file()
