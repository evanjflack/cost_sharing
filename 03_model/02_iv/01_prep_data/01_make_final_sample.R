# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Makes main analyitical data set

# Start Script -----------------------------------------------------------------
source("../../../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct")
)
unpack_opt(option_list)

start_log_file("log/01_make_final_sample")

library(stringr)

# Read in Data -----------------------------------------------------------------

# Read in Potential Sample
sample_vars <- c("bene_id", "rfrnc_yr", "first_mo", "cntrct", "pbp",
                 "race", "sex", "bene_zip", "birth_mo", "age1", "plan_type",
                 "keep_join_month", "state_cd")
sample <- fread(paste0(lib_base_data, "new_enrollee_sample_", pct, ".csv")) %>%
  .[, sample_vars, with = FALSE]
rm(sample_vars)

# Spending in first 90 days
initial_spending <- fread(paste0(lib_base_data,
                                 "initial_part_d_spending_new_enrollee_", pct,
                                 ".csv")) %>%
  .[, .(bene_id, initial_cost_90)]

acs5 <- fread(paste0(lib_base_data, "acs_5year_2012_clean.csv")) %>%
  .[, zip5 := str_pad(zip5, 5, pad = "0")] %>%
  .[, .(zip5, med_inc)]

# ICL amount by year
icl_cat <- fread(paste0(lib_base_data, "icl_std_amts.csv")) %>%
  .[, .(rfrnc_yr, icl_std)]

# Prep Data --------------------------------------------------------------------

# Subset to Feb-Sep enrolless that join in the IEP
sample %<>%
  .[keep_join_month == 1, ] %>%
  .[first_mo %in% seq(2, 9), ]

n1 <- nrow(sample)

# Make 5 digit zip codes
sample %<>%
  .[, bene_zip := str_pad(bene_zip, 9, pad = "0")] %>%
  .[, zip5 := substr(bene_zip, 1, 5)] %>%
  .[, bene_zip := NULL]

# Merge initial spending
DT_model <- sample %>%
  merge(initial_spending, by = "bene_id")

# Define year bin, standard initial spending bin and plan FE variables
DT_model %<>%
  .[, year_cut := ifelse(rfrnc_yr <= 2010, "2007-2010", "2011-2012")] %>%
  .[, pred_cut1 := ifelse(initial_cost_90 <= quantile(initial_cost_90, .7),
                          1, ifelse(initial_cost_90 <=
                                      quantile(initial_cost_90, .97), 2, 3)),
    by = first_mo] %>%
  .[, cntrct_pbp_rfrnc_yr := paste(cntrct, pbp, rfrnc_yr, sep = "_")]

# Merge in Zip5 income
DT_model %<>%
  merge(acs5, by = "zip5", all.x = T)

# Define monthly pre-gap coverage
DT_model %<>%
  merge(icl_cat, by = "rfrnc_yr") %>%
  .[, months_remain := 12 - first_mo + 1] %>%
  .[, cov_per_month := icl_std / months_remain] %>%
  .[, cov_per_month100 := cov_per_month / 100]

# Check that there are still the same number of observations as in original
# sample
n2 <- nrow(DT_model)
if (n1 == n2) {
  message("Merge successful")
} else {
  message("MERGE FAILED")
}

# Export -----------------------------------------------------------------------
message("Exporting...")

fwrite(DT_model, paste0(lib_base_data, "revision_model_data_", pct, ".csv"))

end_log_file()
