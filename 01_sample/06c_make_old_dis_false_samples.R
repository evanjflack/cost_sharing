# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Makes the same sample subsets for the older/disabled/dual
#       falsification samples as in the main sample

# Start Script -----------------------------------------------------------------
source("../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct"),
  make_option(c("-f", "--first_year"), type = 'integer', default = 2007),
  make_option(c("-l", "--last_year"), type = 'integer', default = 2012)
)
unpack_opt(option_list)

library(foreach)
library(doParallel)
library(lubridate)
library(stringr)

# Start log file
start_log_file(paste0("log/06c_make_old_dis_false_subsets_", pct))

# registerDoParallel(cores = length(years))
old_benes <- foreach(year = years,
                     .combine = "rbind") %do%
  {
    # Data Read In -------------------------------------------------------------

    DT_all <- fread(paste0(lib_base_data, "old_disabled_month_level_bsf_", year,
                           "_", pct, ".csv"))

    # Subset to Disabled Sample ------------------------------------------------
    DT_all %<>%
      .[, keep_dis := ifelse(age1 <= 64, 1, 0)] %>%
      .[, keep_dual_cstshr := ifelse(dual_cstshr_ind == 1, 1, 0)] %>%
      .[, keep_age := ifelse(age1 %between% c(50, 65), 1, 0)]

    subset_vars <- c("part_d", "alive", "keep_dual_cstshr", "keep_dis",
                     "keep_age", "keep_plan_type","keep_drug_benefit_type",
                     "keep_non_snp", "keep_icl", "keep_us")

    subset_list <- subset_sample(DT = DT_all,
                                 subset_vars = subset_vars,
                                 progress = F)
    dis_sample <- subset_list$DT_subset

    fwrite(dis_sample, paste0(lib_base_data, "dual_disabled_sample_", year, "_",
                               pct, ".csv"))

    dis_benes <- dis_sample %>%
      .[, .(bene_id, rfrnc_yr)] %>%
      unique()

    rm(dis_sample)

    # Subset to Dual Older Sample ----------------------------------------------
    DT_all %<>%
      .[, keep_dual_cstshr := ifelse(dual_cstshr_ind == 1, 1, 0)] %>%
      .[, keep_age := ifelse(age1 %between% c(66, 85), 1, 0)]

    # Keep criteria
    subset_vars <- c("part_d", "alive", "keep_dual_cstshr", "keep_age",
                     "keep_plan_type","keep_drug_benefit_type", "keep_non_snp",
                     "keep_icl", "keep_us")

    subset_list <- subset_sample(DT = DT_all,
                                 subset_vars = subset_vars,
                                 progress = F)
    dual_sample <- subset_list$DT_subset

    fwrite(dual_sample, paste0(lib_base_data, "dual_older_sample_", year, "_",
                               pct, ".csv"))

    dual_benes <- dual_sample %>%
      .[, .(bene_id, rfrnc_yr)] %>%
      unique()

    rm(dual_sample)

    # Subset to Non-Dual Sample ------------------------------------------------
    DT_all %<>%
      .[, keep_dual_cstshr := ifelse(dual_cstshr_ind == 0, 1, 0)] %>%
      .[, keep_age := ifelse(age1 %between% c(66, 85), 1, 0)]

    # Keep criteria
    subset_vars <- c("part_d", "alive", "keep_dual_cstshr", "keep_age",
                     "keep_plan_type","keep_drug_benefit_type", "keep_non_snp",
                     "keep_no_ded", "keep_icl", "keep_us")

    subset_list <- subset_sample(DT = DT_all,
                                 subset_vars = subset_vars,
                                 progress = F)
    non_dual_sample <- subset_list$DT_subset

    fwrite(non_dual_sample, paste0(lib_base_data, "non_dual_older_sample_",
                                   year, "_", pct, ".csv"))

    non_dual_benes <- non_dual_sample %>%
      .[, .(bene_id, rfrnc_yr)]

    message("Done ", year, ".")

    old_benes1 <- rbind(non_dual_benes, dual_benes, dis_benes)

  }
stopImplicitCluster()

old_benes %<>%
  unique()

fwrite(old_benes, paste0(lib_base_data, "old_false_sample_benes_", pct,
                         ".csv"))

# End --------------------------------------------------------------------------

end_log_file()
