# ------------------------------------------------------------------------------
# Proj: Cost-sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc:

# Start Script -----------------------------------------------------------------
source("../00_pre_process/start_script.R")

option_list = list(
  make_option(c("-p", "--pct"), type='character', default = "20pct"),
  make_option(c("-f", "--first_year"), type='integer', default = 2007),
  make_option(c("-l", "--last_year"), type='integer', default = 2012)
)
unpack_opt(option_list)

# Start log file
file_name <- paste0("log/01c_make_dual_pred_sample_", pct)
start_log_file(file_name = file_name)

# Data Read In -----------------------------------------------------------------
message("Reading in data...")
# Subsetted bsf file (from ../01_sample/subset_dual_claims.sas)
bsf <- read_and_combine(lib_base_data, "dual_bsf", years, pct)

sample <- bsf[part_d_mo == end_mo] %>%
  .[mort == 0 | (death_mo >= birth_mo + 3)] %>%
  .[age1 <= 85 & age1 >= 66, ] %>%
  .[, .(bene_id, rfrnc_yr, birth_mo, cntrct01)] %>%
  .[, pdp := ifelse(substr(cntrct01, 1, 1) == "S", 1, 0)] %>%
  setnames("birth_mo", "first_mo") %>%
  .[first_mo %between% c(2, 9), ] %>%
  .[, cntrct01 := NULL]

set.seed(712)
keep_perc <- .5
sample_benes <- unique(sample[, bene_id])
keep_benes <- sample(sample_benes, floor(length(sample_benes) * keep_perc))
sample <- sample[bene_id %chin% keep_benes]

bsf_sub <- bsf %>%
  .[bene_id %chin% unique(sample$bene_id), ]

# Export -----------------------------------------------------------------------
message("Exporting...")
fwrite(sample, paste0(lib_base_data, "dual_pred_sample_",
                                  pct, ".csv"))

fwrite(bsf, paste0(lib_base_data, "dual_pred_bsf_",
                      pct, ".csv"))

end_log_file()
