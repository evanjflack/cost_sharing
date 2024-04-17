# Proj: Cost-Sharing
# Author Evan Flack (flack@stanford.edu)
# Desc: Creates features based on initial part D claims (first 90 days)

# Start Script -----------------------------------------------------------------
source("../00_pre_process/start_script.R")

library(lubridate)
library(stringr)

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type='character', default = "20pct"),
  make_option(c("-f", "--first_year"), type='integer', default = 2007),
  make_option(c("-l", "--last_year"), type='integer', default = 2012)
)
unpack_opt(option_list)

# Additional user inputs
# Number of days to calculate for
initial_days <- seq(30, 270, 30)
# Types of features to compute
types <- c("atc", "cost", "claims")

# Start log file
file_name <- paste0("log/03a_create_new_enrollee_initial_part_d_features_", pct)
start_log_file(file_name = file_name)



# Data Read In -----------------------------------------------------------------
message("Reading in data...")

# Part D claims
pde <- read_and_combine(lib_base_data, "sample_pde", years, pct)

# List of benes with claims
pde_benes <- fread(paste0(lib_base_data, "pde_benes_", pct, ".csv"))


# Prep Data --------------------------------------------------------------------
message("Prepping data...")

pde %<>%
  .[, .(bene_id, rfrnc_yr, srvc_dt, lab_prod, totalcst, dayssply)] %>%
  .[, lab_prod := str_pad(lab_prod, 9, pad = "0")] %>%
  # keep only pde records of benes in sample and in their first year
  merge(pde_benes, by = "bene_id") %>%
  # Date of benes first in Part D
  .[, first_day := mdy(paste0(first_mo, "-01-", join_year))] %>%
  .[, srvc_dt := mdy(srvc_dt)] %>%
  # number of since join date that claim was filled
  .[, within_days := as.numeric(srvc_dt - first_day)] %>%
  # subset to claims within 90 days of enrollment
  .[within_days <= max(initial_days), ] %>%
  setnames(c("totalcst"), c("cost")) %>%
  .[, .(bene_id, within_days, lab_prod, cost,  dayssply)]

# Calculate Initial Part D Spending --------------------------------------------
if ("cost" %in% types) {
  message("Calculating initial part d total spending...")

  initial_cost <- pde %>%
    .[, .(bene_id, within_days, cost)]
  for (i in initial_days) {
    initial_cost %<>%
      .[, paste0("initial_cost_", i) := ifelse(within_days <= i, cost, 0)]
  }
  initial_cost %<>%
    .[, c("bene_id", paste0("initial_cost_", initial_days)), with = FALSE] %>%
    .[, lapply(.SD, sum), by = "bene_id"] %>%
    merge(pde_benes[, .(bene_id)], by = "bene_id", all.y = T)
  initial_cost[is.na(initial_cost)] <- 0

  # Export
  fwrite(initial_cost, paste0(lib_base_data,
                              "initial_part_d_spending_new_enrollee_", pct,
                              ".csv"))

  # rm(initial_cost)
}

# Number of Claims/Days Filled -------------------------------------------------
if ("claims" %in% types) {
  message("Calculating number of initial claims...")
  initial_days <- seq(30, 90, 30)
  initial_claims <- pde %>%
    .[, .(bene_id, within_days, dayssply)]
  for (i in initial_days) {
    initial_claims %<>%
      .[, paste0("n_claims_", i) := ifelse(within_days <= i &
                                             within_days >= 0, 1, 0)] %>%
      .[, paste0("n_days_", i) := ifelse(within_days <= i &
                                           within_days >= 0, dayssply, 0)]
  }

  initial_claims %<>%
    .[, within_days := NULL] %>%
    .[, dayssply := NULL] %>%
    .[, lapply(.SD, sum), by = bene_id] %>%
    merge(pde_benes[, .(bene_id)], on = 'bene_id', all.y = TRUE)
  initial_claims[is.na(initial_claims)] <- 0

  fwrite(initial_claims, paste0(lib_base_data ,
                                "initial_part_d_claims_new_enrollee_", pct,
                                ".csv"))

  rm(initial_claims)
}

# Make ATC Class Indicators ---------------------------------------------------
if ("atc" %in% types) {
  message("Calculating initial ATC indicators...")
  initial_days <- c(90)
  for (i in initial_days) {
    # ATC indicators (by ndc9)
    atc_ind <- fread(paste0(lib_base_data, "atc_indicators_", "20pct",
                            ".csv")) %>%
      .[, lab_prod := str_pad(lab_prod, 9, pad = "0")]

    initial_atc <- pde %>%
      .[within_days <= i] %>%
      .[, .(bene_id, lab_prod)] %>%
      merge(atc_ind, by = 'lab_prod') %>%
      .[, lab_prod := NULL] %>%
      .[, lapply(.SD, max), by = bene_id] %>%
      merge(pde_benes[, .(bene_id)], on = 'bene_id', all.y = TRUE)

    initial_atc[is.na(initial_atc)] <- 0

    # Export
    fwrite(initial_atc, paste0(lib_base_data, "initial_atc_", i,
                               "_days_new_enrollee_", pct, ".csv"))
    remove(initial_atc)
  }
}

# End --------------------------------------------------------------------------

end_log_file()
