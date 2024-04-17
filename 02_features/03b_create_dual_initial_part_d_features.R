# Header -----------------------------------------------------------------------
# Proj: Cost Sharing
# Author Evan Flack (evanjflack@gmail.com)
# Desc: Creates features based on initial part D claims (to be used to predict
#       end of year spending) for 66+ dual enrollees (ATC and spending bin
#       indicators)

# Start Script -----------------------------------------------------------------
library(lubridate)
library(stringr)
library(foreach)
library(doParallel)
source("../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type='character', default = "20pct"),
  make_option(c("-f", "--first_year"), type='integer', default = 2007),
  make_option(c("-l", "--last_year"), type='integer', default = 2012)
)
unpack_opt(option_list)


# Start log file
start_log_file(paste0("log/03b_create_dual_initial_part_d_features_", pct))

# Data Read In -----------------------------------------------------------------
message("Reading in data...")

atc_ind <- fread(paste0(lib_base_data, "atc_indicators_", "20pct", ".csv")) %>%
  .[, lab_prod := str_pad(lab_prod, 9, pad = "0")]

# Make Initial Features --------------------------------------------------------
message("Making initial features...")

registerDoParallel(cores = 3)
ret <- foreach(year = years) %dopar%
  {
    message(year)
    # Read in data
    dual_benes <- fread(paste0(lib_base_data, "dual_pred_sample_", pct, ".csv")) %>%
      .[rfrnc_yr == year, ]

    pde <- fread(paste0(lib_base_data, "dual_pde_", year, "_", pct, ".csv"))

    # Prep data
    pde %<>%
      .[, .(bene_id, rfrnc_yr, srvc_dt, lab_prod, totalcst)] %>%
      .[, lab_prod := str_pad(lab_prod, 9, pad = "0")] %>%
      # keep only pde records of benes in sample and in their first year
      merge(dual_benes, by = c("bene_id", "rfrnc_yr")) %>%
      # Date of benes first in Part D
      .[, first_day := mdy(paste0(first_mo, "-01-", rfrnc_yr))] %>%
      .[, srvc_dt := mdy(srvc_dt)] %>%
      # number of since join date that claim was filled
      .[, within_days := as.numeric(srvc_dt - first_day)] %>%
      setnames("totalcst", "cost") %>%
      .[within_days %between% c(0, 90), ]


    initial_atc <- pde %>%
      .[, .(bene_id, rfrnc_yr, lab_prod)] %>%
      merge(atc_ind, by = 'lab_prod') %>%
      .[, lab_prod := NULL] %>%
      .[, lapply(.SD, max), by = .(bene_id, rfrnc_yr)] %>%
      merge(dual_benes[, .(bene_id, rfrnc_yr)], on = c('bene_id', "rfrnc_yr"),
            all.y = TRUE)

    initial_atc[is.na(initial_atc)] <- 0

    fwrite(initial_atc, paste0(lib_base_data ,
                                "initial_atc_dual_", year, "_", pct,
                                ".csv"))

    rm(initial_atc)

    # Initial Spending
    initial_cost <- pde %>%
      .[bene_id %chin% dual_benes$bene_id] %>%
      .[, .(bene_id, rfrnc_yr, cost)] %>%
      .[, .(initial_cost_90 = sum(cost)), by = .(bene_id, rfrnc_yr)] %>%
      merge(dual_benes[, .(bene_id, rfrnc_yr)],
            by = c("bene_id", "rfrnc_yr"), all.y = T)
    initial_cost[is.na(initial_cost)] <- 0

    fwrite(initial_cost, paste0(lib_base_data ,
                                  "initial_part_d_cost_dual_", year, "_", pct,
                                  ".csv"))

    rm(initial_cost)


    initial_claims <- pde %>%
      .[, .(bene_id, rfrnc_yr)] %>%
      .[, .(n_claims_90 = .N), by = .(bene_id, rfrnc_yr)] %>%
      merge(dual_benes[, .(bene_id, rfrnc_yr)],
            by = c("bene_id", "rfrnc_yr"), all.y = T)
    initial_claims[is.na(initial_claims)] <- 0

    fwrite(initial_claims, paste0(lib_base_data ,
                                  "initial_part_d_claims_dual_", year, "_", pct,
                                  ".csv"))

    rm(initial_claims)

    message("Done: ", year)
  }
stopImplicitCluster()

# End --------------------------------------------------------------------------

end_log_file()
