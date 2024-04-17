# Proj: Cost-sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Estimates reduced form of monthly mortality on birth month in
#       falsification samples (dual oasi 66-85, non-dual oasi 66-85, and dual
#       disabled 50-64)

# Start Script -----------------------------------------------------------------

source("../../../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type='character', default = "20pct"),
  make_option(c("-f", "--first_year"), type='integer', default = 2007),
  make_option(c("-l", "--last_year"), type='integer', default = 2012),
  make_option(c("-t", "--type"), type='character', default = 'dis')
)
unpack_opt(option_list)

library(iterators)
library(foreach)
library(parallel)
library(doParallel)
library(tidyr)
library(estimatr)
library(stringr)

# Start log file
start_log_file(paste0("log/05a_estimate_oos_false_", type))

# Read in Data -----------------------------------------------------------------
message("Reading in data...")

# Jan-Mar total part d spending for each of the three samples above
init_cost <- read_and_combine(lib_base_data, "old_false_init_cost", years, pct)



if (type == 'dual') {
  # Dual 66-85 sample
  DT_fit <- read_and_combine(lib_base_data, "dual_older_sample", years,
                             pct) %>%
    .[age1 %between% c(66, 85)] %>%
    .[, sample_type := "dual"] %>%
    merge(init_cost, by = c("bene_id", "rfrnc_yr"), all.x = T) %>%
    .[is.na(initial_cost3), initial_cost3 := 0] %>%
    .[birth_mo %in% seq(2, 9)]
}

if (type == 'dis') {
  # Disabled + dual 50-64 sample
  DT_fit <- read_and_combine(lib_base_data, "dual_disabled_sample", years,
                             pct) %>%
    .[age1 %between% c(50, 64)] %>%
    .[, sample_type := "dis"] %>%
    merge(init_cost, by = c("bene_id", "rfrnc_yr"), all.x = T) %>%
    .[is.na(initial_cost3), initial_cost3 := 0] %>%
    .[birth_mo %in% seq(2, 9)]
}

if (type == 'non_dual') {
  DT_fit <- read_and_combine(lib_base_data, "non_dual_older_sample",
                             years, pct) %>%
    .[age1 %between% c(70, 85), ] %>%
    .[, sample_type := "non_dual"] %>%
    merge(init_cost, by = c("bene_id", "rfrnc_yr"), all.x = T) %>%
    .[is.na(initial_cost3), initial_cost3 := 0] %>%
    .[birth_mo %in% seq(2, 9)]
}



# Prep Data --------------------------------------------------------------------
message("Prepping data...")

# icl_oopt <- fread(paste0(lib_base_data, "icl_oopt_std_amts_sample.csv"))
#
# DT_fit %<>%
#   merge(icl_oopt, by = "rfrnc_yr")
#
# DT_fit %<>%
#   .[, months_remain := 12 - birth_mo + 1] %>%
#   .[, cov_per_month := icl_std / months_remain] %>%
#   .[, cov_per_month100 := cov_per_month / 100]


# Estimation Grid --------------------------------------------------------------
message("Prepping estimation grid...")

month <- c(4, 5, 6, 7, 8, 9, 10, 11, 12)

if (type == 'dual') {
  age1 <- seq(66, 85)
  grid <- crossing(month, age1) %>%
    as.data.table() %>%
    .[, sample_type := "dual"]
}

if (type == "dis") {

  age1 <- seq(50, 64)
  grid <- crossing(month, age1) %>%
    as.data.table() %>%
    .[, sample_type := "dis"]
}

type <- "non_dual"
if (type == 'non_dual') {
  age1 <- seq(70, 85)
  grid <- crossing(month, age1) %>%
    as.data.table() %>%
    .[, sample_type := "non_dual"]
}

message(nrow(grid), " specifications.")

grid %<>%
  .[, spec := seq(1, nrow(grid))]

instrument <- "birth_mo"
# Estimate Reduced Form --------------------------------------------------------
message("Estimating reduced form...")

# registerDoParallel(cores = 8)
dt_fit <- foreach(month_sub = grid$month,
                  age1_sub = grid$age1,
                  sample_type_sub = grid$sample_type,
                  spec = grid$spec,
                  .combine = "rbind") %do%
  {

    message(spec, "/", nrow(grid))

    # Subset to age, calendar month, and dual status, define initial spending
    # bins
    DT_fit1 <- DT_fit %>%
      .[month == month_sub &
          age1 == age1_sub &
          sample_type == sample_type_sub, ] %>%
      .[, pred_cut1 := ifelse(initial_cost3 <= quantile(initial_cost3, .7), 1,
                              ifelse(initial_cost3 <= quantile(initial_cost3,
                                                               .97), 2, 3)),
        by = birth_mo] %>%
      .[, cntrct_pbp_rfrnc_yr := paste(cntrct, pbp, rfrnc_yr, sep = "")] %>%
      .[, z := get(instrument)]


    # By initial spending
    base_mort_pred <- calc_cmean(DT_fit1, "mort", "pred_cut1") %>%
      setnames("mean", "base_mort") %>%
      .[, variable := NULL] %>%
      .[, pred_cut1 := as.character(pred_cut1)]

    fit_pred <- lm_robust(mort ~ z:factor(pred_cut1) +
                            factor(pred_cut1) + factor(race) + factor(sex),
                          data = DT_fit1,
                          se_type = "stata", fixed_effects = ~ cntrct_pbp_rfrnc_yr)

    dt_fit1 <- fit_to_dt(fit_pred, "z", "pred_cut1") %>%
      .[, `:=`(type = "pred")] %>%
      merge(base_mort_pred, by = "pred_cut1") %>%
      .[, `:=`(month = month_sub, age1 = age1_sub,
               sample_type = sample_type_sub)] %>%
      .[pred_cut1 == 2, ]
  }
# stopImplicitCluster()

# Export -----------------------------------------------------------------------
message("Exporting...")

fwrite(dt_fit, paste0(lib_base_data, "oos_false_estimates_",
                      type, "_", instrument, "_", pct, ".csv"))

end_log_file()
