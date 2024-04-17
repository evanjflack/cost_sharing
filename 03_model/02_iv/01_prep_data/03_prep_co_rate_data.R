# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Calculates EOY coinsurance rates + average prices faced by individuals
#       in the sample.

# Start Script -----------------------------------------------------------------
source("../../../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct")
)
unpack_opt(option_list)

# Start log file
start_log_file(paste0("log/03_prep_co_rate_data_", pct))

# Read In/Prep Sample Data -----------------------------------------------------

# Sample
sample_vars <- c("bene_id", "rfrnc_yr", "cntrct", "pbp", "first_mo")
sample <- fread(paste0(lib_base_data, "revision_model_data_", pct,
                         ".csv")) %>%
  .[, sample_vars, with = FALSE]
rm(sample_vars)

# ICL/CCL amounts by year
icl_oopt <- fread(paste0(lib_base_data, "icl_oopt_std_amts_sample.csv"))

sample %<>%
  merge(icl_oopt, by = "rfrnc_yr")

# EOY total spending + OOP spending
cost_by_mo <- fread(paste0(lib_base_data, "sample_spending_by_month_", pct,
                           ".csv")) %>%
  .[, c("bene_id", "cum_cost_1_12", "cum_plan_oop_cost_1_12"), with = FALSE] %>%
  setnames(c("cum_cost_1_12", "cum_plan_oop_cost_1_12"),
           c("cum_cost", "cum_oop_cost"))

sample %<>%
  merge(cost_by_mo, by = "bene_id")

sample %<>%
  .[, c("bene_id", "cntrct", "pbp", "rfrnc_yr", "icl_amt", "oopt_amt",
        "cum_cost", "cum_oop_cost"), with = FALSE]

# Read In/Prep Price Data ------------------------------------------------------

# Coinsurance rate by plan arm
co_rates_plan <- read_and_combine(lib_base_data, "co_rates_plan",
                                  seq(2007, 2012), pct) %>%
  .[dual == 0, ] %>%
  .[, .(cntrct, pbp, rfrnc_yr, arm, sum_co_rate_overall)] %>%
  setnames("sum_co_rate_overall", "co_rate") %>%
  .[, co_rate := co_rate*100]

# Average OOP price by plan/arm
prices_plan <- read_and_combine(lib_base_data, "prices_plan",
                               seq(2007, 2012), pct) %>%
  .[dual == 0, ] %>%
  .[, .(cntrct, pbp, rfrnc_yr, arm, mean_price_overall)] %>%
  setnames("mean_price_overall", "price")

price_by_class <- read_and_combine(lib_base_data, "atc_prices_plan",
                                seq(2007, 2012), pct) %>%
  .[dual == 0, ] %>%
  .[, .(cntrct, pbp, rfrnc_yr, arm, atc3, obs, mean_price_overall)] %>%
  setnames("mean_price_overall", "oop_cost")

# price_by_class <- read_and_combine(lib_base_data,
#                                  "co_rates_atc3", seq(2007, 2012), pct) %>%
#   .[dual == 0, ] %>%
#   .[, dual := NULL]

ace_arb_price_by_class <- price_by_class %>%
  .[atc3 %chin% c("C09C", "C09A")] %>%
  .[, .(oop_cost = weighted.mean(oop_cost, obs),
        obs = sum(obs)),
    by = .(cntrct, pbp, rfrnc_yr, arm)] %>%
  .[, atc3 := "ace_arb"]

price_by_class %<>%
  rbind(ace_arb_price_by_class)

price_by_class %<>%
  .[atc3 %chin% c("C10A", "C07A", "ace_arb"), ] %>%
  dcast(cntrct + pbp + rfrnc_yr + arm ~ atc3, value.var = "oop_cost")

# Calculate Coinsurance Rates/Prices -------------------------------------------

sample_co_rates <- copy(sample) %>%
  .[, arm := ifelse(cum_cost <= icl_amt, "pre",
                    ifelse(cum_cost > icl_amt & cum_oop_cost <= oopt_amt,
                           "gap", "cat"))] %>%
  .[, .(bene_id, cntrct, pbp, rfrnc_yr, arm)] %>%
  merge(co_rates_plan, by = c("cntrct", "pbp", "rfrnc_yr", "arm"),
        all.x = T) %>%
  .[, agg_co_rate := mean(co_rate, na.rm = T),
    by = c("rfrnc_yr", "arm")] %>%
  .[is.na(co_rate), co_rate := agg_co_rate] %>%
  .[, agg_co_rate := NULL]

sample_co_rates %<>%
  merge(prices_plan, by = c("cntrct", "pbp", "rfrnc_yr", "arm"),
        all.x = T) %>%
  .[, agg_price := mean(price, na.rm = T),
    by = c("rfrnc_yr", "arm")] %>%
  .[is.na(price), price := agg_price] %>%
  .[, agg_price := NULL]

sample_co_rates %<>%
  merge(price_by_class, by = c("cntrct", "pbp", "rfrnc_yr", "arm"),
        all.x = T) %>%
  .[, agg_C10A := mean(C10A, na.rm = T),
    by = c("rfrnc_yr", "arm")] %>%
  .[is.na(C10A), C10A := agg_C10A] %>%
  .[, agg_C07A := mean(C07A, na.rm = T),
    by = c("rfrnc_yr", "arm")] %>%
  .[is.na(C07A), C07A := agg_C07A] %>%
  .[, agg_ace_arb := mean(ace_arb, na.rm = T),
    by = c("rfrnc_yr", "arm")] %>%
  .[is.na(ace_arb), ace_arb := agg_ace_arb] %>%
  .[, `:=`(agg_C10A = NULL, agg_C07A = NULL, agg_ace_arb = NULL)] %>%
  setnames(c("C10A", "C07A", "ace_arb"),
           paste0("price_", c("statin", "beta_blocker", "ace_arb"))) %>%
  .[, `:=`(cntrct = NULL, pbp = NULL, rfrnc_yr = NULL)]

# Export -----------------------------------------------------------------------
message("Exporting...")

fwrite(sample_co_rates, paste0(lib_base_data, "sample_co_rates_all_", pct,
                               ".csv"))

end_log_file()
