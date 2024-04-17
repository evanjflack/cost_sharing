# Header -----------------------------------------------------------------------
# Proj: Cost Sharing
# Author: Evan Flack (eflack@stanford.edu)
# Desc: Estimates overall cost sharing in coinsurance by plan/year/coverage-arm
#       across all beneficiaries in a plan (not just the 65 year olds)

# Start Script -----------------------------------------------------------------

library(stringr)
library(foreach)

source("../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct"),
  make_option(c("-f", "--first_year"), type='integer', default = 2007),
  make_option(c("-l", "--last_year"), type='integer', default = 2012)
)
unpack_opt(option_list)

start_log_file(paste0("log/05_estimate_plan_cost_sharing_", pct))

atc3_classes <- c("C10A", "C09A", "C03A", "C07A", "C09C", "C08C")
lab_prod_xwalk <- fread(paste0(lib_base_data,
                               "ndc9_atc4_xwalk_20pct.csv"))
lab_prod_xwalk %<>%
  .[, .(lab_prod, rxcui, atc4)] %>%
  .[, lab_prod := str_pad(lab_prod, 9, pad = "0")] %>%
  cbind(str_split_fixed(.[, atc4], "; ", 50)) %>%
  melt(id.var = c("lab_prod", "rxcui", "atc4"),
       value.name = "atc4_new") %>%
  .[!(atc4_new == ""), ] %>%
  .[order(lab_prod), ] %>%
  .[, .(lab_prod, rxcui, atc4_new)] %>%
  setnames("atc4_new", "atc4") %>%
  .[, atc3 := substr(atc4, 1, 4)] %>%
  .[atc3 %in% atc3_classes, ] %>%
  .[, .(lab_prod, atc3)]


# Calculate Price and Coinsurance Rates ----------------------------------------
message("Calculating average prices/coinsurance rates...")

ret <- foreach(year = years) %do%
{
  # Read in data
  pde <- fread(paste0(lib_base_data, "plan_pde_", year, "_", pct, ".csv")) %>%
    .[drcvstcd == "C"]

  bsf <- fread(paste0(lib_base_data, "plan_bsf_", year, "_", pct, ".csv"))

  # Prep data
  pde %<>%
    .[bnftphas %chin% c("PP", "II", "CC"), ] %>%
    merge(bsf, by = "bene_id")

  rm(bsf)

  # Define if a claim had any dual/lics help
  pde %<>%
    .[, dual := ifelse(lics_amt > 0, 1, 0)]

  # Calculate average coinsurance rates/price
  co_rates <- pde %>%
    .[, co_rate := ptpayamt/totalcst] %>%
    .[totalcst == 0, co_rate := 0] %>%
    .[, .(sum_totalcst = sum(totalcst), sum_ptpayamt = sum(ptpayamt),
          mean_oop_price_overall = mean(ptpayamt),
          mean_co_rate_overall = mean(co_rate), obs = .N),
      by = .(cntrct, pbp, bnftphas, dual)] %>%
    .[, sum_co_rate_overall := sum_ptpayamt/sum_totalcst] %>%
    .[, arm := ifelse(bnftphas == "PP", "pre",
                      ifelse(bnftphas == "II", "gap", "cat"))] %>%
    .[, rfrnc_yr := year] %>%
    .[, .(cntrct, pbp, rfrnc_yr, arm, dual, obs, mean_co_rate_overall,
          sum_co_rate_overall)]

  fwrite(co_rates, paste0(lib_base_data, "co_rates_plan_", year, "_", pct,
                          ".csv"))
  rm(co_rates)

  prices <- pde %>%
    .[, .(mean_price_overall = mean(ptpayamt), obs = .N),
      by = .(cntrct, pbp, bnftphas, dual)] %>%
    .[, arm := ifelse(bnftphas == "PP", "pre",
                      ifelse(bnftphas == "II", "gap", "cat"))] %>%
    .[, rfrnc_yr := year] %>%
    .[, .(cntrct, pbp, rfrnc_yr, arm, dual, obs, mean_price_overall)]

  fwrite(prices, paste0(lib_base_data, "prices_plan_", year, "_", pct,
                        ".csv"))

  rm(prices)

  atc_prices <- pde %>%
    .[, lab_prod := str_pad(lab_prod, width = 9, pad = "0")] %>%
    merge(lab_prod_xwalk, by = "lab_prod") %>%
    .[, .(mean_price_overall = mean(ptpayamt), obs = .N),
      by = .(cntrct, pbp, bnftphas, dual, atc3)] %>%
    .[, arm := ifelse(bnftphas == "PP", "pre",
                      ifelse(bnftphas == "II", "gap", "cat"))] %>%
    .[, rfrnc_yr := year] %>%
    .[, .(cntrct, pbp, rfrnc_yr, arm, dual, atc3, obs, mean_price_overall)]

  fwrite(atc_prices, paste0(lib_base_data, "atc_prices_plan_", year, "_", pct,
                        ".csv"))

  message("Done: ", year)
}

# End --------------------------------------------------------------------------

end_log_file()
