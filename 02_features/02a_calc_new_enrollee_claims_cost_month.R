# Proj: Cost-Sharing
# Author Evan Flack (flack@stanford.edu)
# Desc: Calculates Part D spending by month (total and OOP), as well as
#       cumulative measures for each within a year, over first two years of
#       enrollment.

# Start Script -----------------------------------------------------------------
library(stringr)
library(tidyr)
library(lubridate)
source("../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type='character', default = "20pct"),
  make_option(c("-f", "--first_year"), type='integer', default = 2007),
  make_option(c("-l", "--last_year"), type='integer', default = 2013)
)
unpack_opt(option_list)

# Start log file
start_log_file(paste0("log/02a_calc_new_enrollee_claims_cost_by_month_",
                      pct))

# Data Read In -----------------------------------------------------------------
message("Reading in data...")

# Subsetted pde file (from ../01_sample/02a_subset_new_enrollee_claims.sas)
pde <- read_and_combine(lib_base_data, "sample_pde", years, pct) %>%
  .[, .(bene_id, rfrnc_yr, srvc_mo, srvc_dt, lab_prod, bnftphas, totalcst,
        ptpayamt, lics_amt, othtroop, plro_amt, cpp_amt, npp_amt,
        dayssply)] %>%
  .[, lab_prod := str_pad(lab_prod, 9, pad = "0")]

# List of new enrollee join months and years
pde_benes <- fread(paste0(lib_base_data, "pde_benes_", pct, ".csv")) %>%
  .[, .(bene_id, join_year)]

# Prep Data --------------------------------------------------------------------
message("Prepping data...")

# Merge in bene join year. Define year relative to join year (1 = join year).
# Subset to claims in first 2 years or Jan of year 3. Merge in branded xwalk.
pde %<>%
  merge(pde_benes, by = "bene_id") %>%
  .[, year := rfrnc_yr - join_year + 1] %>%
  .[year <= 2 | (year == 3 & srvc_mo == 1), ]

# Calculate the out of pocket cost medicare uses to determine catastrophic
# coverage. In 2011 onward, there is a (starting at) 50% discount for branded
# drugs in the coverage gap. However, the amount of this discount also applies
# to the out of pocket limit where the catastophic kicks in. The discount amount
# is equal to the difference in the total cost and the sum of (1) amount paid by
# the patient, (2) the covered/non covered amounts, (3) the amount paid for by
# LIS and, (4) two other payment amounts.
pde %<>%
  .[, almost_total := ptpayamt + cpp_amt +
      npp_amt + lics_amt + othtroop + plro_amt] %>%
  .[, gap_ind := ifelse(grepl("I", bnftphas), 1, 0)] %>%
  .[is.na(gap_ind), gap_ind := 0] %>%
  .[, change_ind := ifelse(rfrnc_yr >= 2011, 1, 0)] %>%
  .[, manuf_disc := ifelse(gap_ind == 1 & change_ind == 1,
                           totalcst - almost_total, 0)] %>%
  .[abs(manuf_disc) < .01, manuf_disc := 0] %>%
  .[, plan_oop_cost := ptpayamt + othtroop + manuf_disc]

# Subset variables and rename
pde %<>%
  .[, .(bene_id, year, srvc_mo, dayssply, totalcst, ptpayamt,
        plan_oop_cost)] %>%
  setnames(c("srvc_mo", "totalcst", "ptpayamt"), c("month", "cost", "oop_cost"))


# Calculate Claims by Month ----------------------------------------------------
message("Calculating claims by month...")

# Count the number of claims and sum the days supply of pills by month. Cast to
# wide. Fill in 0s for those with no claims.
claims_by_mo <- pde %>%
  .[, .(claims = .N, pill_days = sum(dayssply)),
    by = .(bene_id, year, month)] %>%
  dcast(bene_id ~ year + month, value.var = c("claims", "pill_days")) %>%
  merge(pde_benes[, .(bene_id)], all.y = T)
claims_by_mo[is.na(claims_by_mo)] <- 0

# Export
fwrite(claims_by_mo, paste0(lib_base_data,
                            "part_d_claims_pill_days_mo_new_enrollee_",
                            pct, ".csv"))

rm(claims_by_mo)


# Calculate Spending by Month --------------------------------------------------
message("Calculating spending by month...")

# Make a "long" version of pde benes that includes all years (1-3) and months
# (1-12)
year <- c(1, 2, 3)
month <- seq(1, 12)
years_months <- crossing(year, month) %>%
  as.data.table() %>%
  .[!(year == 3 & month > 1), ]
pde_benes_long <- crossing(pde_benes[, .(bene_id)], years_months) %>%
  as.data.table()

# Sum cost variables by bene/year/month, merge with long version
cost_by_mo <- pde %>%
  .[, lapply(.SD, sum), by = .(bene_id, year, month),
    .SDcols = c("cost", "oop_cost", "plan_oop_cost")] %>%
  merge(pde_benes_long, by = c("bene_id", "year", "month"), all.y = T)
cost_by_mo[is.na(cost_by_mo)] <- 0

# Make cumulative cost variables by year
cost_by_mo %<>%
  .[order(bene_id, year, month)] %>%
  .[, cum_cost := ave(cost, bene_id, year, FUN = cumsum)] %>%
  .[, cum_oop_cost := ave(oop_cost, bene_id, year, FUN = cumsum)] %>%
  .[, cum_plan_oop_cost := ave(plan_oop_cost, bene_id, year, FUN = cumsum)]

# Cast to wide (subset of months)
cost_wide <- cost_by_mo %>%
  .[(year == 1 & month >= 8) | (year == 2 & month <= 4), ] %>%
  dcast(bene_id ~ year + month, value.var = "cost") %>%
  setnames(names(.)[-1], paste0("cost_", names(.)[-1]))

# Export
fwrite(cost_by_mo, paste0(lib_base_data, "part_d_cost_mo_new_enrollee_",
                          pct, ".csv"))

fwrite(cost_wide, paste0(lib_base_data, "part_d_cost_mo_new_enrollee_wide_",
                         pct, ".csv"))

# End --------------------------------------------------------------------------

end_log_file()
