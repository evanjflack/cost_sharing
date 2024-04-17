#  Proj: Cost-sharing
# Author: Evan Flack (evanjflack@gmail.com)
# Desc: Transforms year level sample of old/disabled beneficiaries to long


# Start Script -----------------------------------------------------------------

source("../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type='character', default = "20pct"),
  make_option(c("-f", "--first_year"), type='integer', default = 2007),
  make_option(c("-l", "--last_year"), type='integer', default = 2012)
)
unpack_opt(option_list)

library(foreach)
library(lubridate)
library(stringr)

# Additional user inputs
# Codes for medicaid dual eligibility/low income subsidy eligibility
full_dual <- c("02", "04", "08")
partial_dual <- c("01", "03", "05", "06")
any_dual <- c(full_dual, partial_dual)
full_cstshr <- c("01", "02", "03")
partial_cstshr <- c("04", "05", "06", "07", "08")
any_cstshr <- c(full_cstshr, partial_cstshr)

# Included plans (1 = HMO, 2 = HMO POS, 4 = Local PPO, 9 = PFFS, 29 = PDP,
#                 31 = Regional PPO)
inc_plans <- c(1, 2, 4, 9, 29, 31)

# States codes for US 50 states plus DC (we are excluding 40 = PR and 38 = VI)
# plus others with codes above 53
us51 <- seq(1, 53)[-c(40, 48)]

# Start log file
start_log_file(paste0("log/06b_transform_old_disabled_bsf_to_long_", pct))

# Iterate over years -----------------------------------------------------------
message("Iterating over years...")

ret <- foreach(year = years) %do%
  {

    # Data Read In -------------------------------------------------------------
    # OASI benes 66 and older and disabled benes 64 and younger
    bsf <- fread(paste0(lib_base_data, "old_dis_bsf_", year, "_", pct,
                        ".csv"))

    # Standard ICL/CCL locations
    icl_cat <- fread(paste0(lib_base_data, "icl_std_amts.csv"))

    # Part d plan characteristics (from read_in_pln.sas)
    pln <- read_and_combine(lib_base_data, "pln", years, "20pct") %>%
      setnames(c("contract_id", "plan_id"), c("cntrct", "pbp"))

    # Identify Relevant Variables ----------------------------------------------
    # year level variables to keep
    year_level_vars <- c("bene_id", "birth_dt", "birth_mo", "birth_yr", "race",
                         "sex", "age", "age1", "bene_zip", "state_cd",
                         "death_yr", "death_mo")
    # alive on first day of month indicators
    alive_vars <- paste0("alive_", seq(1, 12))
    # monthly enrollment/eligibility variables
    # part d contract
    cntrct_vars <- grep("cntrct", names(bsf), value = T)
    # part d pbp
    pbp_vars <- grep("pbpid", names(bsf), value = T)
    # medicaid dual eligibility
    dual_vars <- grep("dual_", names(bsf), value = T)
    # low income cost sharing
    cstshr_vars <- grep("cstshr", names(bsf), value = T)
    # retirement subsidy
    rds_vars <- grep("rdsind", names(bsf), value = T)

    # Standardize Date Variables -----------------------------------------------
    bsf %<>%
      # birth variables
      .[, birth_dt := mdy(bene_dob)] %>%
      .[, birth_yr := year(birth_dt)] %>%
      .[, birth_mo := month(birth_dt)] %>%
      # year the bene turned 65
      .[, year_65 := birth_yr + 65] %>%
      # death variables
      .[, death_dt := mdy(death_dt)] %>%
      .[, death_yr := year(death_dt)] %>%
      .[, death_mo := month(death_dt)] %>%
      # replacing NAs for death_yr/death_mo with 9999
      .[is.na(death_yr), death_yr := 9999] %>%
      .[is.na(death_mo), death_mo := 9999]

    # Reshape to Long ----------------------------------------------------------

    # Month indicators for whether bene was alive on the first of that month
    for (i in 1:12) {
      bsf[, paste0("alive_", i) :=
              ifelse(death_yr == rfrnc_yr & death_mo < i, 0, 1)]
    }

    alive_long <- reshape_month_level(bsf, "bene_id", alive_vars, "alive")
    cntrct_long <- reshape_month_level(bsf, "bene_id",  cntrct_vars, "cntrct")
    pbp_long <- reshape_month_level(bsf, "bene_id", pbp_vars, "pbp")
    dual_long <- reshape_month_level(bsf, "bene_id", dual_vars, "dual")
    cstshr_long <- reshape_month_level(bsf, "bene_id", cstshr_vars, "cstshr")
    rds_long <- reshape_month_level(bsf, "bene_id", rds_vars, "rds")

    # Combining all long data.tables
    DT <- bsf[, .(bene_id, rfrnc_yr)] %>%
      merge(alive_long, by = c("bene_id")) %>%
      merge(cntrct_long, by = c("bene_id", "month")) %>%
      merge(pbp_long, by = c("bene_id", "month")) %>%
      # .[rfrnc_yr == 2013, pbp := str_pad(pbp, 3, pad = "0")] %>%
      merge(dual_long, by = c("bene_id", "month")) %>%
      merge(cstshr_long, by = c("bene_id", "month")) %>%
      merge(rds_long, by = c("bene_id", "month"))

    rm(alive_long, cntrct_long, pbp_long, dual_long, cstshr_long, rds_long)

    # Part D coverage indicators
    DT %<>%
      .[, part_d := ifelse((rfrnc_yr <= 2009 & cntrct != "" & cntrct != "0"
                            & cntrct != "N" & cntrct != "*" & cntrct != "X") |
                             (rfrnc_yr >= 2010 & cntrct != "" & cntrct != "0"
                              & cntrct != "N" & cntrct != "*"), 1, 0)]

    # Subset to only Oct-Jan
    DT %<>%
      .[month %in% c(4, 5, 6, 7, 8, 9, 10, 11, 12)]

    n1 <- nrow(DT)

    # Merge in Plan Characteristics --------------------------------------------
    # Merge in standard icl/ccl locations by calendar year
    pln %<>%
      merge(icl_cat, by = "rfrnc_yr") %>%
      # Format of PBP variable changes in 2013, need to pad with missing 0s
      .[rfrnc_yr == 2013, pbp := str_pad(pbp, 3, pad = "0")]

    # Merge plan characteristics and year level variables into month level data
    DT %<>%
      merge(pln, by = c("rfrnc_yr", "cntrct", "pbp"), all.x = T) %>%
      merge(bsf[, year_level_vars, with = F], by = "bene_id")

    # Define mortality
    DT %<>%
      .[, mort := ifelse(death_yr == rfrnc_yr & death_mo == month, 1, 0)]

    # Standardizing NA variables -----------------------------------------------
    DT %<>%
      .[is.na(cstshr), cstshr := "99"] %>%
      .[, cstshr := str_pad(cstshr, 2, pad = "0")] %>%
      .[is.na(dual), dual :="99"] %>%
      .[, dual := str_pad(dual, 2, pad = "0")] %>%
      .[is.na(plan_type), plan_type := 9999] %>%
      .[is.na(snp_type), snp_type := "9999"] %>%
      .[is.na(drug_benefit_type), drug_benefit_type := 9999] %>%
      .[is.na(ded_apply), ded_apply := 9999] %>%
      .[is.na(icl_amt), icl_amt := 9999] %>%
      .[is.na(state_cd), state_cd := 9999] %>%
      .[is.na(bene_zip), bene_zip := 999999999] %>%
      .[is.na(rds), rds := "0"]

    # Order data
    DT %<>%
      .[order(bene_id, rfrnc_yr, month), ]

    # Define Sample Inclusion Criteria -----------------------------------------

    DT %<>%
      #  no dual/LIS eligibility
      .[, dual_cstshr_ind := ifelse(cstshr %in% any_cstshr |
                                      dual %in% any_dual, 1, 0)] %>%
      # hmo, hmopos, local ppo, pffs, pdp plans
      .[, keep_plan_type := ifelse(plan_type %in% inc_plans, 1, 0)] %>%
      # non-missing drug benefit type
      .[, keep_drug_benefit_type := ifelse(drug_benefit_type %in% c(1, 2, 3, 4),
                                           1, 0)] %>%
      # keep benes with no deductible
      .[, keep_no_ded := ifelse(ded_apply == 3, 1, 0)] %>%
      # keep benes with standard icl amounts
      .[, keep_icl := ifelse(icl_amt == icl_std, 1, 0)] %>%
      # keep benes in US 50 states + DC
      .[, keep_us := ifelse(state_cd %in% us51, 1, 0)] %>%
      # non-missing zip code
      .[, keep_zip := ifelse(bene_zip != 999999999, 1, 0)] %>%
      .[, keep_non_snp := ifelse(snp_type == "0", 1, 0)]

    # Export -------------------------------------------------------------------
    # Check for the same amount of rows after all that merging
    n2 <- nrow(DT)
    # message(n1 == n2)

    fwrite(DT, paste0(lib_base_data, "old_disabled_month_level_bsf_", year,
                      "_", pct, ".csv"))

    message("Done ", year, ".")
  }

# End --------------------------------------------------------------------------

end_log_file()
