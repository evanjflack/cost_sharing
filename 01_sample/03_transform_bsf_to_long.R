# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Preps new enrollee bsf data for sample subsetting by transforming wide
#       (year level) data to long (year + month level), merging in plan
#       information, standardizing missing values, and defining sample
#       subsetting "keep" criteria.

# Start Script -----------------------------------------------------------------
library(lubridate)
library(stringr)
source("../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type='character', default = "20pct"),
  make_option(c("-f", "--first_year"), type='integer', default = 2007),
  make_option(c("-l", "--last_year"), type='integer', default = 2013)
)
unpack_opt(option_list)

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

# Calendar months to include in model
months <- 12

# Months of "ramp up" (months enrolled before the observed month) needed to
# be included in sample
ramp_up_months <- 3

# Max calendar year in sample
max_year <- 2012

# Start log file
start_log_file(paste0("log/03_transform_bsf_to_long_", pct))

# Data Read In -----------------------------------------------------------------
message("Reading in data...")

# Subsetted bsf file including all records from new enrollees
bsf <- read_and_combine(lib_base_data, file = "sample_bsf", years = years,
                        pct = pct)

# List of all new enrollees
pde_benes <- fread(paste0(lib_base_data, "pde_benes_", pct, ".csv"))

# Part d plan characteristics (from read_in_pln.sas)
pln <- read_and_combine(lib_base_data, "pln", years, "20pct") %>%
  setnames(c("contract_id", "plan_id"), c("cntrct", "pbp"))

# Standard ICL/CCL locations
icl_cat <- fread(paste0(lib_base_data, "icl_std_amts.csv"))

# Identify Relevant Variables --------------------------------------------------
message("Identifying relevant variables...")
# year level variables to keep
year_level_vars <- c("bene_id", "birth_dt", "birth_mo", "birth_yr", "race",
                     "sex", "age", "bene_zip", "state_cd", "death_dt")

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

# Subset to join year ----------------------------------------------------------
message("Subsetting to join year...")
bsf65 <- bsf %>%
  merge(pde_benes[, .(bene_id, join_year)],
        by.x = c("bene_id", "rfrnc_yr"),
        by.y = c("bene_id", "join_year"))

# Removing beneficiaries with multiple observations (which is usually due to
# having multiple birth dates on record)
bsf65 %<>%
  .[, obs := .N, by = bene_id]

mult_birth_dates <- bsf65[obs == 2, ]

bsf65 %<>%
  .[obs == 1, ]

# Standardize Date Variables ---------------------------------------------------
message("Standardizing date variables...")
bsf65 %<>%
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

# Reshape to Long --------------------------------------------------------------
message("Reshaping month level variables to long...")
# Month indicators for whether bene was alive on the first of that month
for (i in 1:12) {
  bsf65[, paste0("alive_", i) :=
          ifelse(death_yr == rfrnc_yr & death_mo < i, 0, 1)]
}

alive_long <- reshape_month_level(bsf65, "bene_id", alive_vars, "alive")
cntrct_long <- reshape_month_level(bsf65, "bene_id",  cntrct_vars, "cntrct")
pbp_long <- reshape_month_level(bsf65, "bene_id", pbp_vars, "pbp")
dual_long <- reshape_month_level(bsf65, "bene_id", dual_vars, "dual")
cstshr_long <- reshape_month_level(bsf65, "bene_id", cstshr_vars, "cstshr")
rds_long <- reshape_month_level(bsf65, "bene_id", rds_vars, "rds")

# Combining all long data.tables
DT <- bsf65[, .(bene_id, rfrnc_yr)] %>%
  merge(alive_long, by = c("bene_id")) %>%
  merge(cntrct_long, by = c("bene_id", "month")) %>%
  merge(pbp_long, by = c("bene_id", "month")) %>%
  .[rfrnc_yr == 2013, pbp := str_pad(pbp, 3, pad = "0")] %>%
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

# Merging in pde_benes (which has join_year and first_mo)
DT %<>%
  merge(pde_benes, by = "bene_id")

# Merge in Plan Characteristics ------------------------------------------------
message("Merging plan characteristics...")
# Merge in standard icl/ccl locations by calendar year
pln %<>%
  merge(icl_cat, by = "rfrnc_yr") %>%
  # Format of PBP variable changes in 2013, need to pad with missing 0s
  .[rfrnc_yr == 2013, pbp := str_pad(pbp, 3, pad = "0")]

# Merge plan characteristics and year level variables into month level data
DT %<>%
  merge(pln, by = c("rfrnc_yr", "cntrct", "pbp"), all.x = T) %>%
  merge(bsf65[, year_level_vars, with = F], by = "bene_id")

# Standardizing NA variables ---------------------------------------------------
message("Standardizing NA variables...")
DT %<>%
  .[is.na(cstshr), cstshr := "99"] %>%
  .[, cstshr := str_pad(cstshr, 2, pad = "0")] %>%
  .[is.na(dual), dual :="99"] %>%
  .[, dual := str_pad(dual, 2, pad = "0")] %>%
  .[is.na(plan_type), plan_type := 9999] %>%
  .[is.na(snp_type), snp_type := "9999"] %>%
  .[is.na(drug_benefit_type), drug_benefit_type := 9999] %>%
  .[is.na(ded_apply), ded_apply := 9999] %>%
  # Intentionally making values for icl_amt and icl_std different so people with
  # both missing are not included
  .[is.na(icl_amt), icl_amt := 9999] %>%
  .[is.na(icl_std), icl_std := 99999] %>%
  .[is.na(state_cd), state_cd := 9999] %>%
  .[is.na(bene_zip), bene_zip := 999999999] %>%
  .[is.na(rds), rds := "0"]

DT %<>%
  .[order(bene_id, rfrnc_yr, month), ]

# Define "Keep" Variables ------------------------------------------------------
message("Defining keep variables...")
# Define different keep criteria for sample. A value of 1 means that an
# and observation is kept if that criteria is used. For example,
# keep_dual_cstshr is defined as 1 for beneficiaries with no dual/LIS
# eligibility, because these are the individuals we want in the sample.

DT %<>%
  #  no dual/LIS eligibility
  .[, keep_dual_cstshr := ifelse(cstshr %in% any_cstshr |
                                   dual %in% any_dual, 0, 1)] %>%
  .[, keep_dual := ifelse(dual %in% any_dual, 0, 1)] %>%
  .[, keep_cstshr := ifelse(cstshr %in% any_cstshr, 0, 1)] %>%
  # Yes to dual/LIS eligibiility (for falsification sample)
  .[, keep_dual_cstshr_false := ifelse(cstshr %in% any_cstshr |
                                         dual %in% any_dual, 1, 0)] %>%
  # hmo, hmopos, local ppo, pffs, pdp plans
  .[, keep_plan_type := ifelse(plan_type %in% inc_plans, 1, 0)] %>%
  # keep non special needs plan
  .[, keep_non_snp := ifelse(snp_type == "0", 1, 0)] %>%
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
  # month of anaylsis (usually December)
  .[, keep_month := ifelse(month %in% months, 1, 0)] %>%
  # max year in sample
  .[, keep_year := ifelse(rfrnc_yr <= max_year, 1, 0)] %>%
  # enroll at age 65 only
  .[, keep_age := ifelse(age1 == 65, 1, 0)] %>%
  .[, year_65 := birth_yr + 65] %>%
  # Month joined relative to 65th birth month (first month eligible)
  .[, rel_join_month := ifelse(join_year == year_65, first_mo - birth_mo,
                               (birth_mo - first_mo - 12)*-1)] %>%
  # enroll in initial enrollment period
  .[, keep_join_month := ifelse(rel_join_month %between% c(-1, 3), 1, 0)] %>%
  .[, keep_join_month_no_early :=
      ifelse(rel_join_month %between% c(0, 3), 1, 0)] %>%
  # enroll "on time" (in birth month)
  .[, keep_same := ifelse(first_mo == birth_mo, 1, 0)]

# Define keep criteria based on the first month
message("Defining first month keep variables...")
first_mo_vars <- c("keep_dual_cstshr", "keep_dual_cstshr_false",
                   "keep_plan_type", "keep_non_snp", "keep_drug_benefit_type",
                   "keep_no_ded", "keep_icl")

first_mo <- DT %>%
  .[month == first_mo, ] %>%
  .[, c("bene_id", first_mo_vars), with = F] %>%
  setnames(first_mo_vars, paste0(first_mo_vars, "_first"))

DT %<>%
  merge(first_mo, by = "bene_id")

# Export -----------------------------------------------------------------------
message("Exporting...")

fwrite(DT, paste0(lib_base_data, "month_level_bsf_", pct, ".csv"))

end_log_file()
