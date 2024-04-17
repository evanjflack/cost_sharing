# Proj: Cost Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Creates mortality outcomes for new enrollees from June of year 1

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

# Start log file
start_log_file(paste0("log/01a_make_new_enrollee_mortality_outcomes_", pct))

# Read in Data -----------------------------------------------------------------

# Subsetted bsf file (from ../01_sample/subset_new_enrollee_claims.sas)
bsf <- read_and_combine(lib_base_data, "sample_bsf", years, pct)

# New enrollees
pde_benes <- fread(paste0(lib_base_data, "pde_benes_", pct, ".csv"))

# Prep Data --------------------------------------------------------------------
message("Prepping data...")

# Beneficiaries that have any death date, format date variables
deaths <- bsf %>%
  .[, .(bene_id, death_dt)] %>%
  .[death_dt != "", ] %>%
  .[, .SD[1], by = bene_id] %>%
  .[, death_dt := mdy(death_dt)] %>%
  .[, death_year := year(death_dt)] %>%
  .[, death_mo := month(death_dt)] %>%
  .[, death_day := day(death_dt)] %>%
  .[, death_half := ifelse(death_day <= 15, 1, 2)]



# Merge with join year
deaths %<>%
  merge(pde_benes[, .(bene_id, join_year)])

# Make Mortality Outcomes ------------------------------------------------------
message("Making mortality outcomes...")

# Indicators for death in month
deaths %<>%
  .[, rel_death_year := death_year - join_year + 1] %>%
  .[, rel_death_mo := (death_mo - 5) + (rel_death_year - 1)*12] %>%
  .[rel_death_mo %between% c(1, 43), ]

# By Full Month ----------------------------------------------------------------
message("By full month...")
for (i in 1:43) {
  deaths %<>%
    .[, paste0("jun_mort_", i) := ifelse(rel_death_mo == i, 1, 0)]
}

deaths %<>%
  .[, c("bene_id", paste0("jun_mort_", seq(1, 43))), with = FALSE]

deaths <- merge(deaths, pde_benes[, .(bene_id)], by = "bene_id", all.y = T)
deaths[is.na(deaths)] <- 0

deaths[, jun_alive_1 := 1]
for (i in 2:43) {
  deaths %<>%
    .[, paste0("jun_alive_", i) := ifelse(rowSums(.SD) == 0, 1, 0),
      .SDcols = paste0("jun_mort_", seq(1, i - 1))]
}

fwrite(deaths, paste0(lib_base_data, "mort_outcomes_jun_year_1_", pct,
                      ".csv"))

# End --------------------------------------------------------------------------

end_log_file()
