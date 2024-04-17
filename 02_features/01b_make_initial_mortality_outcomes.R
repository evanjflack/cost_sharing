# Proj: Cost Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Creates mortality outcomes for new enrollees in their 30-, 60-, 90- days
#       of enrollment

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
start_log_file(paste0("log/01b_make_initial_mortality_outcomes_", pct))

# Data Read In -----------------------------------------------------------------

# Subsetted bsf file (from ../01_sample/subset_new_enrollee_claims.sas)
bsf <- read_and_combine(lib_base_data, "sample_bsf", years, pct)

pde_benes <- fread(paste0(lib_base_data, "pde_benes_", pct, ".csv"))

# Identify Death Dates ---------------------------------------------------------
# Beneficiaries that have any death date
deaths <- bsf %>%
  .[, .(bene_id, death_dt)] %>%
  .[death_dt != "", ] %>%
  .[, .SD[1], by = bene_id] %>%
  .[, death_dt := mdy(death_dt)]

# Mortality Indicators ---------------------------------------------------------

# Define months relative to their join year
initial_mort_outcomes <- deaths %>%
  merge(pde_benes[, .(bene_id, join_year, first_mo)], by = "bene_id") %>%
  .[, day1 := mdy(paste0(first_mo, "-01-", join_year))] %>%
  .[, rel_death_days := as.numeric(death_dt - day1)] %>%
  .[rel_death_days > 0, ]

# Indiciators for whether the person died in a month, or was still alive
for (i in seq(30, 90, 30)) {
  initial_mort_outcomes %<>%
    .[, paste0("initial_mort_", i) := ifelse(rel_death_days %between%
                                               c(i - 29, i), 1, 0)] %>%
    .[, paste0("cum_initial_mort_", i) := ifelse(rel_death_days <= i, 1, 0)]
}

# Fill in 0s for the rest of the sample
initial_mort_outcomes %<>%
  .[, c("bene_id", paste0("initial_mort_", seq(30, 90, 30)),
        paste0("cum_initial_mort_", seq(30, 90, 30))),
    with = FALSE] %>%
  merge(pde_benes, by = 'bene_id', all.y = TRUE)
initial_mort_outcomes[is.na(initial_mort_outcomes)] <- 0

# Export -----------------------------------------------------------------------

fwrite(initial_mort_outcomes, paste0(lib_base_data,
                                     "initial_mortality_outcomes_", pct,
                                     ".csv"))

end_log_file()
