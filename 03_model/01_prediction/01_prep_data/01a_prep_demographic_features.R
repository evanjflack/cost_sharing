# Proj: Cost Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Preps demographic features (age, sex, race, geography, calendar year)
#       for ML risk prediction

# Start Script -----------------------------------------------------------------
library(fastDummies)
library(stringr)
source("../../../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type='character', default = "20pct"),
  make_option(c("-f", "--first_year"), type='integer', default = 2007),
  make_option(c("-l", "--last_year"), type='integer', default = 2012),
  make_option(c("-s", "--sample"), type='character',
              default = "new_enrollee")
)
unpack_opt(option_list)

start_log_file(file_name = paste0("log/01a_prep_demographic_features_", sample,
                                  "_", pct))

# Data Read In -----------------------------------------------------------------
message("Reading in Data...")

if (sample == 'dual_pred') {
  sample_benes <- fread(paste0(lib_base_data, sample, "_sample_", pct, ".csv"))


  DT_sample <- fread(paste0(lib_base_data, "dual_pred_bsf_",
                            pct, ".csv")) %>%
    merge(sample_benes[, .(bene_id, rfrnc_yr)], by = c("bene_id", "rfrnc_yr"))

  id_vars <- c("bene_id", "rfrnc_yr")
} else {
  DT_sample <- fread(paste0(lib_base_data, "revision_model_data_", pct,
                            ".csv"))

  id_vars <- "bene_id"
}


# Age Polynomials --------------------------------------------------------------
message("Age Polynomials...")
DT_age <- DT_sample %>%
  .[, c(id_vars, "age1"), with = FALSE]
for (i in 2:3) {
  DT_age[, paste0("age", i) := age1^i]
}

fwrite(DT_age, paste0(lib_base_data, "DT_age_", sample, "_", pct, ".csv"))

remove(DT_age)

# Sex and Race -----------------------------------------------------------------
message("Sex and race indicators...")

DT_dem <- DT_sample %>%
  .[, c(id_vars, "race", "sex"), with = FALSE] %>%
  .[, female := ifelse(sex == 2, 1, 0)] %>%
  .[, `:=`(race_unknown = ifelse(race == 0, 1, 0),
           race_white = ifelse(race == 1, 1, 0),
           race_black = ifelse(race == 2, 1, 0),
           race_asian = ifelse(race == 4, 1, 0),
           race_hisp = ifelse(race == 5, 1, 0),
           race_native = ifelse(race == 6, 1, 0))] %>%
  .[, `:=`(sex = NULL, race = NULL)]

fwrite(DT_dem, paste0(lib_base_data, "DT_dem_", sample, "_", pct, ".csv"))

remove(DT_dem)

# State (Indicators) -----------------------------------------------------------
message("State indicators...")
DT_state <- DT_sample %>%
  .[, c(id_vars, "state_cd"), with = FALSE] %>%
  .[is.na(state_cd), state_cd := 99] %>%
  dummy_cols(select_columns = "state_cd") %>%
  .[, state_cd := NULL] %>%
  setnames(names(.), gsub("_cd_", "_", names(.)))

fwrite(DT_state, paste0(lib_base_data, "DT_state_", sample, "_", pct, ".csv"))

remove(DT_state)

# End --------------------------------------------------------------------------

end_log_file()
