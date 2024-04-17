# Proj: Cost-Sharing
# Author: Evan Flack (evanjflack@gmail.com)
# Desc:

# Start Script -----------------------------------------------------------------
source("../../../00_pre_process/start_script.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct")
)
unpack_opt(option_list)

start_log_file("log/02a_prep_spending_data")

# Reading In Data --------------------------------------------------------------
message("Reading in data...")

# Final sample
sample_benes <- fread(paste0(lib_base_data, "revision_model_data_", pct, ".csv")) %>%
  .[, .(bene_id, first_mo)]

for (i in 8:11) {
  sample_benes1 <- fread(paste0(lib_base_data, "new_enrollee_sample_month_",
                                i, "_", pct, ".csv")) %>%
    .[, .(bene_id, first_mo)]
  sample_benes %<>%
    rbind(sample_benes1) %>%
    unique()
  rm(sample_benes1)
}

# Part D Spending Measures by Month
part_d_cost_mo <- fread(paste0(lib_base_data, "part_d_cost_mo_new_enrollee_",
                               pct, ".csv")) %>%
  .[bene_id %chin% sample_benes$bene_id, ]

# Subset/Transform to Wide -----------------------------------------------------
message("Subsetting/transforming to wide...")

# Calculating cumulative total spending in first 12 month of enrollment
one_year_cost <- part_d_cost_mo %>%
  merge(sample_benes, by = "bene_id") %>%
  .[(year == 1 & month >= first_mo) | (year == 2 & month < first_mo)] %>%
  .[, .(one_year_cost = sum(cost)), by = bene_id]

cost_wide <- part_d_cost_mo %>%
  .[(year <= 2)] %>%
  dcast(bene_id ~ year + month,
        value.var = c("oop_cost", "cum_cost", "cum_oop_cost",
                      "cum_plan_oop_cost", "cost"))

rm(part_d_cost_mo)

# Export -----------------------------------------------------------------------
message("Exporting...")

fwrite(one_year_cost, paste0(lib_base_data, "sample_first_year_total_spending_",
                             pct, ".csv"))

fwrite(cost_wide, paste0(lib_base_data, "sample_spending_by_month_", pct,
                         ".csv"))

end_log_file()
