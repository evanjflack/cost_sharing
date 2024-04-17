# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Makes the "ramp up" figure showing that it takes time for enrollees to
#       reach steady state

# Start Script -----------------------------------------------------------------
package_list <- c("ggplot2", "estimatr")
source("../../../00_pre_process/start_script.R")
source("../../../00_pre_process/define_plot_theme.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct")
)
unpack_opt(option_list)

month_labs <- c("Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep")
month_labs2 <- month_labs[seq(1, 7, 2)]

start_log_file("log/02_make_ramp_up_figure")

# Read In Data -----------------------------------------------------------------
message("Reading in data...")

model_vars <- c("bene_id", "first_mo")
DT_model <- fread(paste0(lib_base_data, "revision_model_data_", pct,
                         ".csv")) %>%
  .[, model_vars, with = FALSE]

part_d_cost_mo <- fread(paste0(lib_base_data, "part_d_cost_mo_new_enrollee_",
                               pct, ".csv"))

# Ramp Up ----------------------------------------------------------------------
message("Plotting ramp up...")

ramp_up <- part_d_cost_mo %>%
  .[, .(bene_id, year, month, cost)] %>%
  .[year %in% c(1, 2), ] %>%
  merge(DT_model[, .(bene_id, first_mo)]) %>%
  .[(year == 1 & month >= first_mo) | (year == 2 & month < first_mo), ] %>%
  .[order(bene_id, year, month)] %>%
  .[, cum_cost := ave(cost, bene_id, FUN=cumsum)] %>%
  .[, month1 :=  seq(1, .N), by = bene_id] %>%
  .[, any := ifelse(cum_cost > 0, 1, 0)]

dtp <- calc_cmean(ramp_up, y = "any", x = c("first_mo", "month1")) %>%
  .[, first_mo := factor(first_mo, labels = month_labs)]

p <- ggplot(dtp) +
  aes(x = month1, y = mean, color = factor(first_mo)) +
  geom_line() +
  labs(x = "Months Since Enrollment", y = "% with Any Claim",
       color = "Enrollment Month") +
  scale_x_continuous(breaks = seq(1, 11, 2)) +
  scale_color_brewer(palette = "Spectral") +
  my_theme_paper +
  theme(legend.position = c(.8, .4))

ggsave(paste0(lib_base, "plots/appendix/a2_ramp_up.png"), p,
       width = 6, height = 3.5)

# End --------------------------------------------------------------------------

end_log_file()
