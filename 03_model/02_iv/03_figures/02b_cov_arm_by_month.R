# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Makes Figure 2 Panel B (proportion of beneficiaries in the coverage gap)

# Start Script -----------------------------------------------------------------
library(ggplot2)
source("../../../00_pre_process/start_script.R")
source("../../../../supporting_code/define_plot_theme.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct")
)
unpack_opt(option_list)

# Colors and x-axis labels for plot
pre_col <- "lightblue3"
dh_col <- "red3"
month_labs <- c("Feb", "Apr", "Jun", "Aug")

# Start log file
start_log_file("log/02b_plot_cov_arm_by_month")

# Read In Data -----------------------------------------------------------------

# Main model data
model_vars <- c("bene_id", "first_mo", "rfrnc_yr")
DT_model <- fread(paste0(lib_base_data, "revision_model_data_", pct,
                         ".csv")) %>%
  .[, model_vars, with = FALSE]
rm(model_vars)

cost_wide <- fread(paste0(lib_base_data, "sample_spending_by_month_", pct,
                           ".csv")) %>%
  .[, c("bene_id", grep("cum_cost_", names(.), value = T),
        grep("cum_plan_oop_cost_", names(.), value = T)), with = FALSE]

# Part D c overage phase spending amounts
icl_oopt <- fread(paste0(lib_base_data, "icl_oopt_std_amts_sample.csv"))

# Prep Data --------------------------------------------------------------------

# Transform spending data from long to wide
# cost_wide <- part_d_cost_mo %>%
#   .[year == 1, ] %>%
#   dcast(bene_id ~ year + month,
#         value.var = c("cum_cost", "cum_plan_oop_cost"))
# rm(part_d_cost_mo)

# Merge in spending amount and limits
DT_model %<>%
  merge(cost_wide, by = "bene_id") %>%
  merge(icl_oopt, by = "rfrnc_yr")

# Calculate monthly pre-gap coverage
DT_model %<>%
  .[, months_remain := 12 - first_mo + 1] %>%
  .[, cov_per_month := icl_amt / months_remain]

# Coverage Arm by Month --------------------------------------------------------

# Calculate the percentage of beneficiaries from each enrollment month that are
# in each coverage phase for each month of year 1
dtp <- data.table()
for (i in seq(2, 12)) {
  DT_model %<>%
    .[, cum_cost := get(paste0("cum_cost_1_", i))] %>%
    .[, plan_cost := get(paste0("cum_plan_oop_cost_1_", i))] %>%
    .[, paste0("arm_", i) := ifelse(cum_cost <= icl_amt, "pre",
                                    ifelse(cum_cost > icl_amt &
                                             plan_cost <= oopt_amt,
                                           "gap", "cat"))]

  dtp1 <- DT_model %>%
    .[, .(obs = .N), by = c("first_mo", paste0("arm_", i))] %>%
    .[, total_obs := sum(obs), by = .(first_mo)] %>%
    .[, perc := obs/total_obs] %>%
    setnames(paste0("arm_", i), "arm") %>%
    .[, month := i] %>%
    .[, .(month, arm, first_mo, perc)]

  dtp %<>% rbind(dtp1)
}

# Format -----------------------------------------------------------------------
dtp %<>%
  .[, perc := perc*100] %>%
  .[, arm := factor(arm, levels = c("pre", "gap", "cat"),
                    labels = c("Initial Coverage", "Coverage Gap",
                               "Catastrophic"))] %>%
  .[, first_mo1 := ifelse(first_mo %in% c(2, 9), first_mo, 3)] %>%
  .[, first_mo1 := factor(first_mo1, levels = c(2, 3, 9),
                          labels = c("February", "March-\nAugust",
                                     "September"))]

# Average monthly pre-gap coverage amount by enrollment month
dtp_cov <- calc_cmean(DT_model, y = "cov_per_month",
                      x = c("first_mo"), se = T) %>%
  .[, .(first_mo, mean)] %>%
  .[, mean := round(mean, 0)] %>%
  setnames("mean", "cov_per_month") %>%
  .[order(first_mo), ]

# Labels for plot lines
feb_lab <- paste0("February Enrollees\n($", dtp_cov[first_mo == 2, cov_per_month],
                  " monthly\npre-donut budget)")
mar_aug_lab <- paste0("March-August  ($",
                      dtp_cov[first_mo == 3, cov_per_month], "-",
                      dtp_cov[first_mo == 8, cov_per_month], ")")
sep_lab <- paste0("September ($", dtp_cov[first_mo == 9, cov_per_month], ")")

# Plot -------------------------------------------------------------------------

p_arm <- ggplot(dtp[month >= first_mo & arm == "Coverage Gap" ]) +
  aes(x = month, y = perc, color = factor(first_mo1), group = factor(first_mo),
      alpha = factor(first_mo1)) +
  geom_line() +
  scale_color_manual(values = c("red3", "gray", "dodgerblue4")) +
  scale_alpha_manual(values = c(1, .33, 1)) +
  scale_x_continuous(breaks = seq(1, 12, 3),
                     limits = c(1, 12),
                     labels = c("Jan", "Apr", "Jul", "Oct")) +
  scale_y_continuous(limits = c(-1, 12.25),
                     breaks = seq(0, 12, 3)) +
  labs(x = "Calendar Month (Year 1)", y = "% of Enrollees in Donut Hole",
       color = "Enrollment Month", alpha = "Enrollment Month") +
  my_theme_paper +
  annotate(geom=  "text", x = 5, y = 5.5, label = feb_lab,
           size = 3.5) +
  annotate(geom = "text", x = 10.5, y = -.5, label = sep_lab, size = 3.5) +
  annotate(geom = "text", x = 9, y = 2, label = mar_aug_lab, size = 3.5) +
  theme(legend.position = "none")


ggsave(paste0(lib_base, "plots/02b_cov_arm_by_month.png"),
       p_arm,  width = 6, height = 3)


feb_gap <- dtp[arm == "Coverage Gap" & first_mo == 2 & month == 12, perc]
sep_gap <- dtp[arm == "Coverage Gap" & first_mo == 9 & month == 12, perc]


perc_diff <- round((feb_gap - sep_gap) / sep_gap * 100, 0)

message("Feb enrollees are ", perc_diff,
        "% more likely to be in the coverage gap than Sep enrollees")
# End --------------------------------------------------------------------------

end_log_file()
