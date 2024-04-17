# Proj: Cost-sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Makes enrollment timing figure

# Start Script -----------------------------------------------------------------

library(ggplot2)

source("../../../00_pre_process/start_script.R")
source("../../../00_pre_process/define_plot_theme.R")

# Command line args
option_list = list(
  make_option(c("-p", "--pct"), type = 'character', default = "20pct")
)
unpack_opt(option_list)

month_labs <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul",
                "Aug", "Sep", "Oct", "Nov", "Dec")

start_log_file("log/01_make_enrollemt_timing_figure")

# Read In Data -----------------------------------------------------------------
message("Reading in data...")
DT_sample <- fread(paste0(lib_base_data, "new_enrollee_sample_", pct, ".csv"))

# Timing -----------------------------------------------------------------------
message("Determining enrollment timing...")

DT_sample %<>%
  .[, first_mo1 := (join_year - year_65)*12 + first_mo] %>%
  .[, rel_month := first_mo1 - birth_mo]

# Plot -------------------------------------------------------------------------
message("Plotting figure...")

calc_norm_perc <- function(DT, x1, x2) {
  dt_norm_perc <- DT %>%
    .[, .(.N), by = c(x1, x2)] %>%
    merge(DT[, .(.N), by = x2], by = x2) %>%
    .[, norm_perc := N.x/N.y] %>%
    .[, `:=`(N.x = NULL, N.y = NULL)]
  return(dt_norm_perc)
}

dtp <- calc_norm_perc(DT_sample[rel_join_month %in% seq(-1, 11)], "first_mo", "birth_mo") %>%
  .[, birth_mo := factor(birth_mo, labels = month_labs)]

p <- ggplot(dtp) +
  aes(x = first_mo, y = norm_perc) +
  geom_bar(stat = "identity") +
  facet_wrap(~ birth_mo) +
  scale_x_continuous(breaks = seq(1, 12, 3),
                     labels = c("Jan", "Apr", "Jul", "Oct")) +
  labs(x = "Enrollment Month", y = "Percentage of Enrollees") +
  my_theme_paper

ggsave(paste0(lib_base, "plots/appendix/a1_enrollment_timing.png"), p,
       width = 6, height = 3.5)

sample1 <- DT_sample %>%
  .[first_mo %in% seq(2, 9), ] %>%
  .[rel_join_month %in% seq(-1, 11)]

message(round(mean(sample1$keep_join_month), 3) * 100, "% enroll in IEP")
message(round(mean(sample1$first_mo == sample1$birth_mo), 3) * 100,
        "% enroll in birth month")

# End --------------------------------------------------------------------------
end_log_file()
