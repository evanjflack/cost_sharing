# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Makes figure 2 panel A (part D design)

# Start Script -----------------------------------------------------------------
library(ggplot2)
source("../../../00_pre_process/start_script.R")
source("../../../00_pre_process/define_plot_theme.R")

# Colors and x-axis labels for plot
pre_col <- "lightblue3"
dh_col <- "red3"
cat_col <- "dodgerblue4"

# Start log file
start_log_file("log/02a_plot_part_d_design")

# Design -----------------------------------------------------------------------

icl_amt <- 2510
dh <- data.table(total = c(0, 275, 2510, 5726, 7000),
                 oop_i = c(0, 275, .25*(2510 - 275), 5726-2510,
                           .07*(7000-5726))) %>%
  .[, oop := ave(oop_i, FUN = cumsum)]

p_design <- ggplot(dh) +
  aes(x = total - icl_amt, y = oop) +
  geom_line() +
  geom_point(size = 2) +
  labs(x = "Total Spending (relative to ICL)", y = "Out-of-Pocket Cost") +
  annotate(geom = "rect", xmin = 0, xmax = 3216, ymin = 0, ymax = 4300,
           fill = dh_col, alpha = .5,color = NA) +
  annotate(geom = "rect", xmin = -2510 + 250, xmax = 0, ymin = 0, ymax = 4300,
           fill = pre_col, alpha = .5,color = NA) +
  annotate(geom = "rect", xmin = -2510 , xmax = -2510 + 250, ymin = 0,
           ymax = 4300,
           fill = "white", alpha = .5,color = NA) +
  annotate(geom = "rect", xmin = 3216, xmax = 7000 - icl_amt, ymin = 0,
           ymax = 4300,
           fill = cat_col, alpha = .5,color = NA) +
  annotate("text", x = 750 - icl_amt, y = 1500,
           label = "Deductible (100%)", size = 3) +
  annotate("segment", x = 500 - icl_amt, xend = 125 - icl_amt,  y = 1200,
           yend = 200, arrow = arrow(angle = 15,
                                     length = unit(.15, "inches"))) +

  annotate("text", x = 1600 - icl_amt, y = 2400,
           label = "Initial Coverage (25%)",  size = 3) +
  annotate("segment", x = -700, xend = -500,  y = 2200, yend = 800,
           arrow = arrow(angle = 15, length = unit(.15, "inches"))) +
  annotate("text", x = 3400 - icl_amt, y = 3500,
           label = "Gap/Donut Hole (100%)", size = 3) +
  annotate("segment", x = 800, xend = 3700 - icl_amt,  y = 3200, yend = 2200,
           arrow = arrow(angle = 15, length = unit(.15, "inches"))) +
  annotate("text", x = 6000 - icl_amt, y = 2750,
           label = "Catastrophic (~7%)", size = 3) +
  annotate("segment", x = 6000 - icl_amt, xend = 6500 - icl_amt,
           y = 3100, yend = 4000,
           arrow = arrow(angle = 15, length = unit(.15, "inches"))) +
  scale_x_continuous(breaks = c(0 - icl_amt, 2510 - icl_amt, 5726 - icl_amt),
                     labels = c("-2510", "0 (ICL)", "3216")) +
  my_theme_paper +
  theme(plot.margin = unit(c(.1,.5,.1,.1), "cm")) +
  theme(axis.title.y = element_text(margin = ggplot2::margin(t = 0, r = 10,
                                                             b = 0, l = 0))) +
  theme(plot.title = element_text(margin = ggplot2::margin(t = 0, r = 0,
                                                           b = 20, l = 0)),
        legend.title = element_blank(),
        legend.position = "none")

# Export -----------------------------------------------------------------------

ggsave(paste0(lib_base, "plots/02a_part_d_design.png"), p_design,
       width = 6, height = 3)

p_design

end_log_file()
