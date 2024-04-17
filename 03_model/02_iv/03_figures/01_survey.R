# Proj: Cost-Sharing
# Author: Evan Flack (flack@stanford.edu)
# Desc: Makes figure 1 (survey)

# Start Script -----------------------------------------------------------------
library(ggplot2)
source("../../../00_pre_process/start_script.R")
source("../../../00_pre_process/define_plot_theme.R")

start_log_file("log/01_survey.R")

# Read in Data -----------------------------------------------------------------

# Survey data
dt_all <- fread(paste0(lib_base_data, "survey/cfo_survey_full.csv"))

# Prep Data --------------------------------------------------------------------

# Create indicators for "most severe" answer to Q5, eliminate people that
# responded "no change"
dt_all %<>%
  .[, death := ifelse(Q5r4 == 1, 1, 0)] %>%
  .[, hosp_no_death := ifelse(Q5r3 == 1 & Q5r4 == 0, 1, 0)] %>%
  .[, chron := ifelse(Q5r2 == 1 & Q5r3 == 0 & Q5r4 == 0, 1, 0)] %>%
  .[, feel := ifelse(Q5r1 == 1 & Q5r2 == 0 & Q5r3 == 0 & Q5r4 == 0, 1, 0)] %>%
  .[, none := ifelse(Q5r99 == 1, 1, 0)]

dt <- dt_all %>%
  .[none == 0]

# Percent saying no effect within a week
message(round((1 - mean(dt$Q4New == 1)) * 100, 1), " % saying no effct in a week")

message(round((mean(dt$Q5r4 == 1 | dt$Q5r3 == 1)) * 100, 1),
        " % saying hospitalization or death")

message(round(mean(dt$death == 1) * 100, 1),
        " % saying death")

# Make Area Plot ---------------------------------------------------------------

dtp <- data.table()
for (i in c(1, 2, 3, 4, 5)) {
  # Count number of people with a certain time frame that also answered a
  # certain event
  dtp1 <- dt[Q4New == i] %>%
    .[, lapply(.SD, sum),
      .SDcols = c("death", "hosp_no_death", "chron", "feel")] %>%
    melt(measure.vars = names(.)) %>%
    .[, Q4New := i]
  dtp %<>% rbind(dtp1)
}

# Calculate cumulative counts/percentages over time frames, rename labels
dtp %<>%
  setnames("Q4New", "time") %>%
  .[order(variable, time)] %>%
  .[, cum_n := ave(value, variable, FUN = cumsum)] %>%
  .[, cum_perc := (cum_n / nrow(dt)) * 100] %>%
  .[, variable := factor(variable, levels = c("feel", "chron",
                                              "hosp_no_death", "death"),
                         labels = c("Feel Worse on Days Missed",
                                    "Worsening Chronic Conditions",
                                    "Hospitalization", "Death"))]

# Plot
p_survey <- ggplot(dtp) +
  aes(x = time, y = cum_perc, fill = factor(variable)) +
  geom_area() +
  my_theme_paper +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = c("lemonchiffon3",  "lightblue3", "dodgerblue4",
                               "red3")) +
  scale_x_continuous(breaks = seq(1, 5),
                     labels = c("< 1 week", "1-2 wks", "2-3 wks",
                                "3-4 wks", "> 4 wks")) +
  labs(x = "Time to Event", y = "Cumulative Percentage of Respondents") +
  annotate(geom = "text", y = 7, x = 3, label = "Death", color = "white") +
  annotate(geom = "text", y = 20, x = 3, label = "Hospitalization",
           color = "white") +
  annotate(geom = "text", y = 50, x = 3, label = "Worsened Chronic Conditions",
           color = "black") +
  annotate(geom = "text", y = 78, x = 3, label = "Feel Worse on Days Missed",
           color = "black", angle = 8) +
  theme(legend.position = "none")

# Export -----------------------------------------------------------------------

ggsave(paste0(lib_base, "plots/01_survey_figure.png"), p_survey,
       width = 6, height = 3)

end_log_file()
