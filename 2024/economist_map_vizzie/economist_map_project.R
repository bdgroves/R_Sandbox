# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(scales)
library(lubridate)
library(readxl)

# Load the data
data <- read_excel("C:/data/R_Projects/R_Sandbox/2024/economist_map_vizzie/us-states-grid-number.xlsx")

# Create the imp_data_df dataframe with simulated data
imp_data_df <- data.frame(
  state = rep(state.abb, 5),
  stat_date = rep(seq.Date(from = as.Date("2020-02-01"), to = as.Date("2020-06-01"), by = "month"), each = 50),
  stat = rnorm(250, mean = 1000, sd = 150)
) %>%
  mutate(stat = ifelse(stat > 1000 | stat < 0, sample(runif(100), n(), replace = TRUE) * 100, stat))

# Create the decision_dates_df dataframe with sample data
decision_dates_df <- data.frame(
  state = state.abb,
  start_dt = sample(x = as.Date("2020-02-01") + months(0:2), size = 50, replace = TRUE)
) %>%
  mutate(
    end_dt = start_dt + months(sample(2:4, size = 1)),
    end_dt = if_else(end_dt > as.Date("2020-06-01"), as.Date("2020-06-01"), end_dt),
    easing_dt = start_dt + days(sample(20:40, size = 1, replace = TRUE))
  )

# Create the plots_tbl dataframe with sample data
plots_tbl <- tibble(
  state = state.abb,
  highlight_color = sample(c("#cd6b61", "#578ca4"), size = 50, replace = TRUE)
)

# Merge plots_tbl with data
plots_tbl <- left_join(plots_tbl, data, by = "state")

# Function to create range bar plot
rangebar_plot <- function(df, xaxis_st_dt = as.Date("2020-02-01"), xaxis_end_dt = as.Date("2020-06-01")) {
  ggplot(df, aes(x = easing_dt, y = 1)) +
    geom_point(size = 0.1) +
    geom_segment(aes(x = start_dt, xend = end_dt, y = 1, yend = 1), linewidth = 0.1) +
    scale_x_date(limits = c(xaxis_st_dt, xaxis_end_dt), date_breaks = "1 month") +
    scale_y_continuous(limits = c(0.98, 1.02), expand = c(0, 0)) +
    annotate("segment", x = df$start_dt, xend = df$start_dt, y = 0.99, yend = 1.01, size = 0.1) +
    annotate("segment", x = df$end_dt, xend = df$end_dt, y = 0.99, yend = 1.01, size = 0.1) +
    theme_void()
}

# Function to create state plot
state_plot <- function(df, highlight_color = "blue", xaxis_st_dt = as.Date("2020-02-01"), xaxis_end_dt = as.Date("2020-06-01")) {
  ggplot(df, aes(x = stat_date, y = stat)) +
    geom_area(fill = "#559ab7") +
    scale_y_continuous(limits = c(0, 1100), expand = c(0, 0)) +
    scale_x_date(limits = c(xaxis_st_dt, xaxis_end_dt), date_breaks = "1 month") +
    annotate("text", x = as.Date("2020-02-01"), y = 1070, label = unique(df$state), size = 1.5, color = highlight_color, hjust = 0, vjust = 1, fontface = "bold") +
    geom_hline(yintercept = Inf, size = 0.3, color = highlight_color) +
    theme(
      axis.ticks.length.x = unit(1.3, "points"),
      axis.ticks.x = element_line(color = "#8aa6b6", size = 0.2),
      axis.line.x = element_line(color = "#8aa6b6", size = 0.2),
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.background = element_rect(fill = "#d5e4eb", linetype = 0),
      panel.border = element_blank(),
      plot.background = element_rect(fill = "#d5e4eb", color = NA),
      panel.grid = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    geom_hline(yintercept = 500, size = 0.1, color = "white")
}

# Combine state_plot and rangebar_plot for each state in plots_tbl
plots_tbl <- plots_tbl %>%
  mutate(plots = map2(state, highlight_color, function(x, y) { 
    st_plt <- state_plot(filter(imp_data_df, state == x), highlight_color = y)
    rng_plt <- rangebar_plot(filter(decision_dates_df, state == x))
    cowplot::plot_grid(st_plt, rng_plt, nrow = 2, align = "v", axis = "t", rel_heights = c(5, 1))
  }))

# Display head of plots_tbl
head(plots_tbl)

# Create example lp plot
lp <- ggplot(filter(imp_data_df, state == "CA"), aes(x = stat_date, y = stat)) +
  geom_area(fill = "#559ab7") +
  scale_y_continuous(limits = c(0, 1100), expand = c(0, 0), labels = comma, position = "right") +
  scale_x_date(breaks = as.Date("2020-06-01") - months(0:4), labels = rev(c("F", "M", "A", "M", "Jun")), expand = c(0, 0)) +
  geom_hline(yintercept = Inf, size = 0.3, color = '#578ca4') +
  theme(
    axis.ticks.length.x = unit(1.5, "points"),
    axis.ticks.x = element_line(color = "#8aa6b6", size = 0.2),
    axis.line.x = element_line(color = "#8aa6b6", size = 0.2),
    axis.text = element_text(color = "black", size = 2.2),
    axis.title = element_blank(),
    panel.background = element_rect(fill = "#d5e4eb", linetype = 0),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "#d5e4eb", colour = NA),
    panel.grid = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  geom_hline(yintercept = 500, size = 0.1, color = "white")

# Display lp plot
lp

# Create example pizza preference plot function
pizza_pref <- data.frame(
  y = 1,
  x = 0,
  label = c("Thin", "Deep"),
  color = c('#cd6b61', '#578ca4'),
  stringsAsFactors = FALSE
)

pizza_pref_plot_fn <- function(what_type) {
  df <- filter(pizza_pref, label == what_type)
  label_color <- df$color
  
  ggplot(df, aes(x = x, y = y, label = label, color = label)) +
    geom_segment(aes(xend = 1, yend = y), size = 0.3) +
    geom_text(size = 1.5, color = label_color, hjust = 0, vjust = 1.5, fontface = "bold") +
    scale_color_manual(values = label_color) +
    theme_void() +
    theme(legend.position = "none")
}

# Display pizza preference plot for "Deep"
pizza_pref_plot_fn("Deep")

# Create range bar legend plot
wyoming_decision_dates <- filter(decision_dates_df, state == 'WY')
range_bar_legend_plt <- rangebar_plot(wyoming_decision_dates) +
  scale_x_date(limits = c(as.Date("2020-02-01"), as.Date("2020-07-01")), date_breaks = "1 month") + 
  annotate("text", x = wyoming_decision_dates$start_dt, label = "Start Date", y = 1, hjust = 1, size = 1) + 
  annotate("text", x = wyoming_decision_dates$end_dt, label = "End Date", y = 1, hjust = 0, size = 1) + 
  annotate("text", x = wyoming_decision_dates$easing_dt, label = "Easing starts", y = 1, vjust = 1.2, size = 1)

# Create a list of plots and assemble final plot
my_list <- rep(NA, 88)
my_list[plots_tbl$boxnumber] <- plots_tbl$plots
my_list[[1]] <- lp
my_list[[3]] <- pizza_pref_plot_fn(what_type = "Thin")
my_list[[4]] <- pizza_pref_plot_fn(what_type = "Deep")

# Combine the plots into a grid
gridded_plots <- cowplot::plot_grid(plotlist = my_list, nrow = 8, ncol = 11)

# Define title position
title_pos <- 0.01

# Create the final plot
final_plot <- cowplot::ggdraw(gridded_plots, ylim = c(-0.05, 1.1)) +
  cowplot::draw_plot(range_bar_legend_plt, x = .35, y = .915, width = .4, height = 0.04) +
  cowplot::draw_label("Important dates to remember", x = .48, y = 0.95, hjust = 0, vjust = 0, size = 4) +
  cowplot::draw_label("States of play", x = title_pos, y = 1.1, hjust = 0, vjust = 1, size = 8, fontface = "bold") +
  cowplot::draw_label("Random data. Change people's opinion using this chart", x = title_pos, y = 1.065, hjust = 0, vjust = 1, size = 5.5) +
  cowplot::draw_label("https://www.nandeshwar.info. Generated using R", x = title_pos, y = -0.05, hjust = 0, vjust = -1, size = 4) +
  cowplot::draw_label("Pizza preference surveyed in 2019", x = .186, y = 0.95, hjust = 0, vjust = 0, size = 4)

# Save the final plot
ggsave(
  plot = final_plot,
  filename = "my_us_map_plot.png",
  width = 6,
  height = 4, 
  bg = "#E5EBF0" # changing the whole background color
)
