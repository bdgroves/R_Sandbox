knitr::opts_chunk$set(echo = TRUE)

# Load libraries
library(tidyverse)     # Data manipulation and visualization
library(dataRetrieval) # Pull gage data
library(ggridges)      # Create ridge plots
library(grid)          # Plot layout
library(cowplot)       # Layout final plot
library(sysfonts)      # Text editing
library(showtext)      # Add Google fonts
library(magick)        # Add logo

# Set timeframe to plot (past 12 months)
end_date <- Sys.Date()
start_date <- as.Date(end_date) - 365

# Select gage of interest (Rio Grande at Embudo, NM)
site_no <- "08279500"

# Pull NWIS data
stream_data_raw <- readNWISuv(
  siteNumbers = site_no, 
  parameterCd = c("00060", "00065"),
  startDate = start_date, 
  endDate = end_date
) |> renameNWISColumns()

# Prepare data for plotting
stream_data <- stream_data_raw %>%
  filter(dateTime >= as.POSIXct(start_date), dateTime <= as.POSIXct(end_date)) %>%
  mutate(
    julian_day = yday(dateTime),
    year = as.numeric(format(dateTime, "%Y")),
    day = as.numeric(format(dateTime, "%d")),
    month = as.numeric(format(dateTime, "%m"))
  ) %>%
  group_by(site_no, year, julian_day) %>%
  summarise(mean_daily_flow_cfs = mean(Flow_Inst, na.rm = TRUE)) %>%
  filter(!is.na(mean_daily_flow_cfs))

# Load custom fonts and settings
font_legend <- 'Questrial'
font_add_google(font_legend)
showtext_opts(dpi = 300, regular.wt = 300, bold.wt = 800)
showtext_auto(enable = TRUE)

# Create main plot
main_plot <- ggplot(stream_data, aes(x = julian_day, y = year, group = year, height = mean_daily_flow_cfs)) +
  geom_density_ridges(stat = "identity", scale = 6, fill = "black", color = 'white', alpha = 1) +
  coord_cartesian(clip = "off") +
  geom_text(x = 183, y = max(stream_data$year) - 1, label = "STREAMFLOW", color = "white", size = 10, hjust = 0.5, family = font_legend) +
  scale_x_continuous(name = NULL) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "black"),
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    plot.background = element_rect(fill = "black", color = NA),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    text = element_text(size = 26, family = font_legend, color = "white"),
    plot.title = element_text(hjust = 0.5, vjust = -0.5),
    plot.margin = unit(c(1, 1, 2, 1), "lines")
  ) +
  labs(title = "RIO GRANDE RIVER, NEW MEXICO")

# Define colors
font_color <- "#ffffff"
background_color <- "#000000"

# Background canvas
canvas <- grid::rectGrob(
  x = 0, y = 0,
  width = 16, height = 9,
  gp = grid::gpar(fill = background_color, alpha = 1, col = background_color)
)

# Load USGS logo
usgs_logo <- magick::image_read("c:/data/R_Projects/R_Sandbox/2024/Ridgeline_Plots/usgs-1-logo-black-and-white.png")  # Ensure this file is in your working directory

# Draw final plot
ggdraw(ylim = c(0, 1), xlim = c(0, 1)) +
  draw_grob(canvas, x = 0, y = 1, height = 8, width = 8, hjust = 0, vjust = 1) +
  draw_plot(main_plot, x = -0.001, y = 0.01, height = 1) +
  draw_label(
    "Brooks Groves, \nData: NWIS",
    fontfamily = font_legend,
    x = 0.933, y = 0.039,
    size = 10,
    hjust = 1, vjust = 0,
    color = font_color
  ) +
  draw_image(usgs_logo, x = 0.070, y = 0.034, width = 0.1, hjust = 0, vjust = 0, halign = 0, valign = 0)

# Save final image in Twitter's 16 by 9 format
ggsave(
  filename = paste0("out/", format(Sys.Date(), "%Y%m%d"), "_pop-culture_brooks.png"),
  width = 8, height = 8, dpi = 300,
  bg = "black"
)
