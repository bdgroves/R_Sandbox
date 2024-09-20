# Load required libraries
library(ggplot2)
library(sf)       # For handling spatial data
library(dplyr)    # For data wrangling
library(rnaturalearth)  # To get the base map of states and countries
library(rnaturalearthdata)

# Load the shapefile using st_read
snake_range_sf <- st_read("C:/data/snakes_20240912.shp")

# Get the basemap of USA states
states_sf <- ne_states(country = "United States of America", returnclass = "sf")

# Filter the base map to only include California, Arizona, Nevada, and Utah
west_states_sf <- states_sf %>%
  filter(name %in% c("California", "Arizona", "Nevada", "Utah"))

# Get the basemap of countries (for Mexico and Baja California)
mexico_sf <- ne_countries(country = "Mexico", returnclass = "sf")

# Plot the map
ggplot() +
  # Plot the base map of the selected US states
  geom_sf(data = west_states_sf, fill = "lightgray", color = "black", lwd = 0.5) +
  
  # Plot the base map of Mexico (including Baja California)
  geom_sf(data = mexico_sf, fill = "lightgray", color = "black", lwd = 0.5) +
  
  # Plot the snake range as polygons
  geom_sf(data = snake_range_sf, aes(fill = "Snake Range"), color = "blue", alpha = 0.5) +
  
  # Customize the appearance
  labs(
    title = "Snake Species Distribution",
    subtitle = "California, Arizona, Nevada, Utah, Baja California, and Mexico",
    fill = "Legend"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "bottom"
  )
