library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)

# Load your rattlesnake points data
points <- st_read("C:/data/R_Projects/R_Sandbox/rattlesnakes/rattlesnakes_data_clean.csv")  # Adjust based on your file type


# Convert longitude and latitude to numeric
points$longitude <- as.numeric(points$longitude)
points$latitude <- as.numeric(points$latitude)

# Convert to an sf object
points_sf <- st_as_sf(points, coords = c("longitude", "latitude"), crs = 4326)  # WGS84 CRS

# Load the base map for North America
world <- ne_countries(scale = "medium", continent = "North America", returnclass = "sf")

# Reproject to North America Albers Equal Area (EPSG:5070)
world_projected <- st_transform(world, crs = 5070)
points_projected <- st_transform(points_sf, crs = 5070)


library(MASS)

# Extract coordinates from the points
coords <- st_coordinates(points_projected)

# Perform KDE
kde <- kde2d(coords[,1], coords[,2], n = 300)  # Increase `n` for finer resolution

# Convert KDE results to a raster-like data frame for ggplot
kde_df <- data.frame(
  x = rep(kde$x, each = length(kde$y)),
  y = rep(kde$y, length(kde$x)),
  density = as.vector(kde$z)
)

ggplot() +
  # Add the base map
  geom_sf(data = world_projected, fill = "white", color = "gray80") +
  
  # Add the KDE density layer
  geom_raster(data = kde_df, aes(x = x, y = y, fill = density), interpolate = TRUE) +
  scale_fill_gradient(low = "#FBE7C6", high = "#8B4513", name = "Density") +
  
  # Add points for rattlesnake observations
  geom_sf(data = points_projected, color = "black", size = 0.2, alpha = 0.5) +
  
  # Customize appearance
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank()
  ) +
  labs(
    title = "Range of the Rattler",
    subtitle = "Observed rattlesnake locations in North America",
    fill = "Observation Density"
  )

ggsave("rattlesnake_map.png", width = 12, height = 8, dpi = 300)

ggplot() +
  # Add the base map
  geom_sf(data = world_projected, fill = "white", color = "gray80") +
  
  # Add the KDE density layer
  geom_raster(data = kde_df, aes(x = x, y = y, fill = density), interpolate = TRUE) +
  scale_fill_gradient(low = "#FBE7C6", high = "#8B4513", name = "Density") +
  
  # Add points for rattlesnake observations
  geom_sf(data = points_projected, color = "black", size = 0.2, alpha = 0.5) +
  
  # Set the zoomed-in view with coord_sf
  coord_sf(xlim = c(-125, -70), ylim = c(14, 50)) +
  
  # Customize appearance
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank()
  ) +
  labs(
    title = "Range of the Rattler",
    subtitle = "Observed rattlesnake locations in the USA and Mexico",
    fill = "Observation Density"
  )

