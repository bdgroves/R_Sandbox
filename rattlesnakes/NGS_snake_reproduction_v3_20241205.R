# Load required libraries
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(MASS)
library(dplyr)
library(viridis)

# Load your rattlesnake points data
points <- st_read("C:/data/R_Projects/R_Sandbox/rattlesnakes/rattlesnakes_data_clean.csv")  # Adjust path as needed

# Convert longitude and latitude to numeric
points$longitude <- as.numeric(points$longitude)
points$latitude <- as.numeric(points$latitude)

# Convert to an sf object
points_sf <- st_as_sf(points, coords = c("longitude", "latitude"), crs = 4326)  # WGS84 CRS

# Load the base map for North America
world <- ne_countries(scale = "medium", continent = "North America", returnclass = "sf")

# Filter for USA and Mexico only
world_filtered <- world %>% filter(admin %in% c("United States of America", "Mexico"))

# Reproject to North America Albers Equal Area (EPSG:5070)
world_projected <- st_transform(world_filtered, crs = 5070)
points_projected <- st_transform(points_sf, crs = 5070)

# Extract coordinates for KDE
coords <- st_coordinates(points_projected)

# Define bounding box limits based on your data
x_range <- range(coords[, 1])  # Longitude
y_range <- range(coords[, 2])  # Latitude

# Perform Kernel Density Estimation (KDE) within the bounding box
kde <- kde2d(coords[, 1], coords[, 2], n = 100, lims = c(x_range[1], x_range[2], y_range[1], y_range[2]))

# Convert KDE results into a data frame
kde_df <- data.frame(
  x = rep(kde$x, each = length(kde$y)),
  y = rep(kde$y, length(kde$x)),
  density = as.vector(kde$z)
)

# Create a contour (range) based on KDE density bins (8 bins)
# To avoid duplicate breaks, we calculate the breaks based on unique values
breaks <- unique(quantile(kde_df$density, probs = seq(0, 1, length.out = 9)))  

# If there are still non-unique breaks, we use a small perturbation to make them unique
if(length(breaks) < 9) {
  while(length(breaks) < 9) {
    breaks <- unique(quantile(kde_df$density, probs = seq(0, 1, length.out = 9)))
  }
}

# Create bins for density categories
kde_df$bin <- cut(kde_df$density, breaks = breaks, include.lowest = TRUE, labels = FALSE)

# Convert to sf object for polygon mapping
kde_sf <- st_as_sf(kde_df, coords = c("x", "y"), crs = 5070)

# Plot the map with binned polygons based on snake density
rattlesnake_map <- ggplot() +
  # Add the base map for USA and Mexico
  geom_sf(data = world_projected, fill = "white", color = "gray80") +
  
  # Add the KDE density contour polygons with color bins
  geom_sf(data = kde_sf, aes(fill = factor(bin)), color = NA) +
  scale_fill_viridis_d(name = "Density", option = "D") +
  
  # Add borders for USA and Mexico
  geom_sf(data = world_projected, fill = NA, color = "black", size = 0.4) +
  
  # Add points for rattlesnake observations
  geom_sf(data = points_projected, color = "black", size = 0.2, alpha = 0.5) +
  
  # Zoom in on the USA and Mexico
  coord_sf(xlim = c(x_range[1], x_range[2]), ylim = c(y_range[1], y_range[2])) +
  
  # Customize appearance
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid = element_blank()
  ) +
  labs(
    title = "Range of the Rattler",
    subtitle = "Observed rattlesnake locations in the USA and Mexico",
    fill = "Density (Binned)"
  )

# Save the map as a PNG file
ggsave("rattlesnake_map_range_binned.png", plot = rattlesnake_map, width = 12, height = 8, dpi = 300)
