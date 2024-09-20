# Load required packages
library(terra)
library(sf)
library(dplyr)

# Load the DEM
dem <- rast("C:/data/merged_snake_dem.tif")

# Load the shapefile containing snake points
snake_points <- st_read("C:/data/Crotalus_stephensi.shp")

# Check CRS of the DEM
dem_crs <- crs(dem)
print(dem_crs)

# Check CRS of the shapefile points
snake_crs <- st_crs(snake_points)
print(snake_crs)

# Reproject shapefile if necessary
if (snake_crs != dem_crs) {
  snake_points <- st_transform(snake_points, crs(dem))
}

# Extract elevation values at the locations of the points
elevation_values <- extract(dem, st_coordinates(snake_points))

# Check the structure of elevation_values
str(elevation_values)

# Convert elevation_values to a numeric vector, handling any nested structure
if (is.list(elevation_values)) {
  elevation_values <- unlist(elevation_values)  # Flatten the list
}
elevation_values <- as.numeric(elevation_values)  # Ensure it's numeric

# Handle missing values by replacing NA with a default value or removing
elevation_values[is.na(elevation_values)] <- NA  # Set NA values if they exist

# Add elevation values to the original points data and convert to feet
snake_points <- snake_points %>%
  mutate(elv_meter = elevation_values) %>%
  mutate(elv_feet = elv_meter * 3.28084)

# Check the first few rows to verify
print(head(snake_points))

# Save the updated shapefile
st_write(snake_points, "C:/data/Updated_Crotalus_stephensi_with_elevation.shp")
