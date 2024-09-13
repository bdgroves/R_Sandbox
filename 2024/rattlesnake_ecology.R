# Load necessary libraries
library(sf)         # For handling shapefiles
library(tidyverse)  # For data manipulation
library(ggplot2)    # For plotting
library(raster)     # For spatial analysis
library(vegan)      # For ecological statistics

# Load the shapefiles for Western Diamondback and Mojave rattlesnakes
wd_shapefile <- st_read("C:/data/R_Projects/R_Sandbox/data/Crotalus_atrox.shp")
mojave_shapefile <- st_read("C:/data/R_Projects/R_Sandbox/data/Crotalus_scutulatus.shp")

# Load the point data from CSV (assuming long, lat coordinates)
wd_points <- read.csv("C:/data/R_Projects/R_Sandbox/data/Crotalus_atrox.csv")
mojave_points <- read.csv("C:/data/R_Projects/R_Sandbox/data/Crotalus_scutulatus.csv")

# Convert WKT columns to sf objects
wd_points_sf <- st_as_sf(wd_points, wkt = "WKT", crs = 4326)
mojave_points_sf <- st_as_sf(mojave_points, wkt = "WKT", crs = 4326)

# Check the structure of the sf objects
str(wd_points_sf)
str(mojave_points_sf)

# Plot the shapefiles and point data on the same map
ggplot() +
  geom_sf(data = wd_shapefile, fill = "blue", alpha = 0.3, color = "blue") +
  geom_sf(data = mojave_shapefile, fill = "green", alpha = 0.3, color = "green") +
  geom_sf(data = wd_points_sf, color = "blue", size = 2, alpha = 0.7) +
  geom_sf(data = mojave_points_sf, color = "green", size = 2, alpha = 0.7) +
  labs(title = "Ranges and Observations of Western Diamondback and Mojave Rattlesnakes",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

# Simplify shapefiles to include only geometry
wd_shapefile_simplified <- st_union(st_geometry(wd_shapefile))
mojave_shapefile_simplified <- st_union(st_geometry(mojave_shapefile))

# Convert simplified sf objects to Spatial objects
wd_spatial <- as(wd_shapefile_simplified, "Spatial")
mojave_spatial <- as(mojave_shapefile_simplified, "Spatial")

# Create a raster template with the extent of the shapefiles
r <- raster(extent(wd_spatial), res = 0.01)

# Rasterize the Spatial objects
wd_raster <- rasterize(wd_spatial, r, fun = "count", background = 0)
mojave_raster <- rasterize(mojave_spatial, r, fun = "count", background = 0)

# Calculate overlap between the two rasters
overlap_raster <- overlay(wd_raster, mojave_raster, fun = function(x, y) pmin(x, y))

# Calculate areas
total_area_wd <- cellStats(wd_raster, stat = 'sum')
total_area_mojave <- cellStats(mojave_raster, stat = 'sum')
overlap_area_size <- cellStats(overlap_raster, stat = 'sum')

# Calculate proportions of overlap
overlap_prop_wd <- overlap_area_size / total_area_wd
overlap_prop_mojave <- overlap_area_size / total_area_mojave

# Print overlap proportions
cat("Proportion of Western Diamondback overlap:", overlap_prop_wd, "\n")
cat("Proportion of Mojave Rattlesnake overlap:", overlap_prop_mojave, "\n")

# Convert rasters to frequency tables
wd_table <- as.data.frame(freq(wd_raster))
mojave_table <- as.data.frame(freq(mojave_raster))

# Calculate proportion of each cell type
proportion_wd <- wd_table$freq / sum(wd_table$freq)
proportion_mojave <- mojave_table$freq / sum(mojave_table$freq)

# Calculate Levin's Niche Breadth
levins_wd <- 1 / sum(proportion_wd^2)
levins_mojave <- 1 / sum(proportion_mojave^2)

# Print Levin's Niche Breadth
cat("Levin's Niche Breadth for Western Diamondback:", levins_wd, "\n")
cat("Levin's Niche Breadth for Mojave Rattlesnake:", levins_mojave, "\n")
