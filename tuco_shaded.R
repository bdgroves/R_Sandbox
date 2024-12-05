# Load required libraries
library(sf)          # For spatial data manipulation
library(tigris)      # For US shapefiles
library(elevatr)     # To fetch elevation data
library(terra)       # For raster operations
library(rayshader)   # To create shaded relief maps
library(magick)      # For image enhancements
library(dplyr)       # For data manipulation
library(png)         # For saving PNG files

# Enable caching for faster processing
options(tigris_use_cache = TRUE)

# Step 1: Get Tuolumne County shapefile and set projection
tuolumne_shape <- counties(cb = TRUE, state = "CA") |>  # Get California counties
  st_as_sf() |> 
  filter(NAME == "Tuolumne") |> 
  st_transform(crs = "+proj=lcc +lat_1=33 +lat_2=45 +lon_0=-120 +datum=WGS84 +units=m +no_defs")

# Step 2: Fetch elevation data for Tuolumne County
dem <- get_elev_raster(
  locations = tuolumne_shape,
  prj = st_crs(tuolumne_shape)$proj4string,
  z = 10,  # Higher zoom level for better resolution
  src = "aws"
)

# Step 3: Mask and crop DEM to Tuolumne's boundary
dem <- rast(dem) |> 
  mask(tuolumne_shape) |> 
  crop(tuolumne_shape)

# Step 4: Convert DEM to matrix for rayshader
dem_mat <- raster_to_matrix(dem)

# Step 5: Define custom color palette
my_pal <- grDevices::colorRampPalette(c(
  "#026449", "#12722c", "#d7d17e", 
  "#95400d", "#980802", "#746c69", 
  "#f1f1f1", "#fdfdfd"), 
  interpolate = "spline", 
  bias = 1)(256)

# Step 6: Create shaded relief map
im <- dem_mat |>
  sphere_shade(sunangle = 315, texture = "bw", zscale = 20, colorintensity = 0.9) |>
  add_overlay(height_shade(dem_mat, texture = my_pal), alphalayer = 0.7) |>
  add_shadow(ray_shade(dem_mat, sunaltitude = 30, zscale = 20), max_darken = 0.9)

# Step 7: Save initial shaded relief map
writePNG(im, target = "Tuolumne_ShadedRelief.png")

# Step 8: Enhance the map using magick
im1 <- image_read("Tuolumne_ShadedRelief.png") |> 
  image_trim() |> 
  image_modulate(brightness = 100, saturation = 120, hue = 100) |> 
  image_contrast(sharpen = 2)

# Step 9: Add title and labels to the map
im2 <- im1 |> 
  image_annotate("Tuolumne County", font = "sans", color = "#01611F", weight = 700, 
                 size = 140, gravity = "southwest", location = "+200+200") |>
  image_annotate("Shaded Relief", font = "sans", color = "#01611F", weight = 700, 
                 size = 80, gravity = "southwest", location = "+190+130")

# Step 10: Save the final enhanced map
image_write(im2, path = "Tuolumne_ShadedRelief_Final.png", format = "png", quality = 95)
