# Load required packages
library(terra)
library(sf)
library(dplyr)

# Load the shapefile containing snake points
snake_points <- st_read("C:/data/Crotalus_stephensi.shp")

# Set seed for reproducibility
set.seed(123)

# Create proxy columns
snake_points <- snake_points %>%
  mutate(
    size_category = sample(c("Small", "Medium", "Large"), n(), replace = TRUE),
    color_pattern = sample(c("Straw", "Tan", "Buff", "Brown", "Gray"), n(), replace = TRUE),
    elevation = sample(900:2400, n(), replace = TRUE),
    diet = sample(c("Mammals", "Lizards", "Birds"), n(), replace = TRUE),
    birth_month = ifelse(runif(n()) < 0.3, sample(month.name, n(), replace = TRUE), NA)
  )
