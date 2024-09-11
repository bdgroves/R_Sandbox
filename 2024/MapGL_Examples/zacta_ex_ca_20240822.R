# Load necessary libraries
library(tidycensus)  # For retrieving ACS data
library(tigris)      # For retrieving geographical boundaries
library(sf)          # For handling spatial data
library(mapgl)       # For visualizing data on Mapbox GL maps
library(glue)        # For creating dynamic text strings
library(dplyr)       # For data manipulation

# Step 1: Retrieve California state geometry
# We use the `states()` function from the tigris package to get state boundaries
# and filter to keep only California.
ca_geometry <- states(cb = TRUE) %>% 
  filter(NAME == "California")

# Step 2: Retrieve ZCTA (ZIP Code Tabulation Area) data with geometry for California
# We use the `get_acs()` function from tidycensus to retrieve the total population 
# data (variable B01003_001) for each ZCTA in 2021, including the geometry data.
zcta_data <- get_acs(
  geography = "zcta",
  variables = "B01003_001",  # Total population variable
  year = 2021,
  geometry = TRUE            # Include spatial data
)

# Step 3: Filter ZCTAs that intersect with California
# The `st_filter()` function from sf is used here to perform a spatial filter, 
# keeping only ZCTAs that intersect with the California state geometry.
zcta_data_ca <- zcta_data %>%
  st_filter(ca_geometry, .predicate = st_intersects)

# Step 4: Create popups with GEOID and population estimate
# We use the `glue()` function to create a string for each ZCTA that includes the 
# GEOID and the total population estimate. This string will be used for popups on the map.
zcta_data_ca <- zcta_data_ca %>%
  mutate(popup = glue("<strong>GEOID: </strong>{GEOID}<br><strong>Total Population: </strong>{estimate}"))

# Step 5: Initialize the map with California bounds
# The map is initialized using the `mapbox_style()` and `mapboxgl()` functions from the 
# mapgl package, setting the bounds based on the filtered ZCTA data for California.
ca_map <- mapboxgl(mapbox_style("light"), bounds = zcta_data_ca)

# Step 6: Add a fill layer to the map with popups and tooltips
# We add a fill layer to the map using `add_fill_layer()`. The fill color is interpolated 
# based on the population estimate. Popups and tooltips are configured to show the population estimate.
ca_map <- ca_map |> 
  add_fill_layer(
    id = "ca_zctas",
    source = zcta_data_ca,
    fill_color = interpolate(
      column = "estimate",
      values = c(0, 10000, 50000, 100000),
      stops = c("lightblue", "blue", "darkblue", "purple"),
      na_color = "lightgrey"   # Color for missing data
    ),
    fill_opacity = 0.6,        # Set fill opacity
    popup = "popup",           # Display popup with GEOID and population
    tooltip = "estimate",      # Tooltip showing population estimate
    hover_options = list(      # Options when hovering over a ZCTA
      fill_color = "yellow",   # Change fill color on hover
      fill_opacity = 0.8       # Change fill opacity on hover
    )
  )

ca_map

ca_map <- mapboxgl(mapbox_style("dark"), bounds = zcta_data_ca)
ca_map <- mapboxgl(mapbox_style("streets"), bounds = zcta_data_ca)
ca_map <- mapboxgl(mapbox_style("outdoors"), bounds = zcta_data_ca)
ca_map <- mapboxgl(mapbox_style("satellite"), bounds = zcta_data_ca)






