# Load libraries
library(mapgl)
library(tidyverse)
library(tidycensus)
library(sf)
library(leaflet)

# Fetch population data for California counties
ca_population <- get_acs(
  geography = "county",
  variables = "B01003_001",  # Total population variable
  state = "CA",
  year = 2020,
  geometry = TRUE
) %>%
  rename(population = estimate, county_name = NAME)

# Reproject to WGS84
ca_population <- st_transform(ca_population, crs = 4326)


# Create the map
leaflet(ca_population) %>%
  addTiles() %>%
  addPolygons(
    fillColor = "lightblue",
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~paste(county_name, "Population:", format(population, big.mark = ",")),
    labelOptions = labelOptions(
      textsize = "15px",
      direction = "auto"
    )
  )
