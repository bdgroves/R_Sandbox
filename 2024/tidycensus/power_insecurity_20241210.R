# Load required libraries
library(tidycensus)  # For fetching ACS data
library(sf)          # For spatial data processing
library(tidyverse)   # For data wrangling and visualization
library(janitor)     # For cleaning column names
library(tigris)      # For cartographic shapefiles
library(scico)       # For color palettes
library(showtext)    # For custom fonts

# Load ACS 1-year estimates variable list
acs_vars <- load_variables(2023, "acs1", cache = TRUE)

# Define heating source variables from ACS
heating_vars <- c(
  "B25040_002" = "utility_gas",
  "B25040_003" = "electricity",
  "B25040_004" = "fuel_oil",
  "B25040_005" = "wood",
  "B25040_006" = "solar",
  "B25040_007" = "other_fuel",
  "B25040_008" = "no_fuel"
)

# Define a function to fetch heating data
get_heating_data <- function(year, states) {
  get_acs(
    geography = "county",
    variables = names(heating_vars),
    state = states,
    year = year,
    geometry = TRUE,
    survey = "acs1"
  ) %>%
    clean_names() %>%
    mutate(variable = recode(variable, !!!heating_vars)) %>%
    pivot_wider(names_from = variable, values_from = estimate) %>%
    rowwise() %>%
    mutate(
      total_homes = sum(c_across(c(utility_gas, electricity, fuel_oil, wood, solar, other_fuel, no_fuel)), na.rm = TRUE),
      percent_no_fuel = (no_fuel / total_homes) * 100,
      percent_electricity = (electricity / total_homes) * 100,
      percent_utility_gas = (utility_gas / total_homes) * 100,
      percent_fuel_oil = (fuel_oil / total_homes) * 100,
      percent_wood = (wood / total_homes) * 100
    ) %>%
    ungroup()
}

# Define list of states to analyze
states <- c("WA", "OR", "CA", "NV", "AZ", "TX", "NY", "FL")

# Fetch heating data for 2023
heating_data_2023 <- get_heating_data(2023, states)

# Fetch shapefiles for counties and states
county_sf <- counties(cb = TRUE, state = states) %>%
  st_transform("EPSG:5070") %>%
  clean_names()

state_sf <- states(cb = TRUE) %>%
  st_transform("EPSG:5070") %>%
  filter(STUSPS %in% states)  # Ensure the correct column is used for state abbreviations

# Convert `county_sf` to a standard data frame with relevant columns
county_data <- county_sf %>%
  st_set_geometry(NULL) %>%  # Remove geometry for a non-spatial join
  select(geoid, name, statefp, countyfp)  # Adjust columns as needed

# Perform the join
heating_sf <- heating_data_2023 %>%
  left_join(county_data, by = "geoid")



# Define custom font for plots
font_legend <- "Source Sans Pro"
font_add_google(font_legend)
showtext_auto()

map_heating <- function(data, variable, title) {
  ggplot(data) +
    geom_sf(aes(fill = !!sym(variable)), color = "white", linewidth = 0.05) +
    geom_sf(data = state_sf, fill = NA, color = "grey50", linewidth = 0.1) +
    scale_fill_scico(
      name = title,
      palette = "bamako",
      direction = -1,
      labels = scales::percent_format(scale = 1)  # Ensure scale is correct for percentages
    ) +
    theme_void() +
    theme(
      text = element_text(family = font_legend, size = 14),
      legend.position = "bottom",
      legend.title = element_text(hjust = 0.5)  # Replacing legend.title.align
    )
}


# Map showing the percentage of homes using electricity for heating
electricity_map <- map_heating(heating_sf, "percent_electricity", "Percent Homes Heating with Electricity")

# Display the map
print(electricity_map)
