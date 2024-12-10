# Install required packages
install.packages(c(
  'tidycensus', 'tidyverse', 'sf', 'janitor', 'tigris', 
  'rmapshaper', 'showtext', 'scico', 'ggalt', 'plotly', 'scales'
))

# Load required libraries
library(tidycensus)  # Processing and visualizing Census data
library(sf)          # Spatial data processing
library(janitor)     # Cleaning column names
library(tidyverse)   # Dataframe processing
library(tigris)      # Cartographic boundary shapefiles
library(rmapshaper)  # Simplifying spatial objects
library(showtext)    # Custom fonts
library(scico)       # Color palettes
library(ggalt)       # Dumbbell charts

# Load ACS 1-year estimates variable list for 2023
acs_vars <- load_variables(2023, "acs1", cache = TRUE)

# Define a function to fetch and process Census data
get_census_data <- function(geography, var_names, states, year, proj, survey_var) {
  df <- get_acs(
    geography = geography,
    variable = var_names,
    state = states,
    year = year,
    geometry = TRUE,
    survey = survey_var
  ) |> 
    clean_names() |> 
    st_transform(proj)
  return(df)
}

# Define list of states to filter data
western_states <- c(
  'Washington', 'Oregon', 'California', 'Idaho', 'Nevada',
  'Utah', 'Arizona', 'Montana', 'Wyoming', 'Colorado',
  'New Mexico', 'North Dakota', 'South Dakota', 'Nebraska', 'Kansas',
  'Oklahoma', 'Texas', 'Minnesota', 'Iowa', 'Missouri',
  'Arkansas', 'Louisiana'
)

# Define Census variable codes for housing insecurity (severe cost burden)
vars <- c("B01003_001", "B25094_001")  # Total population and severe housing cost burden households

# Fetch Census data for 2023 and 2022
housing_data_2023 <- get_census_data(
  geography = 'county', var_names = vars, states = western_states,
  year = 2023, proj = "EPSG:5070", survey_var = "acs1"
)

housing_data_2022 <- get_census_data(
  geography = 'county', var_names = vars, states = western_states,
  year = 2022, proj = "EPSG:5070", survey_var = "acs1"
)

# Process the data to calculate percentages
process_housing_data <- function(data) {
  data |> 
    mutate(variable_long = case_when(
      variable == "B01003_001" ~ "total_pop",
      variable == "B25094_001" ~ "housing_cost_burden",
      .default = NA_character_
    )) |> 
    select(geoid, name, variable_long, estimate, geometry) |> 
    pivot_wider(names_from = variable_long, values_from = estimate) |> 
    mutate(percent_housing_burden = (housing_cost_burden / total_pop) * 100)
}

housing_data_2023_wide <- process_housing_data(housing_data_2023)
housing_data_2022_wide <- process_housing_data(housing_data_2022)

# Process spatial data for mapping
western_sf <- states(cb = TRUE) |> 
  st_transform("EPSG:5070") |> 
  clean_names() |> 
  filter(name %in% western_states) |> 
  ms_simplify(keep = 0.2)

western_counties_sf <- counties(cb = TRUE) |> 
  st_transform("EPSG:5070") |> 
  clean_names() |> 
  filter(state_name %in% western_states) |> 
  ms_simplify(keep = 0.2)

# Add custom font
font_legend <- "Source Sans Pro"
font_add_google(font_legend)
showtext_opts(dpi = 300, regular.wt = 200, bold.wt = 700)
showtext_auto(enable = TRUE)

# Function to create maps for housing insecurity (housing cost burden)
map_change_housing_insecurity <- function(data, year) {
  data |> 
    ggplot(aes(fill = percent_housing_burden)) +
    geom_sf(color = "white", linewidth = 0.05) +
    geom_sf(data = western_sf, fill = NA, color = "white", linewidth = 0.1) +
    geom_sf(data = western_counties_sf, fill = NA, color = "grey80", linewidth = 0.09) +
    theme_void() +
    theme(
      text = element_text(family = font_legend, size = 16),
      legend.margin = margin(t = 5, b = 2),
      legend.position = 'bottom',
      legend.title.align = 0.5
    ) +
    guides(fill = guide_colorbar(
      title.position = "top",
      title.theme = element_text(face = 'bold', family = font_legend, size = 16),
      direction = "horizontal",
      barwidth = 20,
      barheight = 1
    )) +
    scale_fill_scico(
      name = sprintf("Households with severe housing cost burden (%s)", year),
      limits = c(0, 30),
      breaks = c(0, 5, 10, 15, 20, 25, 30),
      palette = "lajolla",
      direction = -1,
      end = 0.95,
      na.value = "white",
      labels = function(x) paste0(x, "%")
    )
}

# View maps for 2023 and 2022
map_change_housing_insecurity(housing_data_2023_wide, 2023)
map_change_housing_insecurity(housing_data_2022_wide, 2022)

# Combine 2023 and 2022 data for further analysis
housing_data_combined <- housing_data_2023_wide |> 
  st_drop_geometry() |> 
  select(name, total_pop_2023 = total_pop, percent_housing_burden_2023 = percent_housing_burden) |> 
  left_join(
    housing_data_2022_wide |> 
      st_drop_geometry() |> 
      select(name, total_pop_2022 = total_pop, percent_housing_burden_2022 = percent_housing_burden),
    by = "name"
  ) |> 
  mutate(
    state = str_extract(name, "(?<=, ).*"),
    county = str_remove(name, ",\\s*[^,]+$")
  )

# Interactive scatter plot for housing cost burden
p_housing <- ggplot(
  housing_data_combined,
  aes(
    x = total_pop_2023,
    y = percent_housing_burden_2023,
    color = factor(year),
    text = glue("County: {name}<br>Total Population: {total_pop_2023}<br>Percent with Severe Housing Cost Burden: {round(percent_housing_burden_2023, 2)}%")
  )
) +
  geom_point(size = 2) + 
  labs(x = "Total Population", y = "Percent with Severe Housing Cost Burden", color = "Year") +
  scale_x_continuous(labels = scales::comma) +
  scale_color_manual(values = c("2022" = "#005AB5", "2023" = "#DC3220")) +
  theme_minimal()

# Render interactive plot
ggplotly(p_housing, tooltip = "text")

# Interactive scatter plot for housing cost burden
p_housing <- ggplot(
  housing_data_combined,
  aes(
    x = total_pop_2023,
    y = percent_housing_burden_2023,
    color = factor("2023"),  # Explicitly set '2023' for color
    text = glue("County: {name}<br>Total Population: {total_pop_2023}<br>Percent with Severe Housing Cost Burden: {round(percent_housing_burden_2023, 2)}%")
  )
) +
  geom_point(size = 2) + 
  labs(x = "Total Population", y = "Percent with Severe Housing Cost Burden", color = "Year") +
  scale_x_continuous(labels = scales::comma) +
  scale_color_manual(values = c("2023" = "#DC3220")) +  # Directly use "2023" to avoid issues with a factor
  theme_minimal()

# Convert the plot to an interactive plot using ggplotly
interactive_plot <- ggplotly(p_housing, tooltip = "text")

# Render interactive plot
interactive_plot

