# Install required packages
#install.packages(c(
# 'tidycensus', 'tidyverse', 'sf', 'janitor', 'tigris', 
#))

# Load required libraries
library(tidycensus)
library(sf)
library(janitor)
library(tidyverse)
library(tigris)
library(rmapshaper)
library(showtext)
library(scico)
library(ggalt)
library(plotly)
library(scales)
library(glue)

# Load ACS 1-year estimates variable list for 2023
acs_vars <- load_variables(2023, "acs1", cache = TRUE)

# Function to fetch and process Census data
get_census_data <- function(geography, var_names, states, year, proj, survey_var) {
  get_acs(
    geography = geography,
    variable = var_names,
    state = states,
    year = year,
    geometry = TRUE,
    survey = survey_var
  ) |> 
    clean_names() |> 
    st_transform(proj)
}

# Define list of western states
western_states <- c(
  'Washington', 'Oregon', 'California', 'Idaho', 'Nevada', 'Utah', 'Arizona',
  'Montana', 'Wyoming', 'Colorado', 'New Mexico', 'North Dakota', 'South Dakota',
  'Nebraska', 'Kansas', 'Oklahoma', 'Texas', 'Minnesota', 'Iowa', 'Missouri',
  'Arkansas', 'Louisiana'
)

# Define Census variable codes
vars <- c("B01003_001", "B25049_004")

# Fetch Census data for 2023 and 2022
western_data_2023 <- get_census_data(
  geography = 'county', var_names = vars, states = western_states,
  year = 2023, proj = "EPSG:5070", survey_var = "acs1"
)

western_data_2022 <- get_census_data(
  geography = 'county', var_names = vars, states = western_states,
  year = 2022, proj = "EPSG:5070", survey_var = "acs1"
)

# Pivot and process Census data to calculate percentages
process_census_data <- function(data) {
  data |> 
    mutate(variable_long = case_when(
      variable == "B01003_001" ~ "total_pop",
      variable == "B25049_004" ~ "plumbing",
      .default = NA_character_
    )) |> 
    select(geoid, name, variable_long, estimate, geometry) |> 
    pivot_wider(names_from = variable_long, values_from = estimate) |> 
    mutate(percent_lacking_plumbing = (plumbing / total_pop) * 100)
}

western_data_2023_wide <- process_census_data(western_data_2023)
western_data_2022_wide <- process_census_data(western_data_2022)

# Simplify and transform spatial data
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

# Function to create maps
map_change_plumbing <- function(data, year) {
  data |> 
    ggplot(aes(fill = percent_lacking_plumbing)) +
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
      name = sprintf("Households lacking plumbing (%s)", year),
      limits = c(0, 5),
      breaks = c(0, 1, 2, 3, 4, 5),
      palette = "lajolla",
      direction = -1,
      end = 0.95,
      na.value = "white",
      labels = function(x) paste0(x, "%")
    )
}

# View maps for 2023 and 2022
map_change_plumbing(western_data_2023_wide, 2023)
map_change_plumbing(western_data_2022_wide, 2022)

# Combine and prepare data for analysis
western_data_combined <- western_data_2023_wide |> 
  st_drop_geometry() |> 
  select(name, total_pop_2023 = total_pop, percent_lacking_plumbing_2023 = percent_lacking_plumbing) |> 
  left_join(
    western_data_2022_wide |> 
      st_drop_geometry() |> 
      select(name, total_pop_2022 = total_pop, percent_lacking_plumbing_2022 = percent_lacking_plumbing),
    by = "name"
  ) |> 
  mutate(
    state = str_extract(name, "(?<=, ).*"),
    county = str_remove(name, ",\\s*[^,]+$")
  )

# Dumbbell plot function
plot_dumbbell <- function(data, state_of_interest) {
  ggplot(
    data |> filter(state == state_of_interest), 
    aes(y = reorder(county, percent_lacking_plumbing_2023))
  ) +
    geom_dumbbell(
      aes(x = percent_lacking_plumbing_2022, xend = percent_lacking_plumbing_2023),
      size = 1.5, color = "#adadff"
    ) +
    geom_point(aes(x = percent_lacking_plumbing_2022, color = "2022"), size = 3) +
    geom_point(aes(x = percent_lacking_plumbing_2023, color = "2023"), size = 3) +
    labs(
      title = sprintf("Change in incomplete plumbing facilities in %s", state_of_interest),
      x = "Percent Lacking Plumbing",
      y = "",
      color = "Year"
    ) +
    scale_color_manual(values = c("2022" = "#adadff", "2023" = "#00008B")) +
    theme_minimal(base_size = 10) +
    scale_x_continuous(labels = function(x) paste0(x, "%"))
}

# Generate dumbbell plots
plot_dumbbell(western_data_combined, "New Mexico")
plot_dumbbell(western_data_combined, "Arizona")

# Prepare data for interactive scatter plot
western_data_long <- western_data_combined |> 
  pivot_longer(
    cols = starts_with("percent_lacking_plumbing"),
    names_to = "year", values_to = "percent_lacking_plumbing"
  ) |> 
  mutate(year = if_else(year == "percent_lacking_plumbing_2023", 2023, 2022)) |> 
  pivot_longer(
    cols = starts_with("total_pop"),
    names_to = "pop_year", values_to = "total_pop"
  ) |> 
  filter((year == 2023 & pop_year == "total_pop_2023") | (year == 2022 & pop_year == "total_pop_2022")) |> 
  select(-pop_year)

# Interactive scatter plot
p <- ggplot(
  western_data_long,
  aes(
    x = total_pop,
    y = percent_lacking_plumbing,
    color = factor(year),
    text = glue("County: {name}<br>Total Population: {total_pop}<br>Percent Lacking Plumbing: {round(percent_lacking_plumbing, 2)}%")
  )
) +
  geom_point(size = 2) + 
  labs(x = "Total Population", y = "Percent Lacking Plumbing", color = "Year") +
  scale_x_continuous(labels = scales::comma) +
  scale_color_manual(values = c("2022" = "#005AB5", "2023" = "#DC3220")) +
  theme_minimal()

# Render interactive plot
ggplotly(p, tooltip = "text")
