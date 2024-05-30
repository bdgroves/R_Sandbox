# Load required libraries
library(tidyverse)
library(sf)
library(USAboundaries)

# Get California counties geometry
map <- us_counties(states = "California")

# Inspect column names
names(map)

# Remove the duplicated column at location 13
map <- map %>%
  select(-state_name)

# Generate fake data for the example
set.seed(123)
map <- map %>%
  rename(
    orig_geoid = geoid,
    orig_name = name
  ) %>%
  mutate(
    value = runif(n(), 15, 30),  # generate random values between 15 and 30
    cd_dprt = as.character(orig_geoid),
    dprtmnt = orig_name
  ) %>%
  select(cd_dprt, dprtmnt, value, geometry)

# Create classes based on 'value'
clean <- map %>%
  mutate(clss = case_when(
    value < 18 ~ "1",
    value < 20 ~ "2",
    value < 22 ~ "3",
    value < 24 ~ "4",
    value < 26 ~ "5",
    TRUE ~ "6"
  ))

# Set color palette
pal <- c("#bb3e03", "#ee9b00", "#e9d8a6", "#94d2bd", "#0a9396", "#005f73")
# Set color background
bck <- "#001219"

# Set theme
theme_custom <- theme_void() +
  theme(
    plot.margin = margin(1, 1, 10, 1, "pt"),
    plot.background = element_rect(fill = bck, color = NA),
    legend.position = "bottom",
    legend.title = element_text(hjust = 0.5, color = "white", face = "bold"),
    legend.text = element_text(color = "white")
  )

# Make choropleth
ggplot(clean, aes(fill = clss)) +
  geom_sf() +
  labs(fill = "Member of a sport association") +
  guides(
    fill = guide_legend(
      nrow = 1,
      title.position = "top",
      label.position = "bottom"
    )
  ) +
  scale_fill_manual(
    values = pal,
    labels = c("< 18 %", "< 20 %", "< 22 %", "< 24 %", "< 26 %", "≥ 26 %")
  ) +
  theme_custom

# Make grid
grd <- st_make_grid(
  clean, # map name
  n = c(60, 60) # number of cells per longitude/latitude
) %>%
  # convert back to sf object
  st_sf() %>%
  # add a unique id to each cell
  mutate(id = row_number())

# Extract centroids
cent <- grd %>%
  st_centroid()

# Take a look at the results
ggplot() +
  geom_sf(grd, mapping = aes(geometry = geometry)) +
  geom_sf(cent, mapping = aes(geometry = geometry), pch = 21, size = 0.5) +
  theme_void()

# Intersect centroids with basemap
cent_clean <- cent %>%
  st_intersection(clean)

# Make a centroid without geom
# (convert from sf object to tibble)
cent_no_geom <- cent_clean %>%
  st_drop_geometry()

# Join with grid thanks to id column
grd_clean <- grd %>%
  left_join(cent_no_geom)

ggplot() +
  geom_sf(
    grd_clean %>% drop_na(),
    mapping = aes(geometry = geometry, fill = clss)
  ) +
  geom_sf(cent_clean, mapping = aes(geometry = geometry), fill = NA, pch = 21, size = 0.5) +
  labs(fill = "Member of a sport association") +
  guides(
    fill = guide_legend(
      nrow = 1,
      title.position = "top",
      label.position = "bottom"
    )
  ) +
  scale_fill_manual(
    values = pal,
    labels = c("< 18 %", "< 20 %", "< 22 %", "< 24 %", "< 26 %", "≥ 26 %")
  ) +
  theme_custom

# Set offset
off <- 2000

# Create second centroid
cent_off <- cent_clean %>%
  mutate(
    lon = st_coordinates(.)[, 1] + off,
    lat = st_coordinates(.)[, 2]
  ) %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c('lon', 'lat')) %>%
  st_set_crs(st_crs(cent_clean))

# Make map!
ggplot() +
  geom_sf(
    grd_clean %>% drop_na(),
    mapping = aes(geometry = geometry, fill = clss)
  ) +
  geom_sf(cent_off, mapping = aes(geometry = geometry), color = alpha("black", 0.5), size = 0.5) +
  geom_sf(cent_clean, mapping = aes(geometry = geometry, color = clss), size = 0.5) +
  geom_sf(cent_clean, mapping = aes(geometry = geometry), fill = NA, pch = 21, size = 0.5) +
  labs(fill = "Member of a sport association") +
  guides(
    color = 'none',
    fill = guide_legend(
      nrow = 1,
      title.position = "top",
      label.position = "bottom"
    )
  ) +
  scale_fill_manual(
    values = pal,
    labels = c("< 18 %", "< 20 %", "< 22 %", "< 24 %", "< 26 %", "≥ 26 %")
  ) +
  scale_color_manual(values = pal) +
  theme_custom
