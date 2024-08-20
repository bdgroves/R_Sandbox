library(mapgl)
library(sf)
library(dplyr)

# Set seed for reproducibility
set.seed(1234)

# Define the bounding box for Washington DC (approximately)
bbox <- st_bbox(c(
  xmin = -77.119759,
  ymin = 38.791645,
  xmax = -76.909393,
  ymax = 38.995548
),
crs = st_crs(4326))

# Generate 30 random points within the bounding box
random_points <- st_as_sf(
  data.frame(
    id = 1:30,
    lon = runif(30, bbox["xmin"], bbox["xmax"]),
    lat = runif(30, bbox["ymin"], bbox["ymax"])
  ),
  coords = c("lon", "lat"),
  crs = 4326
)

# Assign random categories
categories <- c('music', 'bar', 'theatre', 'bicycle')
random_points <- random_points %>%
  mutate(category = sample(categories, n(), replace = TRUE))

# Map with circle layer
mapboxgl(style = mapbox_style("dark"),
         bounds = random_points) %>%
  add_circle_layer(
    id = "poi-layer",
    source = random_points,
    circle_color = match_expr(
      "category",
      values = c("music", "bar", "theatre",
                 "bicycle"),
      stops = c("#1f78b4", "#33a02c",
                "#e31a1c", "#ff7f00")
    ),
    circle_radius = 8,
    circle_stroke_color = "#ffffff",
    circle_stroke_width = 2,
    circle_opacity = 0.8,
    tooltip = "category",
    hover_options = list(circle_radius = 12,
                         circle_color = "#ffff99")
  ) %>%
  add_categorical_legend(
    legend_title = "Points of Interest",
    values = c("Music", "Bar", "Theatre", "Bicycle"),
    colors = c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00"),
    circular_patches = TRUE
  )