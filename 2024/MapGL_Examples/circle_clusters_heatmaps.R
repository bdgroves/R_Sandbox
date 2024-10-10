library(mapgl)
library(tidyverse)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

intox <- read_csv("https://raw.githubusercontent.com/walkerke/geog30323/refs/heads/master/intoxication.csv") %>%
  na.omit() %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_jitter(factor = 0.0001)

ft_worth <- places(cb = TRUE, year = 2023) |> 
  filter(NAME == "Fort Worth")

ggplot() + 
  geom_sf(data = ft_worth, fill = "navy", alpha = 0.2) + 
  geom_sf(data = intox, color = "red") + 
  theme_void()

ftw_map <- maplibre(
  style = maptiler_style("openstreetmap"), 
  bounds = ft_worth
) |>
  add_fill_layer(
    id = "city",
    source = ft_worth,
    fill_color = "navy",
    fill_opacity = 0.2
  ) 

ftw_map |> 
  add_circle_layer(
    id = "circles",
    source = intox,
    circle_color = "red",
    circle_stroke_color = "white",
    circle_stroke_width = 1
  ) 

ftw_map |> 
  add_circle_layer(
    id = "circles",
    source = intox,
    circle_color = "red",
    circle_stroke_color = "white",
    circle_stroke_width = 1,
    cluster_options = cluster_options()
  )

# Make this more custom
ftw_map |> 
  add_circle_layer(
    id = "circles",
    source = intox,
    circle_color = "red",
    circle_stroke_color = "white",
    circle_stroke_width = 1,
    cluster_options = cluster_options(
      cluster_radius = 30,
      color_stops = c("#377eb8", "#4daf4a", "#984ea3"), 
      count_stops = c(0, 200, 500),
      circle_blur = 0.2,
      circle_stroke_color = "white",
      circle_stroke_width = 5
    )
  )

ftw_map |> 
  add_heatmap_layer(
    id = "heatmap",
    source = intox
  )

ftw_map |> 
  add_heatmap_layer(
    id = "heatmap",
    source = intox,
    heatmap_radius = 10, 
    heatmap_color = interpolate(
      property = "heatmap-density",
      values = seq(0, 1, 0.2),
      stops = c("transparent", viridisLite::viridis(5))
    )
  )

library(glue)

intox <- intox %>%
  mutate(popup_content = glue('
    <div style="font-family: \'Open Sans\', sans-serif; max-width: 300px; padding: 10px; background-color: #f8f9fa; border-radius: 5px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);">
      <h3 style="color: #007bff; margin-top: 0;">Incident Details</h3>
      <p>
        <strong>Date:</strong> <span style="color: #495057;">{`Reported Date`}</span><br>
        <strong>Address:</strong> <span style="color: #495057;">{`Block Address`}</span><br>
        <strong>Location:</strong> <span style="color: #495057;">{`Location Description`}</span>
      </p>
    </div>
  '))

ftw_map |> 
  add_heatmap_layer(
    id = "heatmap",
    source = intox,
    heatmap_radius = 10, 
    heatmap_color = interpolate(
      property = "heatmap-density",
      values = seq(0, 1, 0.2),
      stops = c("transparent", viridisLite::viridis(5))
    ),
    heatmap_opacity = interpolate(
      property = "zoom",
      values = c(11, 14),
      stops = c(1, 0)
    )
  ) |> 
  add_circle_layer(
    id = "circles",
    source = intox,
    circle_color = "red",
    circle_stroke_color = "white",
    circle_stroke_width = 1, 
    min_zoom = 12.5,
    popup = "popup_content"
  ) 