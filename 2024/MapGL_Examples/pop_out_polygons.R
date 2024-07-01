library(tigris)
# remotes::install_github("walkerke/mapgl")
library(mapgl) 
options(tigris_use_cache = TRUE)

tuolumne = tracts("CA", "Tuolumne", cb = TRUE)

mapboxgl(style = mapbox_style("light")) |> 
  add_fill_extrusion_layer(
    id = "tracts",
    source = tuolumne,
    fill_extrusion_color = "blue",
    fill_extrusion_opacity = 0.6,
    fill_extrusion_height = 50,
    popup = "GEOID",
    hover_options = list(
      fill_extrusion_color = "red",
      fill_extrusion_height = 5000
    )
  )