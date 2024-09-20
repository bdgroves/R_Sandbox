library(mapgl)
library(sf)

snakes <- st_read("c:/data/snakes_20240912.shp")

mapboxgl(bounds = snakes) |> 
  add_fill_layer(id = "snakes",
                 source = snakes,
                 fill_color = "red",
                 fill_opacity = 0.5)