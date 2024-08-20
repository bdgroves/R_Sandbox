library(mapgl)

mapboxgl(
  style = mapbox_style("satellite-streets"),
  center = c(-114.26608,32.7213),
  zoom = 14,
  pitch = 80,
  bearing = 41
)

add_raster_dem_source(
  id = "mapbox-dem",
  url = "mapbox://mapbox.mapbox-terrain-dem-v1",
  tileSize = 512,
  maxzoom = 14
)
set_terrain(
  source = "mapbox-dem",
  exaggeration = 1.5
)
