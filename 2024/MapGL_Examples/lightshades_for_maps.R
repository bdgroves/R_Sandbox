library(mapgl)

mapboxgl(
  zoom = 16.5,
  center = c(-122.3493, 47.6205), # Longitude, Latitude of Seattle Space Needle
  pitch = 75,
  bearing = 95.2
) |>
  set_config_property(
    "basemap",
    "lightPreset",
    "dawn" # Swap in "dawn", "day",night, or "dusk"
  )
