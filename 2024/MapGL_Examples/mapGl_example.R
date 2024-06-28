library(mapgl)

mapboxgl()

mapboxgl(
  style = mapbox_style("satellite"),
  projection = "winkelTripel")

mapboxgl(
  center = c(-97.6, 25.4)
) |> 
  fly_to(
    center = c(-96.810481, 32.790869),
    zoom = 15,
    pitch = 75,
    bearing = 136.8
  )

mapboxgl(
  center = c(-97.6, 25.4)
) |> 
  fly_to(
    center = c(-122.3493, 47.6205),  # Coordinates for the Space Needle
    zoom = 16,
    pitch = 75,
    bearing = 0
  )

