# Load necessary libraries
library(XML)
library(leaflet)
library(dplyr)

# Parse the GPX file
gpx_parsed <- htmlTreeParse(file = "C:/Users/v-brooksg/Downloads/activity_908106687.gpx", useInternalNodes = TRUE)

# Extract coordinates and elevation data from the parsed GPX file
coords <- xpathSApply(doc = gpx_parsed, path = "//trkpt", fun = xmlAttrs)
elevation <- xpathSApply(doc = gpx_parsed, path = "//trkpt/ele", fun = xmlValue)

# Create a data frame with latitude, longitude, and elevation
df <- data.frame(
  lat = as.numeric(coords["lat", ]),
  lon = as.numeric(coords["lon", ]),
  elevation = as.numeric(elevation)
)

# Display the first and last 10 rows of the data frame
print(head(df, 10))
print(tail(df, 10))

# Plot the GPS track
plot(x = df$lon, y = df$lat, type = "l", col = "black", lwd = 3,
     xlab = "Longitude", ylab = "Latitude")

# Create an interactive map with Leaflet
leaflet() %>%
  addTiles() %>%
  addPolylines(data = df, lat = ~lat, lng = ~lon, color = "#000000", opacity = 0.8, weight = 3) %>%
  print()

# Define a function to assign colors based on elevation
get_color <- function(elevation) {
  if (elevation < 500) {
    return("green")
  } else if (elevation < 1000) {
    return("yellow")
  } else if (elevation < 1500) {
    return("orange")
  } else {
    return("red")
  }
}

# Add a new column 'color' to the data frame based on elevation
df_color <- df %>%
  rowwise() %>%
  mutate(color = get_color(elevation))

# Add a column for the previous row's color to handle segment coloring
df_color <- df_color %>%
  mutate(last_color = lag(color))

# Initialize a Leaflet map
map <- leaflet() %>%
  addTiles()

# Add polylines to the map, segmenting by color
unique_colors <- unique(df_color$color)
for (color in unique_colors) {
  map <- addPolylines(
    map,
    data = df_color %>%
      filter(color == color | last_color == color),
    lat = ~lat,
    lng = ~lon,
    color = color,
    weight = 3,
    opacity = 0.8
  )
}

# Add markers for debugging purposes (optional)
map <- map %>%
  addCircleMarkers(data = df_color, lat = ~lat, lng = ~lon, radius = 1,
                   popup = ~paste("Elevation:", elevation, "Color:", color))

# Display the map
print(map)
