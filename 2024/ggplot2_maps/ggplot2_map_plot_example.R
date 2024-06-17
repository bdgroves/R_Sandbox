# Load libraries
library(ggplot2)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)

# Set theme for plots
theme_set(theme_bw())

# Load world data using Natural Earth dataset
world <- ne_countries(scale = "medium", returnclass = "sf")

# Filter to include only Sardinia from Italy's geometry
italy <- world[world$admin == "Italy", ]
sardinia <- st_crop(italy, xmin = 7, xmax = 11, ymin = 38, ymax = 42)

# Get centroid for labeling points on the map
sardinia_points <- st_centroid(sardinia)
sardinia_points <- cbind(sardinia, st_coordinates(sardinia_points$geometry))

# Save the map of Sardinia
ggplot(data = sardinia) +
  geom_sf(fill = "antiquewhite") +
  geom_text(data = sardinia_points, aes(x = X, y = Y, label = admin),
            color = "darkblue", fontface = "bold", check_overlap = TRUE) +
  coord_sf(xlim = c(7, 11), ylim = c(38, 42), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotate(geom = "text", x = 9, y = 37.5, label = "Mediterranean Sea", 
           fontface = "italic", color = "grey22", size = 6) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Map of Sardinia") +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "aliceblue"),
    legend.position = "bottomleft"
  )

# Save the map as a PDF
ggsave("map.pdf")

# Save the map as a PNG suitable for web display
ggsave("map_web.png", width = 6, height = 6, dpi = "screen")

# Real archaeological sites data in Sardinia
sites <- data.frame(
  name = c("Nuraghe Santu Antine", "Su Nuraxi di Barumini", "Tharros"),
  longitude = c(8.7202, 8.9924, 8.5295),
  latitude = c(40.4705, 39.6797, 39.9384)
)

# Convert sites data to sf object with specified CRS
sites <- st_as_sf(sites, coords = c("longitude", "latitude"), crs = 4326)

# Create a plot of Sardinia with archaeological sites
ggplot(data = sardinia) +
  geom_sf(fill = "antiquewhite") +
  geom_sf(data = sites, size = 4, shape = 23, fill = "darkred") +
  geom_text(data = sardinia_points, aes(x = X, y = Y, label = admin),
            color = "darkblue", fontface = "bold", check_overlap = TRUE) +
  coord_sf(xlim = c(7, 11), ylim = c(38, 42), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotate(geom = "text", x = 9, y = 37.5, label = "Mediterranean Sea", 
           fontface = "italic", color = "grey22", size = 6) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Map of Sardinia with Archaeological Sites") +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "aliceblue"),
    legend.position = "bottomleft"
  )

# Create a basic plot of Sardinia with just the boundaries
ggplot(data = sardinia) +
  geom_sf() +
  geom_sf(data = sites, size = 4, shape = 23, fill = "darkred") +
  coord_sf(xlim = c(7, 11), ylim = c(38, 42), expand = FALSE)
