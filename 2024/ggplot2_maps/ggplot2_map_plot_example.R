library(ggplot2)
theme_set(theme_bw())
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)

# Load the world data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Filter to include only Sardinia
# Since Sardinia is part of Italy, we'll need to extract it from Italy's geometry
italy <- world[world$admin == "Italy", ]
sardinia <- st_crop(italy, xmin = 7, xmax = 11, ymin = 38, ymax = 42)

# Get the centroid for Sardinia for labeling
sardinia_points <- st_centroid(sardinia)
sardinia_points <- cbind(sardinia, st_coordinates(sardinia_points$geometry))

# Create the plot centered around Sardinia with a more appealing theme
ggplot(data = sardinia) +
  geom_sf(fill = "antiquewhite") +
  geom_text(data= sardinia_points, aes(x=X, y=Y, label=admin),
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

ggsave("map.pdf")
ggsave("map_web.png", width = 6, height = 6, dpi = "screen")