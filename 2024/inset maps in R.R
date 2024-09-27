## Attach libraries

library(dplyr)
library(ggplot2)
library(sf)
library(cancensus)
library(scales)
library(cowplot)


## Import dissemination areas

DAs <- 
  cancensus::get_census(
    dataset = "CA16",
    regions = list(PR = "12"), 
    level = "DA",
    geo_format = "sf"
  ) %>% 
  sf::st_transform(32617)


## Load listing data, join to DAs, and simplify geometry

load(url("https://upgo.lab.mcgill.ca/data/listings_NS.Rdata"))

DAs <- 
  DAs %>% 
  dplyr::left_join(listings_NS) %>%
  dplyr::select(GeoUID, dwellings = Dwellings, listings = n, geometry) %>% 
  sf::st_simplify(preserveTopology = TRUE, dTolerance = 5)


## Make main map object

main_map <- 
  DAs %>% 
  ggplot() +
  geom_sf(
    aes(fill = listings / dwellings),
    lwd = 0, 
    colour = "white"
  ) +
  geom_rect(
    xmin = 1869227,
    ymin = 5086142,
    xmax = 1887557,
    ymax = 5104660,
    fill = NA, 
    colour = "black",
    size = 0.6
  ) +
  scale_fill_gradientn(
    colors = c("#9DBF9E", "#FCB97D", "#A84268"),
    na.value = "grey80",
    limits = c(0, 0.1),
    oob = scales::squish,
    labels = scales::percent,
    name = "Active STRs as share of total dwellings"
  ) +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(
    legend.justification = c(0, 1),
    legend.position = c(0, .95)
  ) +
  theme(
    text = element_text(family = "Futura-Medium"),
    legend.title = element_text(family = "Futura-Bold", size = 10),
    legend.text = element_text(family = "Futura-Medium", size = 10)
  )  


## Assemble final map with inset

main_map %>% 
  ggdraw() +
  draw_plot(
    {
      main_map + 
        coord_sf(
          xlim = c(1869227, 1887557),
          ylim = c(5086142, 5104660),
          expand = FALSE) +
        theme(legend.position = "none")
    },
    x = 0.58, 
    y = 0,
    width = 0.46, 
    height = 0.46)
)