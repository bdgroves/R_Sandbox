library(tidyverse)
library(readxl)
library(ozmaps) 
library(grid)
library(gt)

# coordinates
data <- read.csv("https://raw.githubusercontent.com/MohsinRamay/sampledata/main/GPS_coordinates_for_map.csv") %>% 
  select(-3)

gt(head(data))

# Australia map
sf_aus <- ozmap("states")

# cities 
town <- data %>% 
  arrange(Town) %>% 
  group_by(Town) %>% 
  filter(row_number()==1) %>% 
  ungroup()

gt(town)

data %>% 
  ggplot() + 
  geom_sf(data = sf_aus) +
  geom_point(aes(x = Longitude, y = Latitude, color = Weed.species, shape = Site)) +
  xlim(112, 155) +
  labs() +
  theme_bw()

data %>% 
  ggplot() + 
  geom_sf(data = sf_aus) +
  geom_point(aes(x = Longitude, y = Latitude, color = Weed.species, shape = Site)) +
  xlim(112, 155) +
  geom_hline(yintercept = -38, lty = 2, colour = "red") +
  geom_hline(yintercept = -29, lty = 2, colour = "red") +
  geom_vline(xintercept = 147, lty = 2, colour = "red") +
  geom_vline(xintercept = 153, lty = 2, colour = "red") +
  labs() +
  theme_bw()

data %>% 
  ggplot() + 
  geom_sf(data = sf_aus) +
  geom_point(aes(x = Longitude, y = Latitude, color = Weed.species, shape = Site)) +
  xlim(112, 155) +
  geom_rect(aes(xmin = 147, xmax = 153, ymin = -38, ymax = -29), color = "red", fill = NA) +
  labs() +
  theme_bw()

data %>% 
  mutate(point_size = ifelse(Site == "Demonstration", 1, 0)) %>% 
  ggplot() + 
  geom_sf(data = sf_aus) +
  geom_point(aes(x = Longitude, y = Latitude, color = Weed.species, shape = Site, size = point_size > 0), alpha = 0.75) +
  scale_size_manual(values=c(2,3.5)) +
  xlim(147, 153) +
  ylim(-38, -30) +
  theme_test() +
  guides(size = "none")

# Now the dissolve
library(sf)

AN = sf_aus %>% 
  filter(NAME %in% c("New South Wales", "Australian Capital Territory"))

NM <- st_union(AN)

plot(NM)

ss <- data %>% 
  mutate(point_size = ifelse(Site == "Demonstration", 1, 0)) %>% 
  ggplot() + 
  geom_sf(data = NM) +
  geom_point(aes(x = Longitude, y = Latitude, color = Weed.species, shape = Site, size = point_size > 0), alpha = 0.75) +
  scale_size_manual(values=c(2,3.5)) +
  xlim(147, 153) +
  ylim(-38, -30) +
  ggrepel::geom_text_repel(aes(x = Longitude, y = Latitude, label = Town), data = town, nudge_y = 0.06, nudge_x = 0.06, size = 2) +
  labs(color = "Weed species") +
  theme_test() +
  theme(panel.background = element_rect(fill = "lightblue")) +
  guides(size = "none") +
  ggsn::north(location = "topleft", scale = 0.8, symbol = 12,
              x.min = 151.5, x.max = 152.5, y.min = -36, y.max = -38) +
  ggsn::scalebar(location = "bottomleft", dist = 100,
                 dist_unit = "km", transform = TRUE, 
                 x.min=150.5, x.max=152, y.min=-38, y.max=-30,
                 st.bottom = FALSE, height = 0.025,
                 st.dist = 0.05, st.size = 3)

ss

# Map of Australia
inset <- sf_aus %>% 
  ggplot() + 
  geom_sf() +
  #geom_sf(data = NM) +
  geom_rect(aes(xmin = 147, xmax = 153, ymin = -38, ymax = -30), color = "red", fill = NA) +
  xlim(112, 155) +
  labs(x = NULL, y = NULL) +
  theme_test() + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.ticks.length = unit(0, "pt"),
        axis.title=element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        panel.background = element_rect(fill = "lightblue"))

inset

ss

# Combining both maps
print(inset, vp = viewport(0.322, 0.859, width = 0.25, height = 0.25))