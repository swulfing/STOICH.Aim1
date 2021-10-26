#Script to map locations of NEON data

library(tidyverse)
library(sf)
library(ggspatial)
library(raster)
library(rnaturalearth)
library(rgeos)

sites <- read.csv("Data/NEON/SIMPLE_NEON_SPATIAL.csv")
states <- ne_countries(country = "United States of America", returnclass = "sf")


ggplot() +
  geom_sf(states, mapping = aes(), fill = "white") +
  geom_point(sites, mapping = aes(decimalLongitude, decimalLatitude, color = aquaticSiteType)) +
  theme_bw() +
  labs(x = "Longitude",
       y = "Latitude") +
  theme(legend.title = element_blank()) +
  caption

ggsave("Plots/NEON_site_locations.png", width = 6.25, height = 4.25, units = "in", dpi = 500)
