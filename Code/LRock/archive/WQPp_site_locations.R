library(tidyverse)
library(sf)
library(ggspatial)
library(raster)
library(rnaturalearth)
library(rgeos)

states <- ne_countries(country = "United States of America", returnclass = "sf")
WQP_sites <- read.csv("Data/WQP_unique_sites.csv")

WQP_sites_sf <- combine1 %>%
  st_as_sf(coords = c("LON", "LAT"), crs = 4326)


ggplot() +
  geom_sf(states, mapping = aes(), fill = "white") +
  geom_sf(WQP_sites_sf, mapping = aes(color = ECO_TYPE)) +
  theme_bw()
