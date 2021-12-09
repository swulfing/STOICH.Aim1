#mapping Canada site data

library(tidyverse)
library(sf)
library(ggspatial)
library(raster)
library(rnaturalearth)
library(rgeos)

NRC_sites <- read.csv("Data/Natural_Resources_Canada_Data/UNIQUE_SITES.csv")

#canada
canada <- ne_countries(country = "Canada", returnclass = "sf")

ggplot() +
  geom_sf(canada, mapping = aes(), fill = "white") +
  geom_point(NRC_sites, mapping = aes(LON, LAT, color = ECO_TYPE)) +
  theme_bw() +
  labs(x = "Longitude",
       y = "Latitude") +
  theme(legend.title = element_blank())
