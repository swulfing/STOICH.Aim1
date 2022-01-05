
library(tidyverse)
library(sf)
library(ggspatial)
library(raster)
library(rnaturalearth)
library(rgeos)


#call in data:
source("Code/masterData.R")

#get distinct locations
All_sites <- ALL_CNP %>%
  dplyr::select(SITE_ID, LAT, LON, ECO_TYPE) %>%
  distinct()

#global map
world <- ne_countries(returnclass = "sf")

#plot all sites
ggplot() +
  geom_sf(world, mapping = aes(), fill = "white") +
  geom_point(All_sites, mapping = aes(LON, LAT, color = ECO_TYPE)) +
  scale_color_viridis_d() +
  theme_bw() +
  labs(x = "Longitude",
       y = "Latitude",
       title = "All sites") +
  theme(legend.title = element_blank())

#plot river/stream sites
ggplot() +
  geom_sf(world, mapping = aes(), fill = "white") +
  geom_point(All_sites %>% filter(ECO_TYPE != "Lake"), mapping = aes(LON, LAT)) +
  theme_bw() +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Rivers/Streams") +
  theme(legend.title = element_blank())

#plot lake sites
ggplot() +
  geom_sf(world, mapping = aes(), fill = "white") +
  geom_point(All_sites %>% filter(ECO_TYPE == "Lake"), mapping = aes(LON, LAT)) +
  theme_bw() +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Lakes") +
  theme(legend.title = element_blank())


