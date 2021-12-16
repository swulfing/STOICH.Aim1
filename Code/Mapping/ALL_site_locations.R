
library(tidyverse)
library(sf)
library(ggspatial)
library(raster)
library(rnaturalearth)
library(rgeos)

All_sites <- read.csv("Data/Simplified_datasets_per_source/SIMPLE_EU.csv") %>%
  rbind(read.csv("Data/Simplified_datasets_per_source/SIMPLE_LAGOS.csv")) %>%
  rbind(read.csv("Data/Simplified_datasets_per_source/SIMPLE_NEON.csv")) %>%
  rbind(read.csv("Data/Simplified_datasets_per_source/SIMPLE_NLA.csv")) %>%
  rbind(read.csv("Data/Simplified_datasets_per_source/SIMPLE_NRC.csv")) %>%
  dplyr::select(SITE_ID, LAT, LON, ECO_TYPE) %>%
  distinct()

world <- ne_countries(returnclass = "sf")

ggplot() +
  geom_sf(world, mapping = aes(), fill = "white") +
  geom_point(All_sites, mapping = aes(LON, LAT, color = ECO_TYPE)) +
  scale_color_viridis_d() +
  theme_bw() +
  labs(x = "Longitude",
       y = "Latitude",
       title = "All sites") +
  theme(legend.title = element_blank())


ggplot() +
  geom_sf(world, mapping = aes(), fill = "white") +
  geom_point(All_sites %>% filter(ECO_TYPE != "Lake"), mapping = aes(LON, LAT)) +
  theme_bw() +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Rivers/Streams") +
  theme(legend.title = element_blank())

ggplot() +
  geom_sf(world, mapping = aes(), fill = "white") +
  geom_point(All_sites %>% filter(ECO_TYPE == "Lake"), mapping = aes(LON, LAT)) +
  theme_bw() +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Lakes") +
  theme(legend.title = element_blank())
