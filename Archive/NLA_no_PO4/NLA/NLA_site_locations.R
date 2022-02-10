library(tidyverse)
library(sf)

NLA_sites <- read.csv("Data/NLA/NLA_unique_sites.csv")

NLA_sites_sf <- NLA_sites %>%
  st_as_sf(coords = c("LON_DD83", "LAT_DD83"), crs = 4326)


ggplot() +
  geom_sf(NLA_sites_sf, mapping = aes(color = ECO_TYPE)) +
  theme_bw()
