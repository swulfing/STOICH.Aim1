library(tidyverse)
library(sf)
library(ggspatial)
library(raster)
library(rnaturalearth)
library(rgeos)

new.data <- read.csv("C:/Users/linne/Downloads/Data_All_Median_EDI.csv")
new.data.sf <- new.data %>%
  drop_na(Longitude) %>%
  dplyr::select(1:5) %>%
  mutate(ECO_TYPE = "River/Stream") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)


europe <- ne_countries(continent = "Europe", returnclass = "sf")
US <- ne_countries(country = "United States of America", returnclass = "sf")
canada <- ne_countries(country = "Canada", returnclass = "sf")

#I only want to collect data from places that we do not have yet!!
do.not.want <- rbind(europe, US, canada)



library(rmapshaper)
keep.points <- ms_erase(new.data.sf, do.not.want) #erases all points in my dataframe that are inside of the US, Canada, Europe shapefile

#create a quick map to check
world <- ne_countries(returnclass = "sf")

ggplot() +
  geom_sf(world, mapping = aes(), fill = "white") +
  geom_sf(keep.points, mapping = aes(color = ECO_TYPE)) +
  scale_color_viridis_d() +
  theme_bw() +
  labs(x = "Longitude",
       y = "Latitude",
       title = "All sites") +
  theme(legend.title = element_blank())
#looks good! 



keep <- left_join(as.data.frame(keep.points), new.data) %>%
  dplyr::select(1:4, 6:7)

check <- keep %>%
  dplyr::select(Latitude, Longitude) %>%
  distinct()

new.names <- check %>%
  mutate(LAT = Latitude,
         LON = Longitude,
         SITE_ID = paste0("Synthesis_", seq(1:nrow(check))),
         ECO_TYPE = "River/Stream") %>%
  dplyr::select(-Latitude, - Longitude)


the.data <- read.csv("C:/Users/linne/Downloads/All_sites_combined_EDI.csv")
df <- the.data %>%
  mutate(DATE_COL = as.Date(Sampling_Date, format = "%m/%d/%Y")) %>%
  filter(year(DATE_COL) >= 2000) %>%
  dplyr::select(DATE_COL, TP, NO3, DOC, 1:3) %>%
  drop_na()

df1 <- df %>%
  left_join(new.data %>% 
              dplyr::select(1, 4:5)) %>%
  drop_na() %>%
  rename(LAT = Latitude,
         LON = Longitude) %>%
  left_join(new.names)


