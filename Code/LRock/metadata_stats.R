
## script to look at some descriptives about the metadata

library(tidyverse)
library(MetBrewer)
library(sf)
library(ggspatial)
library(raster)
library(rnaturalearth)
library(rgeos)


source("Code/masterDataVARIABLES.R")


## how many observations at each site 
obs.1 <- observations.per.site.of.CNP |>
  filter(n < 2)

obs.2 <- observations.per.site.of.CNP |>
  filter(n == 2)

obs.3 <- observations.per.site.of.CNP |>
  filter(n == 3)

obs.4.10 <- observations.per.site.of.CNP |>
  filter(n < 11 & n > 3)

obs.11.50 <- observations.per.site.of.CNP |>
  filter(n < 51 & n > 10)

obs.51.100 <- observations.per.site.of.CNP |>
  filter(n < 101 & n > 50)

obs.g101 <- observations.per.site.of.CNP |>
  filter(n > 100)

# 992 sites have 1 observation
# 308 sites have 2 observations
# 1309 sites have 3 observations
# 242 sites have 4-10 observations
# 84 sites have 11-50 observations
# 39 sites have 51-100 observations 
# 21 sites have >100 observations 


groups <- observations.per.site.of.CNP |>
  mutate(g = ifelse(n == 1, "1 obs",
                    ifelse(n == 2, "2 obs",
                           ifelse(n == 3, "3 obs",
                                  ifelse(n < 11 & n > 3, "4-10 obs",
                                         ifelse(n < 51 & n > 10, "11-50 obs",
                                                ifelse(n < 101 & n > 50, "51-100 obs",
                                                       ">100 obs"))))))) |>
  mutate(g = factor(g, levels = c("1 obs", "2 obs", "3 obs", "4-10 obs", "11-50 obs", "51-100 obs", ">100 obs")))


ggplot(groups) +
  geom_bar(aes(g), stat = "count") +
  theme_light() +
  labs(x = "Number observations",
       y = "Number sites")


# where are lakes?
world <- ne_countries(returnclass = "sf")

lakesonly <- ALL_CNP |>
  dplyr::select(SITE_ID, LAT, LON, ECO_TYPE) |>
  filter(ECO_TYPE == "Lake") |>
  distinct()

ggplot() +
  geom_sf(world, mapping = aes(), fill = "white") +
  geom_point(lakesonly, mapping = aes(LON, LAT), color = "blue") +
  theme_bw() +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Lakes") +
  theme(legend.title = element_blank()) +
  coord_sf(xlim = c(-171.565140, 49.021941), ylim = c(10.942356, 88.306487), expand = FALSE)


#lake medians 
lakesstats <- ALL_CNP |>
  filter(ECO_TYPE == "Lake") |>
  summarise(minDOC = min(DOC),
            maxDOC = max(DOC),
            minNO3 = min(NO3.as.N),
            maxNO3 = max(NO3.as.N),
            minPO4 = min(PO4.as.P),
            maxPO4 = max(PO4.as.P),
            m.DOC = median(DOC),
            m.NO3 = median(NO3.as.N),
            m.PO4 = median(PO4.as.P))



# where are rivers?

riversonly <- ALL_CNP |>
  dplyr::select(SITE_ID, LAT, LON, ECO_TYPE) |>
  filter(ECO_TYPE == "River/Stream") |>
  distinct()

ggplot() +
  geom_sf(world, mapping = aes(), fill = "white") +
  geom_point(riversonly, mapping = aes(LON, LAT), color = "blue") +
  theme_bw() +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Rivers") +
  theme(legend.title = element_blank()) +
  coord_sf(xlim = c(-171.565140, 49.021941), ylim = c(10.942356, 88.306487), expand = FALSE)


#river medians 
riversstats <- ALL_CNP |>
  filter(ECO_TYPE == "River/Stream") |>
  summarise(minDOC = min(DOC),
            maxDOC = max(DOC),
            minNO3 = min(NO3.as.N),
            maxNO3 = max(NO3.as.N),
            minPO4 = min(PO4.as.P),
            maxPO4 = max(PO4.as.P),
            m.DOC = median(DOC),
            m.NO3 = median(NO3.as.N),
            m.PO4 = median(PO4.as.P))




#all dat medians 
alldatstats <- ALL_CNP |>
  summarise(minDOC = min(DOC),
            maxDOC = max(DOC),
            minNO3 = min(NO3.as.N),
            maxNO3 = max(NO3.as.N),
            minPO4 = min(PO4.as.P),
            maxPO4 = max(PO4.as.P),
            m.DOC = median(DOC),
            m.NO3 = median(NO3.as.N),
            m.PO4 = median(PO4.as.P))

prop.sites
write.csv(prop.sites, "C:/Users/linne/OneDrive/Desktop/prop.sites.csv")


library(colorblindr)

subset <-  ALL_CNP_VARS |>
  filter(VARIABLE %in% c("DOC", "NO3 as N", "PO4 as P")) 

ggplot(subset) +
  geom_boxplot(aes(ECO_TYPE, log10(RESULT), fill = ECO_TYPE)) +
  theme_light() +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("red4", "#336a98")) +
  facet_wrap(~VARIABLE) +
  labs(x = "",
       y = "log10(unit)",
       caption = "Figure XX. Boxplots comparing distrubtion of logged DOC, nitrate, and phosphate in lakes vs. streams.")  +
  theme(
    plot.caption = element_text(hjust = 0)
  )

ggsave("Figures/boxplot_basic_stats.png", height = 4.5, width = 6.5, dpi = 500)




