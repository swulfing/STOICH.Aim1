
## script to look at some descriptives about the metadata

library(tidyverse)
library(MetBrewer)
library(sf)
library(ggspatial)
library(raster)
library(rnaturalearth)
library(rgeos)


source("Code/THE_DATA.R")

observations.per.site.of.CNP <- ALL_CNP %>%
  count(SITE_ID)




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

# lakesonly <- ALL_CNP |>
#   dplyr::select(SITE_ID, LAT, LON, ECO_TYPE) |>
#   filter(ECO_TYPE == "Lake") |>
#   distinct()

ggplot() +
  geom_sf(world, mapping = aes(), fill = "white") +
  geom_point(LAKES, mapping = aes(LON, LAT), color = "blue") +
  theme_bw() +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Lakes") +
  theme(legend.title = element_blank()) +
  coord_sf(xlim = c(-171.565140, 49.021941), ylim = c(10.942356, 88.306487), expand = FALSE)


#lake medians 
lakesstats <- LAKES |>
  summarise(minDOC = min(DOC),
            maxDOC = max(DOC),
            minNO3 = min(NO3.as.N),
            maxNO3 = max(NO3.as.N),
            minTP = min(TP),
            maxTP = max(TP),
            m.DOC = median(DOC),
            m.NO3 = median(NO3.as.N),
            m.TP = median(TP),
            mean.DOC = mean(DOC),
            mean.NO3 = mean(NO3.as.N),
            mean.TP = mean(TP),
            sd.DOC = sd(DOC),
            sd.NO3 = sd(NO3.as.N),
            sd.TP = sd(TP))



# where are rivers?

# riversonly <- ALL_CNP |>
#   dplyr::select(SITE_ID, LAT, LON, ECO_TYPE) |>
#   filter(ECO_TYPE == "River/Stream") |>
#   distinct()

ggplot() +
  geom_sf(world, mapping = aes(), fill = "white") +
  geom_point(RIVERS, mapping = aes(LON, LAT), color = "blue") +
  theme_bw() +
  labs(x = "Longitude",
       y = "Latitude",
       title = "Rivers") +
  theme(legend.title = element_blank()) +
  coord_sf(xlim = c(-171.565140, 49.021941), ylim = c(10.942356, 88.306487), expand = FALSE)


#river medians 
riversstats <- RIVERS |>
  summarise(minDOC = min(DOC),
            maxDOC = max(DOC),
            minNO3 = min(NO3.as.N),
            maxNO3 = max(NO3.as.N),
            minPO4 = min(PO4.as.P),
            maxPO4 = max(PO4.as.P),
            m.DOC = median(DOC),
            m.NO3 = median(NO3.as.N),
            m.PO4 = median(PO4.as.P),
            mean.DOC = mean(DOC),
            mean.NO3 = mean(NO3.as.N),
            mean.PO4 = mean(PO4.as.P),
            sd.DOC = sd(DOC),
            sd.NO3 = sd(NO3.as.N),
            sd.PO4 = sd(PO4.as.P))




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

#prop.sites
#write.csv(prop.sites, "C:/Users/linne/OneDrive/Desktop/prop.sites.csv")


library(colorblindr)

subset <-  bind_rows(LAKES, RIVERS) |>
  pivot_longer(8:11, names_to = "VARIABLE", values_to = "RESULT") |>
  mutate(VARIABLE = ifelse(VARIABLE %in% c("TP", "PO4.as.P"), "P", VARIABLE)) |>
  drop_na(RESULT) |>
  mutate(ECO_TYPE = ifelse(VARIABLE == "P" & 
                             ECO_TYPE == "Lake", "Lake - TP", ECO_TYPE)) |>
  mutate(ECO_TYPE = ifelse(VARIABLE == "P" & 
                             ECO_TYPE == "River/Stream", "River/Stream - PO4", ECO_TYPE))
 
ggplot(subset) +
  geom_boxplot(aes(ECO_TYPE, log10(RESULT), fill = ECO_TYPE)) +
  theme_light() +
  theme(legend.position = "none") +
 # scale_fill_manual(values = c("red4", "red4", "#336a98", "#336a98")) +
  facet_wrap(~VARIABLE, scales = "free_x") +
  labs(x = "",
       y = "log10(unit)",
       caption = "Figure XX. Boxplots comparing distrubtion of logged DOC, nitrate, and phosphorus in lakes vs. streams.")  +
  theme(
    plot.caption = element_text(hjust = 0)
  )

ggsave("Figures/boxplot_basic_stats.png", height = 4.5, width = 6.5, dpi = 500)




