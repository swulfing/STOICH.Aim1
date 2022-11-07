# source("Code/THE_DATA.R")
library(tidyverse)
ALL_CNP <- read_csv("Code/DNguyen/ALL_CNP.csv")

# ok ranges?
apply(ALL_CNP[,c("DOC", "PO4.as.P", "NO3.as.N", "TP")], 2, range, na.rm = TRUE)
#         DOC     PO4.as.P NO3.as.N      TP
# [1,]   0.01 0.0001486568   0.0001  0.0001
# [2,] 290.57 3.6710000000  51.6600 11.2388

# make year col to do calculate annual median
ALL_CNP <- ALL_CNP %>% mutate(year = format(DATE_COL, "%Y"))

# get median of each Lat,LON for each year
ALL_CNP_med_each_year <- 
  ALL_CNP %>% #filter(country %in% country_names) %>%
  # group_by(SITE_ID) %>%
  group_by(LAT, LON, year) %>%
  summarise(#country = unique(country), 
            year = unique(year),
            LAT = unique(LAT),
            LON = unique(LON),
            ECO_TYPE = unique(ECO_TYPE),
            N_med = median(NO3.as.N, na.rm = TRUE),
            P_med = median(PO4.as.P, na.rm = TRUE),
            TP_med = median(TP, na.rm = TRUE),
            DOC_med = median(DOC, na.rm = TRUE))

# get median of each LAT,LON over all year medians
# e.g., median of annual medians like Sarah suggested since this won't
# "overweight" years that have more sampling
ALL_CNP_med <- ALL_CNP_med_each_year %>% 
  group_by(LAT, LON) %>%
  summarise(#country = unique(country),
            LAT = unique(LAT),
            LON = unique(LON),
            ECO_TYPE = unique(ECO_TYPE),
            N_med = median(N_med, na.rm = TRUE),
            P_med = median(P_med, na.rm = TRUE),
            TP_med = median(TP_med, na.rm = TRUE),
            DOC_med = median(DOC_med, na.rm = TRUE))

ALL_CNP_med

world_map <- map_data("world")

# -50 LON is still good cutoff
ALL_CNP_med %>% 
  ggplot() +
  geom_map(aes(map_id = region), col = "gray", fill = "white",
           data = world_map, map = world_map) +
  geom_point(aes(
    x = LON,
    y = LAT)) +
  facet_wrap(~ECO_TYPE)

ALL_CNP_med$region <- factor(ifelse(ALL_CNP_med$LON < -50, "North America", "Europe"))

set_alpha <- 0.5
set_shape <- 3

NA_plot <- ALL_CNP_med %>% 
  filter(region == "North America") %>%
  ggplot() +
  geom_map(aes(map_id = region), col = "gray", fill = "white",
           data = world_map, map = world_map) +
  geom_point(aes(
    x = LON,
    y = LAT),
    alpha = set_alpha, shape = set_shape
    ) +
  facet_wrap(~ECO_TYPE)

EU_plot <- ALL_CNP_med %>% 
  filter(region == "Europe") %>%
  ggplot() +
  geom_map(aes(map_id = region), col = "gray", fill = "white",
           data = world_map, map = world_map) +
  geom_point(aes(
    x = LON,
    y = LAT),
    alpha = set_alpha, shape = set_shape
  ) +
  facet_wrap(~ECO_TYPE)

full_map <- gridExtra::grid.arrange(NA_plot, EU_plot, nrow = 2)

ggsave(plot = full_map, file = "Figures/sample_site_map.png", height = 12, width = 14, units = "cm")

ALL_CNP_med %>%
  group_by(region, ECO_TYPE) %>%
  summarise(nsites = n())

write.csv(ALL_CNP_med, "Code/DNguyen/data/ALL_CNP_med.csv")
