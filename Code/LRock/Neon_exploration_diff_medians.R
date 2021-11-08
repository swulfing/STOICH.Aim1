library(tidyverse)
library(lubridate)
library(patchwork)

neon <- read.csv("Data/NEON/SIMPLE_SURFACE_WATER_CHEMS_DATA.csv")

#checking out site variation
p1 <- ggplot(neon %>% filter(analyte != "DOC")) +
  geom_boxplot(aes(siteID, analyteConcentration, color = analyte)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1)) 

p2 <- ggplot(neon %>% filter(analyte == "DOC")) +
  geom_boxplot(aes(siteID, analyteConcentration, color = analyte)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))

p1 | p2


#total median of all time per site####
median_neon <- neon %>%
  group_by(siteID, analyte) %>%
  mutate(median_concentration = median(analyteConcentration)) %>%
  ungroup() %>%
  select(siteID, analyte, median_concentration, analyteUnits) %>%
  distinct() %>%
  pivot_wider(names_from = analyte, values_from = median_concentration) %>%
  rename(no2no3 = "NO3+NO2 - N")


ggplot(median_neon) +
  geom_point(aes(DOC, no2no3, color = TP)) +
 # scale_y_log10() +
  scale_color_viridis_c(direction = -1, "Total Phosphorus"~mg~L^-1) +
  theme_bw() +
  labs(x = "DOC"~mg~L^-1,
       y = "Nitrate"~mg~L^-1)




#median per date, site####
daily_median_neon <- neon %>%
  group_by(siteID, DateTime, analyte) %>%
  mutate(median_concentration = median(analyteConcentration)) %>%
  ungroup() %>%
  select(-analyteConcentration, -X, -sampleCondition) %>%
  distinct() %>%
  pivot_wider(id_cols = c(siteID, DateTime), names_from = analyte, values_from = median_concentration) %>%
  rename(no2no3 = "NO3+NO2 - N") 


ggplot(daily_median_neon) +
  geom_point(aes(DOC, no2no3, color = TP)) +
  #scale_y_log10() +
  scale_color_viridis_c(direction = -1, "Total Phosphorus"~mg~L^-1) +
  theme_bw() +
  labs(x = "DOC"~mg~L^-1,
       y = "Nitrate"~mg~L^-1)




#annual median per site####
annual_median_neon <- neon %>%
  mutate(year = year(DateTime)) %>%
  group_by(siteID, year, analyte) %>%
  mutate(median_concentration = median(analyteConcentration)) %>%
  ungroup() %>%
  select(-analyteConcentration, -X, -sampleCondition) %>%
  distinct() %>%
  pivot_wider(id_cols = c(siteID, DateTime), names_from = analyte, values_from = median_concentration) %>%
  rename(no2no3 = "NO3+NO2 - N") 


ggplot(annual_median_neon) +
  geom_point(aes(DOC, no2no3, color = TP)) +
  #scale_y_log10() +
  scale_color_viridis_c(direction = -1, "Total Phosphorus"~mg~L^-1) +
  theme_bw() +
  labs(x = "DOC"~mg~L^-1,
       y = "Nitrate"~mg~L^-1)
