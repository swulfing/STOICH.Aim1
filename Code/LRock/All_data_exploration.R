library(tidyverse)
library(lubridate)



#call in data:
source("Code/masterData.R")

PO4_categories <- ALL_CNP |>
  mutate(PO4.as.P_limit = NA) |>
  mutate(PO4.as.P_limit = ifelse(PO4.as.P < 0.05, "natural", 
                                 ifelse(between(PO4.as.P, 0.05, 0.1), "limit",
                                        #ifelse(between(PO4.as.P, 0.1, 0.5), "medium", 
                                               ifelse(PO4.as.P > 0.1, "high", PO4.as.P_limit))))
#plotting limit

ggplot(PO4_categories) +
  geom_point(aes(DOC, NO3.as.N, color = PO4.as.P_limit)) +
  scale_color_viridis_d(expression(Posphorus - PO[4]~(mg~L^-1))) +
  theme_bw() +
  labs(title = "All data",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))+
  scale_y_log10()

#plotting all the data#####
ggplot(ALL_CNP) +
  geom_point(aes(DOC, NO3.as.N, color = PO4.as.P)) +
  scale_color_viridis_c("PO4"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "All data",
    x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1))) +
  scale_y_log10()
     

ggplot(ALL_CNP %>% filter(ECO_TYPE == "Lake")) +
  geom_point(aes(DOC, NO3.as.N, color = PO4.as.P * 1000)) +
  scale_color_viridis_c("PO4.as.P"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "Lakes - all data",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))


ggplot(ALL_CNP %>% filter(ECO_TYPE != "Lake")) +
  geom_point(aes(DOC, NO3.as.N, color = PO4.as.P * 1000)) +
  scale_color_viridis_c("PO4.as.P"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "Rivers/Streams - all data",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))



#medians per year#####

by_year <- ALL_CNP %>%
  group_by(SITE_ID, year(DATE_COL), ECO_TYPE) %>%
  summarise(m.DOC = median(DOC),
            m.NO3 = median(NO3.as.N),
            m.PO4.as.P = median(PO4.as.P))
  
ggplot(by_year) +
  geom_point(aes(m.DOC, m.NO3, color = m.PO4.as.P * 1000)) +
  scale_color_viridis_c("PO4.as.P"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "Yearly medians",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))



ggplot(by_year %>% filter(ECO_TYPE == "Lake")) +
  geom_point(aes(m.DOC, m.NO3, color = m.PO4.as.P * 1000)) +
  scale_color_viridis_c("PO4.as.P"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "Lakes - yearly medians",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))


ggplot(by_year %>% filter(ECO_TYPE != "Lake")) +
  geom_point(aes(m.DOC, m.NO3, color = m.PO4.as.P * 1000)) +
  scale_color_viridis_c("PO4.as.P"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "Rivers/Streams - yearly medians",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))  



#medians per decade#####

by_decade<- ALL_CNP %>%
  mutate(DECADE = ifelse(between(year(DATE_COL), 2000, 2009), 2000, 2010)) %>%
  group_by(SITE_ID, DECADE, ECO_TYPE) %>%
  summarise(m.DOC = median(DOC),
            m.NO3 = median(NO3.as.N),
            m.PO4.as.P = median(PO4.as.P))

ggplot(by_decade) +
  geom_point(aes(m.DOC, m.NO3, color = m.PO4.as.P * 1000)) +
  scale_color_viridis_c("PO4.as.P"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "Decadal medians",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))



ggplot(by_decade %>% filter(ECO_TYPE == "Lake")) +
  geom_point(aes(m.DOC, m.NO3, color = m.PO4.as.P * 1000)) +
  scale_color_viridis_c("PO4.as.P"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "Lakes - decadal medians",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))


ggplot(by_decade %>% filter(ECO_TYPE != "Lake")) +
  geom_point(aes(m.DOC, m.NO3, color = m.PO4.as.P * 1000)) +
  scale_color_viridis_c("PO4.as.P"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "Rivers/Streams - yearly medians",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))  


