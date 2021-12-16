library(tidyverse)
library(lubridate)

#call in the data & ensure it is ready to use#####
temp <- (read.csv("Data/Simplified_datasets_per_source/SIMPLE_LAGOS.csv")) %>%
  rbind(read.csv("Data/Simplified_datasets_per_source/SIMPLE_NEON.csv")) %>%
  rbind(read.csv("Data/Simplified_datasets_per_source/SIMPLE_NLA.csv")) %>%
  rbind(read.csv("Data/Simplified_datasets_per_source/SIMPLE_NRC.csv")) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  filter(year(DATE_COL) >= 2000) #get rid of any data pre 2000


EU <- read.csv("Data/Simplified_datasets_per_source/SIMPLE_EU.csv") %>%
  mutate(DATE_COL = paste(DATE_COL, "-01-01", sep = "")) %>%
  mutate(DATE_COL = as.Date(DATE_COL))

ALL_CNP <- rbind(EU, temp)




#plotting all the data#####
ggplot(ALL_CNP) +
  geom_point(aes(DOC, NO3.as.N, color = TP * 1000)) +
  scale_color_viridis_c("TP"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "All data",
    x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))
     

ggplot(ALL_CNP %>% filter(ECO_TYPE == "Lake")) +
  geom_point(aes(DOC, NO3.as.N, color = TP * 1000)) +
  scale_color_viridis_c("TP"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "Lakes - all data",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))


ggplot(ALL_CNP %>% filter(ECO_TYPE != "Lake")) +
  geom_point(aes(DOC, NO3.as.N, color = TP * 1000)) +
  scale_color_viridis_c("TP"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "Rivers/Streams - all data",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))



#medians per year#####

by_year <- ALL_CNP %>%
  group_by(SITE_ID, year(DATE_COL), ECO_TYPE) %>%
  summarise(m.DOC = median(DOC),
            m.NO3 = median(NO3.as.N),
            m.TP = median(TP))
  
ggplot(by_year) +
  geom_point(aes(m.DOC, m.NO3, color = m.TP * 1000)) +
  scale_color_viridis_c("TP"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "Yearly medians",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))



ggplot(by_year %>% filter(ECO_TYPE == "Lake")) +
  geom_point(aes(m.DOC, m.NO3, color = m.TP * 1000)) +
  scale_color_viridis_c("TP"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "Lakes - yearly medians",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))


ggplot(by_year %>% filter(ECO_TYPE != "Lake")) +
  geom_point(aes(m.DOC, m.NO3, color = m.TP * 1000)) +
  scale_color_viridis_c("TP"~(mu~g~L^-1)) +
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
            m.TP = median(TP))

ggplot(by_decade) +
  geom_point(aes(m.DOC, m.NO3, color = m.TP * 1000)) +
  scale_color_viridis_c("TP"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "Decadal medians",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))



ggplot(by_decade %>% filter(ECO_TYPE == "Lake")) +
  geom_point(aes(m.DOC, m.NO3, color = m.TP * 1000)) +
  scale_color_viridis_c("TP"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "Lakes - decadal medians",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))


ggplot(by_decade %>% filter(ECO_TYPE != "Lake")) +
  geom_point(aes(m.DOC, m.NO3, color = m.TP * 1000)) +
  scale_color_viridis_c("TP"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "Rivers/Streams - yearly medians",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))  


