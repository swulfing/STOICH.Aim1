## Trying to linearize data

library(tidyverse)
library(lubridate)



#call in data:
source("Code/masterData.R")


#plotting all the data#####

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


#Plotting 1/Nitrate concentration
CNP_1overN <- ALL_CNP
CNP_1overN$NTransform <- (1/CNP_1overN$NO3.as.N)
CNP_1overN_HighTP <- filter(CNP_1overN, TP > 0.03)

ggplot(CNP_1overN %>% filter(ECO_TYPE != "Lake")) +
  geom_point(aes(DOC, NTransform, color = TP * 1000)) +
  scale_color_viridis_c("TP"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "Rivers/Streams - all data",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))


ggplot(CNP_1overN_HighTP %>% filter(ECO_TYPE != "Lake")) +
  geom_point(aes(DOC, NTransform, color = TP * 1000)) +
  scale_color_viridis_c("TP"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "Rivers/Streams - all data",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))

#plotting Log of nitrate concentration
CNP_logN <- ALL_CNP
CNP_logN$NTransform <- (log(CNP_logN$NO3.as.N))
CNP_logN_HighTP <- filter(CNP_logN, TP > 0.03)

ggplot(CNP_logN %>% filter(ECO_TYPE != "Lake")) +
  geom_point(aes(DOC, NTransform, color = TP * 1000), alpha = .5) +
  scale_color_viridis_c("TP"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "Rivers/Streams - all data",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))

#Supposed to remove low tp vals but doesn't quite look how I want yet
ggplot(CNP_logN_HighTP %>% filter(ECO_TYPE != "Lake")) +
  geom_point(aes(DOC, NTransform, color = TP * 1000)) +
  scale_color_viridis_c("TP"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "Rivers/Streams - all data",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))


#Now to see if any one dataset can be of help
#LAGOS
LAGOS <- (read.csv("Data/Simplified_datasets_per_source/SIMPLE_LAGOS.csv")) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  filter(year(DATE_COL) >= 2000)

LAGOS$NTransform <- (log(LAGOS$NO3.as.N))
LAGOS_HighTP <- filter(LAGOS, TP > 0.03)

ggplot(LAGOS) +
  geom_point(aes(DOC, NTransform, color = TP * 1000)) +
  scale_color_viridis_c("TP"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "LAGOS",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))

ggplot(LAGOS) +
  geom_point(aes(DOC, NO3.as.N , color = TP * 1000)) +
  scale_color_viridis_c("TP"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "LAGOS",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))

#NEON-Note: Rivers/streams has weird gap. of high conc. NO3. Could we isolate these points and see if anything sepcial about them
NEON <- (read.csv("Data/Simplified_datasets_per_source/SIMPLE_NEON.csv")) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  filter(year(DATE_COL) >= 2000)

NEON$NTransform <- (log(NEON$NO3.as.N))
NEON_HighTP <- filter(NEON, TP > 0.03)

ggplot(NEON %>% filter(ECO_TYPE != "Lake")) +
  geom_point(aes(DOC, NTransform, color = TP * 1000)) +
  scale_color_viridis_c("TP"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "NEON",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))

ggplot(NEON %>% filter(ECO_TYPE != "Lake")) +
  geom_point(aes(DOC, NO3.as.N , color = TP * 1000)) +
  scale_color_viridis_c("TP"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "NEON",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))

#NLA
NLA <- (read.csv("Data/Simplified_datasets_per_source/SIMPLE_NLA.csv")) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  filter(year(DATE_COL) >= 2000)

NLA$NTransform <- (log(NLA$NO3.as.N))
NLA_HighTP <- filter(NLA, TP > 0.03)

ggplot(NLA %>% filter(ECO_TYPE != "Lake")) +
  geom_point(aes(DOC, NTransform, color = TP * 1000)) +
  scale_color_viridis_c("TP"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "NLA",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))

ggplot(NLA %>% filter(ECO_TYPE != "Lake")) +
  geom_point(aes(DOC, NO3.as.N , color = TP * 1000)) +
  scale_color_viridis_c("TP"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "NLA",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))

#NRC
NRC <- (read.csv("Data/Simplified_datasets_per_source/SIMPLE_NRC.csv")) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  filter(year(DATE_COL) >= 2000)

NRC$NTransform <- (log(NRC$NO3.as.N))
NRC_HighTP <- filter(NRC, TP > 0.03)

ggplot(NRC %>% filter(ECO_TYPE != "Lake")) +
  geom_point(aes(DOC, NTransform, color = TP * 1000)) +
  scale_color_viridis_c("TP"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "NRC",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))

ggplot(NRC %>% filter(ECO_TYPE != "Lake")) +
  geom_point(aes(DOC, NO3.as.N , color = TP * 1000)) +
  scale_color_viridis_c("TP"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "NRC",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))

#LTER-Note: this one feels very scattered. look into further
LTER <- (read.csv("Data/Simplified_datasets_per_source/SIMPLE_LTER.csv")) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  filter(year(DATE_COL) >= 2000)

LTER$NTransform <- (log(LTER$NO3.as.N))
LTER_HighTP <- filter(LTER, TP > 0.03)

ggplot(LTER %>% filter(ECO_TYPE != "Lake")) +
  geom_point(aes(DOC, NTransform, color = TP * 1000)) +
  scale_color_viridis_c("TP"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "LTER",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))

ggplot(LTER %>% filter(ECO_TYPE != "Lake")) +
  geom_point(aes(DOC, NO3.as.N , color = TP * 1000)) +
  scale_color_viridis_c("TP"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "LTER",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))
