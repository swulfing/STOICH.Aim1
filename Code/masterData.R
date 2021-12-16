##Run script to create master dataset

library(tidyverse)


#call in the data & ensure it is ready to use#####
temp <- (read.csv("Data/Simplified_datasets_per_source/SIMPLE_LAGOS.csv")) %>%
  rbind(read.csv("Data/Simplified_datasets_per_source/SIMPLE_NEON.csv")) %>%
  rbind(read.csv("Data/Simplified_datasets_per_source/SIMPLE_NLA.csv")) %>%
  rbind(read.csv("Data/Simplified_datasets_per_source/SIMPLE_NRC.csv")) %>%
  rbind(read.csv("Data/Simplified_datasets_per_source/SIMPLE_LTER.csv")) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  filter(year(DATE_COL) >= 2000) #get rid of any data pre 2000


EU <- read.csv("Data/Simplified_datasets_per_source/SIMPLE_EU.csv") %>%
  mutate(DATE_COL = paste(DATE_COL, "-01-01", sep = "")) %>%
  mutate(DATE_COL = as.Date(DATE_COL))

ALL_CNP <- rbind(EU, temp) 
#This dataset contains all concurrently collected DOC, nitrate as N, and TP data. units of everything are mg/L

rm(EU)
rm(temp)


#some information about the dataset
number.sites <- nrow(unique(ALL_CNP %>% dplyr::select(SITE_ID, LAT, LON, ECO_TYPE) %>%
                               distinct()))
number.lakes <- nrow(unique(ALL_CNP %>% dplyr::select(SITE_ID, LAT, LON, ECO_TYPE) %>%
                               distinct() %>% filter(ECO_TYPE == "Lake")))
number.rivers <- nrow(unique(ALL_CNP %>% dplyr::select(SITE_ID, LAT, LON, ECO_TYPE) %>%
                              distinct() %>% filter(ECO_TYPE != "Lake")))

check.that.sites.are.unique <- ALL_CNP %>% 
  dplyr::select(SITE_ID, LAT, LON, ECO_TYPE) %>%
  distinct() %>%
  count(SITE_ID)

observations.per.site <- ALL_CNP %>%
  count(SITE_ID)

stats.per.site <- ALL_CNP %>%
  group_by(SITE_ID) %>%
  summarise(median.DOC = median(DOC),
            median.NO3 = median(NO3.as.N),
            median.TP = median(TP),
            mean.DOC = mean(DOC),
            mean.NO3 = mean(NO3.as.N),
            mean.TP = mean(TP),
            sd.DOC = sd(DOC),
            sd.NO3 = sd(NO3.as.N),
            sd.TP = sd(TP))
