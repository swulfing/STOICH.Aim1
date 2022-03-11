##Run script to create master dataset

library(tidyverse)
library(lubridate)


#call in the data & ensure it is ready to use#####
temp <- (read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/Simplified_datasets_per_source/SIMPLE_LAGOS.csv")) %>%
  
  rbind(read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/Simplified_datasets_per_source/SIMPLE_NEON.csv")) %>%
  
  rbind(read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/Simplified_datasets_per_source/SIMPLE_NRC.csv")) %>%
  
  rbind(read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/Simplified_datasets_per_source/SIMPLE_LTER.csv")) %>%
  
  rbind(read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/Simplified_datasets_per_source/SIMPLE_EIDC.csv"))
  
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  
  filter(year(DATE_COL) >= 2000) #get rid of any data pre 2000





EU <- read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/Simplified_datasets_per_source/SIMPLE_EU.csv") %>%
  
  mutate(DATE_COL = paste(DATE_COL, "-01-01", sep = "")) %>%
  
  mutate(DATE_COL = as.Date(DATE_COL))


ALL_CNP <- rbind(EU, temp) 
#This dataset contains all concurrently collected DOC, nitrate as N, and phosphate as P data. units of everything are mg/L

rm(EU)
rm(temp)

ALL_CNP <- ALL_CNP %>%
  filter(NO3.as.N >0,
         PO4.as.P > 0,
         DOC >0 )

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
            median.PO4.as.P = median(PO4.as.P),
            mean.DOC = mean(DOC),
            mean.NO3 = mean(NO3.as.N),
            mean.PO4.as.P = mean(PO4.as.P),
            sd.DOC = sd(DOC),
            sd.NO3 = sd(NO3.as.N),
            sd.PO4.as.P = sd(PO4.as.P))

