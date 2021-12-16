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