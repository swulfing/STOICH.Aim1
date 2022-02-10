##Run script to create master dataset for all variables 


library(tidyverse)
library(lubridate)


#call in the data & ensure it is ready to use#####
temp <- (read.csv("Data/other_vars_datasets/LAGOS.csv")) %>%
  rbind(read.csv("Data/other_vars_datasets/NEON.csv")) %>%
  rbind(read.csv("Data/other_vars_datasets/NRC.csv")) %>%
  rbind(read.csv("Data/other_vars_datasets/LTER.csv")) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  filter(year(DATE_COL) >= 2000) #get rid of any data pre 2000


EU <- read.csv("Data/other_vars_datasets/EU.csv") %>%
  mutate(DATE_COL = paste(DATE_COL, "-01-01", sep = "")) %>%
  mutate(DATE_COL = as.Date(DATE_COL))

All_CNP_VARS <- rbind(EU, temp) 
#This dataset contains all concurrently collected DOC, nitrate as N, and phosphate as P data. units of everything are mg/L

rm(EU)
rm(temp)

All_CNP_VARS <- All_CNP_VARS %>%
  filter(RESULT > 0)

#some information about the dataset
# number.sites.VARS <- nrow(unique(All_CNP_VARS %>% dplyr::select(SITE_ID, LAT, LON, ECO_TYPE) %>%
#                               distinct()))
# number.lakes.VARS <- nrow(unique(All_CNP_VARS %>% dplyr::select(SITE_ID, LAT, LON, ECO_TYPE) %>%
#                               distinct() %>% filter(ECO_TYPE == "Lake")))
# number.rivers.VARS <- nrow(unique(All_CNP_VARS %>% dplyr::select(SITE_ID, LAT, LON, ECO_TYPE) %>%
#                                distinct() %>% filter(ECO_TYPE != "Lake")))

# check.that.sites.are.unique.VARS <- All_CNP_VARS %>% 
#   dplyr::select(SITE_ID, LAT, LON, ECO_TYPE) %>%
#   distinct() %>%
#   count(SITE_ID)

Variables_available <- All_CNP_VARS |>
  select(VARIABLE) |>
  distinct()




