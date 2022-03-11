#script to standardize data from EIDC

library(tidyverse)
library(lubridate)
  
#read in conwy data
conwy <- read.csv('Data/EIDC/Conwy.csv')

#select appropriate columns and keep 
conwy1 <- conwy |>
  select(1:3, Ca, Mg, Na, Mn, K, SO4, Cl, NO3, NH4, NO2, DOC, pH, Total_N, PO4_P, Conductivity, POC, TDP) |>
  rename(CALCIUM = Ca, 
         MAGNESIUM = Mg,
         MANGANESE = Mn,
         SODIUM = Na, 
         POTASSIUM = K, 
         SULFATE = SO4,
         CHLORIDE = Cl,
         `NO3 as N` = NO3,
         `NH4 as N` = NH4,
         `NO2 as N` = NO2, 
         TN = Total_N, 
         `PO4 as P` = PO4_P,
         SP_COND = Conductivity, 
         TOC = POC, 
         TN = Total_N) |>
  drop_na(`NO3 as N`, `PO4 as P`, DOC)


conwy2 <- conwy1 |>
  mutate(SITE_ID = paste(Site_Code, Master_Site, sep = "_")) |>
  pivot_longer(4:20, names_to = 'VARIABLE', values_to = 'RESULT') |>
  drop_na() |>
  mutate(UNIT = ifelse(VARIABLE %in% c('CALCIUM', 'CHLORIDE', 'SODIUM', 'DOC', 'POTASSIUM', 'MAGNESIUM', 'MANGANESE', 'SULFATE', 'TN', 'TDP', 'TOC', 'PO4 as P', 'NO3 as N', 'NH4 as N', 'NO2 as N'), 'mg/L', NA),
         UNIT = ifelse(VARIABLE == 'pH', 'Unitless', UNIT),
         UNIT = ifelse(VARIABLE == 'SP_COND', 'uS/cm', UNIT)) |>
  mutate(RESULT = ifelse(VARIABLE == 'NH4 as N', RESULT * (18.04/1000) * (14.0067/18.04), RESULT), #convert NH4 to NH4 as N from ueq/L to mg/L
         RESULT = ifelse(VARIABLE == 'NO2 as N', RESULT * (46.005/1000), RESULT), #convert ueq/L NO2 as N to mg/L
         RESULT = ifelse(VARIABLE == 'NO3 as N', RESULT * (62.0049/1000), RESULT)) #convert ueq/L NO3 as N to mg/L

#get site info
conwy_sites <- readxl::read_xlsx('Data/Metadata/EIDC_metadata/Conwy/site_locations.xlsx')

#add site info to dataset -- final conwy for other vars
conwy3 <- conwy2 |>
  left_join(conwy_sites) |>
  rename(LAT = Latitude,
         LON = Longitude) |>
  mutate(ECO_TYPE = 'River/Stream') |>
  mutate(DATE_COL = as.Date(Date, format = "%d-%b-%y")) |>
  select(-c(1:3))

#final for conwy simple
conwy_simple <- conwy3 |>
  filter(VARIABLE %in% c('DOC', 'NO3 as N', 'PO4 as P')) |>
  rename(UNITS = UNIT) |>
  pivot_wider(names_from = 'VARIABLE', values_from = 'RESULT')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#ECN data
ECN <- read.csv('Data/EIDC/ECN_WC1.csv')

#filter for params
ECN1 <- ECN |>
  filter(FIELDNAME %in% c('CALCIUM', 'CHLORIDE', 'CONDY', 'IRON', 'MAGNESIUM', 'NH4N', 'NO3N', 'PH', 'PO4P', 'POTASSIUM', 'SO4S', 'SODIUM', 'DOC', 'TOTALP', 'TOTALN')) |>
  mutate(FIELDNAME = ifelse(FIELDNAME == 'CONDY', 'SP_COND', FIELDNAME),
         FIELDNAME = ifelse(FIELDNAME == 'NH4N', 'NH4 as N', FIELDNAME),
         FIELDNAME = ifelse(FIELDNAME == 'NO3N', 'NO3 as N', FIELDNAME),
         FIELDNAME = ifelse(FIELDNAME == 'PH', 'pH', FIELDNAME),
         FIELDNAME = ifelse(FIELDNAME == 'PO4P', 'PO4 as P', FIELDNAME),
         FIELDNAME = ifelse(FIELDNAME == 'SO4S', 'SULFATE', FIELDNAME),
         FIELDNAME = ifelse(FIELDNAME == 'TOTALN', 'TN', FIELDNAME),
         FIELDNAME = if_else(FIELDNAME == 'TOTALP', 'TP', FIELDNAME)) |>
  pivot_wider(names_from = 'FIELDNAME', values_from = 'VALUE') |>
  drop_na(`NO3 as N`, `PO4 as P`, DOC) |>
  pivot_longer(4:18, names_to = 'VARIABLE', values_to = 'RESULT') |>
  mutate(SITE_ID = paste('ECN', SITECODE, LCODE, sep = '_')) |>
  mutate(ECO_TYPE = 'River/Stream') |>
  mutate(DATE_COL = as.Date(SDATE, format = "%d-%b-%y")) |>
  filter(year(DATE_COL) > 1999) |>
  mutate(UNIT = 'mg/L') |>
  mutate(UNIT = ifelse(VARIABLE == 'pH', 'Unitless', UNIT),
         UNIT = ifelse(VARIABLE == 'SP_COND', 'uS/cm', UNIT)) |>
  distinct() |>
  drop_na()
  

ecn_sites <- read.csv('Data/Metadata/EIDC_metadata/ECN/ECN_sitelocations.csv')

#final ECN all vars dataset
ECN2 <- ECN1 |>
  left_join(ecn_sites) |>
  select(-c(1:3))


#final for ECN simple
ecn_simple <- ECN2 |>
  filter(VARIABLE %in% c('DOC', 'NO3 as N', 'PO4 as P')) |>
  rename(UNITS = UNIT) |>
  pivot_wider(names_from = 'VARIABLE', values_from = 'RESULT')

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#write datasets and save them!!

final_EIDC_allvars <- rbind(ECN2, conwy3)

final_EIDC_simple <- rbind(ecn_simple, conwy_simple)


write.csv(final_EIDC_allvars, 'Data/other_vars_datasets/EIDC.csv')

write.csv(final_EIDC_simple, 'Data/Simplified_datasets_per_source/SIMPLE_EIDC.csv')
