#script that reads in files (stored on LAR's computer - not GitHub) and concatenate it all into a single file -- final data files saved on GitHub

library(tidyverse)
library(data.table)


#surface water chemistry data####
# setwd("C:/Users/lrock1/OneDrive - University of Wyoming/NEON_chems_data")
# 
# #create a list of the files from your target directory
# file_list <- list.files(path="C:/Users/lrock1/OneDrive - University of Wyoming/NEON_chems_data")
# 
# #initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
# chems_dataset <- data.frame()
# 
# #had to specify columns to get rid of the total column
# for (i in 1:length(file_list)){
#   neon_chems_data <- fread(file_list[i], stringsAsFactors = F) #read in files using the fread function from the data.table package
#   chems_dataset <- rbindlist(list(chems_dataset, neon_chems_data), use.names = T) #for each iteration, bind the new data to the building dataset
# }
# 
# 
# 
# #NEON surface water locations spatial data####
# setwd("C:/Users/lrock1/OneDrive - University of Wyoming/NEON_site_data")
# 
# #create a list of the files from your target directory
# file_list <- list.files(path="C:/Users/lrock1/OneDrive - University of Wyoming/NEON_site_data")
# 
# #initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
# site_dataset <- data.frame()
# 
# #had to specify columns to get rid of the total column
# for (i in 1:length(file_list)){
#   neon_site_data <- fread(file_list[i], stringsAsFactors = F) #read in files using the fread function from the data.table package
#   site_dataset <- rbindlist(list(site_dataset, neon_site_data), use.names = T) #for each iteration, bind the new data to the building dataset
# }

##Format Data####

#set wd back to project

#save raw, full datasets
# write.csv(chems_dataset, "Data/NEON/RAW_SURFACE_WATER_CHEMS_DATA.csv")
# write.csv(site_dataset, "Data/NEON/RAW_NEON_SPATIAL.csv")


chems_dataset <- read.csv("Data/NEON/RAW_SURFACE_WATER_CHEMS_DATA.csv")
site_dataset <- read.csv("Data/NEON/RAW_NEON_SPATIAL.csv")


#simplifying chemistry data
parameters <- chems_dataset %>% select(analyte) %>% distinct()
conditions <- chems_dataset %>% select(sampleCondition) %>% distinct()
sites <- chems_dataset1 %>% select(siteID) %>% distinct()

chems_dataset1 <- chems_dataset %>%
  mutate(DateTime = as.POSIXct(collectDate, tz = "UTC", "%Y-%m-%dT%H:%M")) %>%
  filter(analyte == "DOC" |
           analyte == "NO3+NO2 - N" |
           analyte == "Ortho - P") %>%
  select(siteID, domainID, namedLocation, DateTime, analyte, analyteConcentration, analyteUnits, sampleCondition) %>%
  filter(!is.na(analyteConcentration),
         analyteConcentration > 0)


  
#simplifying location data
sites <- site_dataset %>%
  select(domainID, siteID, namedLocation, decimalLatitude, decimalLongitude, elevation, aquaticSiteType) %>%
  distinct()



######################Fixing the data##############################################
#NEON <- read.csv("Data/NEON/SIMPLE_SURFACE_WATER_CHEMS_DATA.csv")
NEON <- chems_dataset1

condition <- as.data.frame(unique(NEON$sampleCondition)) #all ok
units <- as.data.frame(unique(NEON$analyteUnits)) #mg/L

neon.a <- NEON %>%
  mutate(siteID = ifelse(namedLocation == "TOOK.AOS.outlet", "TOOK.outlet", siteID),
         siteID = ifelse(namedLocation == "TOOK.AOS.inlet", "TOOK.inlet", siteID),
         siteID = ifelse(namedLocation == "TOOK.AOS.buoy.c1" |
                           namedLocation == "TOOK.AOS.buoy.c2" |
                           namedLocation == "TOOK.AOS.buoy.c0", "TOOK.buoy", siteID))

NEON.1 <- neon.a %>%
  dplyr::select(siteID, DateTime, analyte, analyteConcentration, analyteUnits) %>%
  rename(SITE_ID = siteID) %>%
  mutate(DATE_COL = as.Date(DateTime)) %>%
  group_by(SITE_ID, analyte, DATE_COL) %>%
  summarise(RESULT = mean(analyteConcentration)) %>%
  ungroup() 


#data ready to pair
NEON.2 <- NEON.1 %>%
  pivot_wider(id_cols = c(SITE_ID, DATE_COL), names_from = analyte, values_from = RESULT) %>%
  mutate(UNITS = "mg/L") %>%
  rename(`NO3 as N` = `NO3+NO2 - N`,
         `PO4 as P` = `Ortho - P`) %>%
  drop_na() 

#sites
sites <- sites

sites.a <-sites %>%
  dplyr::select(-namedLocation, -elevation, -domainID) %>%
  rename(SITE_ID = siteID,
         LAT = decimalLatitude,
         LON = decimalLongitude,
         ECO_TYPE = aquaticSiteType) %>%
  mutate(ECO_TYPE = ifelse(ECO_TYPE == "stream" |
                             ECO_TYPE == "river", "River/Stream", "Lake"))


combine <- left_join(NEON.2, sites.a)

check <- combine |>
  select(SITE_ID) |>
  distinct()

write.csv(combine, "Data/Simplified_datasets_per_source/SIMPLE_NEON.csv")

###############################################################################
###############################################################################
#other variables

chems_all <- chems_dataset |>
  mutate(DateTime = as.POSIXct(collectDate, tz = "UTC", "%Y-%m-%dT%H:%M")) %>%
  select(siteID, domainID, namedLocation, DateTime, analyte, analyteConcentration, analyteUnits, sampleCondition) %>%
  filter(!is.na(analyteConcentration),
         analyteConcentration > 0) |>
  mutate(siteID = ifelse(namedLocation == "TOOK.AOS.outlet", "TOOK.outlet", siteID),
         siteID = ifelse(namedLocation == "TOOK.AOS.inlet", "TOOK.inlet", siteID),
         siteID = ifelse(namedLocation == "TOOK.AOS.buoy.c1" |
                           namedLocation == "TOOK.AOS.buoy.c2" |
                           namedLocation == "TOOK.AOS.buoy.c0", "TOOK.buoy", siteID)) |>
  dplyr::select(siteID, DateTime, analyte, analyteConcentration, analyteUnits) %>%
  rename(SITE_ID = siteID,
         VARIABLE = analyte,
         RESULT = analyteConcentration,
         UNIT = analyteUnits) %>%
  mutate(DATE_COL = as.Date(DateTime)) %>%
  group_by(SITE_ID, VARIABLE, DATE_COL, UNIT) %>%
  summarise(RESULT = mean(RESULT)) %>%
  ungroup() |>
  mutate(VARIABLE = ifelse(VARIABLE == "NO3+NO2 - N", "NO3 as N", VARIABLE),
         VARIABLE = ifelse(VARIABLE == "Ortho - P",  "PO4 as P", VARIABLE)) |>
  left_join(sites.a)

write.csv(chems_all, 'Data/other_vars_datasets/NEON.csv')
