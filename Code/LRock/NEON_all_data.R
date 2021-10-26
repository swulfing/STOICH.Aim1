#script that reads in files (stored on LAR's computer - not GitHub) and concatenate it all into a single file -- final data files saved on GitHub

library(tidyverse)
library(data.table)


#surface water chemistry data####
setwd("C:/Users/lrock1/OneDrive - University of Wyoming/NEON_chems_data")

#create a list of the files from your target directory
file_list <- list.files(path="C:/Users/lrock1/OneDrive - University of Wyoming/NEON_chems_data")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
chems_dataset <- data.frame()

#had to specify columns to get rid of the total column
for (i in 1:length(file_list)){
  neon_chems_data <- fread(file_list[i], stringsAsFactors = F) #read in files using the fread function from the data.table package
  chems_dataset <- rbindlist(list(chems_dataset, neon_chems_data), use.names = T) #for each iteration, bind the new data to the building dataset
}


# #nitrate in surface water data####
# setwd("C:/Users/lrock1/OneDrive - University of Wyoming/NEON_nitrate_data")
# 
# #create a list of the files from your target directory
# file_list <- list.files(path="C:/Users/lrock1/OneDrive - University of Wyoming/NEON_nitrate_data")
# 
# #initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
# nitrate_dataset <- data.frame()
# 
# #had to specify columns to get rid of the total column
# for (i in 1:length(file_list)){
#   neon_nitrate_data <- fread(file_list[i], stringsAsFactors = F) #read in files using the fread function from the data.table package
#   nitrate_dataset <- rbindlist(list(nitrate_dataset, neon_nitrate_data), use.names = T) #for each iteration, bind the new data to the building dataset
# }


#NEON surface water locations spatial data####
setwd("C:/Users/lrock1/OneDrive - University of Wyoming/NEON_site_data")

#create a list of the files from your target directory
file_list <- list.files(path="C:/Users/lrock1/OneDrive - University of Wyoming/NEON_site_data")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
site_dataset <- data.frame()

#had to specify columns to get rid of the total column
for (i in 1:length(file_list)){
  neon_site_data <- fread(file_list[i], stringsAsFactors = F) #read in files using the fread function from the data.table package
  site_dataset <- rbindlist(list(site_dataset, neon_site_data), use.names = T) #for each iteration, bind the new data to the building dataset
}

##Format Data####

#set wd back to project

#save raw, full datasets
write.csv(chems_dataset, "Data/NEON/RAW_SURFACE_WATER_CHEMS_DATA.csv")
write.csv(site_dataset, "Data/NEON/RAW_NEON_SPATIAL.csv")



#simplifying chemistry data
parameters <- chems_dataset %>% select(analyte) %>% distinct()
conditions <- chems_dataset %>% select(sampleCondition) %>% distinct()
sites <- chems_dataset1 %>% select(siteID) %>% distinct()

chems_dataset1 <- chems_dataset %>%
  mutate(DateTime = as.POSIXct(collectDate, tz = "UTC", "%Y-%m-%dT%H:%M")) %>%
  filter(analyte == "DOC" |
           analyte == "NO3+NO2 - N" |
           analyte == "TP") %>%
  select(siteID, domainID, namedLocation, DateTime, analyte, analyteConcentration, analyteUnits, sampleCondition) %>%
  filter(!is.na(analyteConcentration),
         analyteConcentration > 0)


  
#simplifying location data
sites <- site_dataset %>%
  select(domainID, siteID, namedLocation, decimalLatitude, decimalLongitude, elevation, aquaticSiteType) %>%
  distinct()

write.csv(sites, "Data/NEON/SIMPLE_NEON_SPATIAL.csv")



