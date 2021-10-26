#script that reads in files (stored on LAR's computer - not GitHub) and concatenate it all into a single .rds

library(tidyverse)
library(data.table)

setwd("C:/Users/lrock1/OneDrive - University of Wyoming/NEON_all_data")

#create a list of the files from your target directory
file_list <- list.files(path="C:/Users/lrock1/OneDrive - University of Wyoming/NEON_chems_data")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
chems_dataset <- data.frame()

#had to specify columns to get rid of the total column
for (i in 1:length(file_list)){
  neon_chems_data <- fread(file_list[i], stringsAsFactors = F) #read in files using the fread function from the data.table package
  chems_dataset <- rbindlist(list(chems_dataset, neon_data), use.names = T) #for each iteration, bind the new data to the building dataset
}




#set wd back to project

write.csv(chems_dataset, "Data/NEON/ALL_SURFACE_WATER_CHEMS_DATA.csv")


parameters <- chems_dataset %>% select(analyte) %>% distinct()
conditions <- chems_dataset %>% select(sampleCondition) %>% distinct()

chems_dataset1 <- chems_dataset %>%
  mutate(DateTime = as.POSIXct(collectDate, tz = "UTC", "%Y-%m-%dT%H:%M")) %>%
  filter(analyte == "DOC" |
           analyte == "NO3+NO2 - N" |
           analyte == "TP") %>%
  select(siteID, domainID, namedLocation, DateTime, analyte, analyteConcentration, analyteUnits, sampleCondition) %>%
  filter(!is.na(analyteConcentration),
         analyteConcentration > 0)

sites <- chems_dataset1 %>% select(siteID) %>% distinct()
  
