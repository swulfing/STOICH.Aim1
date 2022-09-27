#script to read in, format, and save data from Canadian sites (had to be downloaded as individual csv files)

library(tidyverse)
library(data.table)

getwd()


#create a list of the files from your target directory
file_list <- list.files(path="Data/Natural_Resources_Canada_Data/raw_data")
file_list <- paste("C:/Users/lrock1/Downloads/PhD_Code/STOICH.Aim1/Data/Natural_Resources_Canada_Data/raw_data/", file_list, sep = "")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
chems_dataset <- data.frame()

#had to specify columns to get rid of the total column
for (i in 1:length(file_list)){
  temp_chems_data <- fread(file_list[i], stringsAsFactors = F) #read in files using the fread function from the data.table package
  chems_dataset <- rbindlist(list(chems_dataset, temp_chems_data), use.names = T, fill = TRUE) #for each iteration, bind the new data to the building dataset
}


params <- as.data.frame(unique(chems_dataset$VARIABLE))
units <- as.data.frame(unique(chems_dataset$UNIT_UNITÉ))



#filter to keep only the necessary data and columns
CA_data <- chems_dataset %>%
  select(SITE_NO, DATE_TIME_HEURE, FLAG_MARQUEUR, VALUE_VALEUR, UNIT_UNITE, UNIT_UNITÉ, `UNIT_UNITÃ‰`, VARIABLE) %>% #select the columns that we want
  filter(VARIABLE == "CARBON DISSOLVED ORGANIC" |
          VARIABLE == "NITROGEN DISSOLVED NO3 & NO2" |
           VARIABLE == "NITROGEN, NITRATE" |
           VARIABLE == "NITROGEN TOTAL NITRATE" |
           VARIABLE == "NITROGEN DISSOLVED NITRATE" |
           #VARIABLE == "NITRITE AND NITRATE UNFILTERED" |
           VARIABLE == "NITRITE AND NITRATE (AS N)" |
           #VARIABLE == "NITRITE & NITRATE UNFILTERED" |
           VARIABLE == "NITRATE AND NITRITE (AS N)" |
           VARIABLE == "NITRATE (AS N)" |
           VARIABLE == "PHOSPHATE (AS P)" |
           VARIABLE == "ORTHOPHOSPHATE DISSOLVED") %>%
  mutate(UNIT = UNIT_UNITE,
         UNIT = ifelse(is.na(UNIT), UNIT_UNITÉ, UNIT),
         UNIT = ifelse(is.na(UNIT), `UNIT_UNITÃ‰`, UNIT))

#do I need to filter for mg/L as P or not?
check <- CA_data %>%
  filter(VARIABLE == "ORTHOPHOSPHATE DISSOLVED")
#Nope! all these are as P :) 

#check parameters and units
params <- as.data.frame(unique(CA_data$VARIABLE))
units <- as.data.frame(unique(CA_data$UNIT))



#renaming variables for continuity
CA_data1 <- CA_data %>%
  select(DATE_TIME_HEURE, SITE_NO, VALUE_VALEUR, UNIT, VARIABLE) %>%
  rename(DATE_COL = DATE_TIME_HEURE, 
         SITE_ID = SITE_NO) %>%
  mutate(UNIT = "mg/L") %>%
  mutate(VARIABLE = ifelse(VARIABLE == "NITROGEN DISSOLVED NO3 & NO2" |
                             VARIABLE == "NITROGEN, NITRATE" |
                             VARIABLE == "NITROGEN TOTAL NITRATE" |
                             VARIABLE == "NITROGEN DISSOLVED NITRATE" |
                             #VARIABLE == "NITRITE AND NITRATE UNFILTERED" |
                             VARIABLE == "NITRITE AND NITRATE (AS N)" |
                             #VARIABLE == "NITRITE & NITRATE UNFILTERED" |
                             VARIABLE == "NITRATE AND NITRITE (AS N)" |
                             VARIABLE == "NITRATE (AS N)", "NO3 as N", VARIABLE)) %>%
  mutate(VARIABLE = ifelse(VARIABLE == "PHOSPHATE (AS P)" |
                             VARIABLE == "ORTHOPHOSPHATE DISSOLVED", "PO4 as P", VARIABLE)) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  distinct() %>%
  drop_na() %>%
  group_by(DATE_COL, SITE_ID, VARIABLE) %>%
  summarise(RESULT = mean(VALUE_VALEUR)) %>%
  filter(RESULT > 0)



#pivot wider to get parameters in their own columns
CA_data2 <- CA_data1 %>%
  pivot_wider(id_cols = c(DATE_COL, SITE_ID), names_from = VARIABLE, values_from = RESULT) %>%
  drop_na() %>%
  mutate(UNITS = "mg/L") %>%
  rename(DOC = `CARBON DISSOLVED ORGANIC`)


#get site data
sites <- read.csv("Data/Natural_Resources_Canada_Data/Site_Data/Water-Qual-Eau-Sites-National.csv") 

sites1 <- sites %>%
  select(SITE_NO, LATITUDE, LONGITUDE, SITE_TYPE) %>%
  rename(SITE_ID = SITE_NO,
         LAT = LATITUDE,
         LON = LONGITUDE,
         ECO_TYPE = SITE_TYPE)


combined <- left_join(CA_data2, sites1)

types <- as.data.frame(unique(combined$ECO_TYPE))

combined <- combined%>%
  mutate(ECO_TYPE = "River/Stream") %>%
  ungroup

write.csv(combined, "Data/Simplified_datasets_per_source/SIMPLE_NRC.csv")

onlysites <- combined %>%
  select(SITE_ID, LAT, LON, ECO_TYPE) %>%
  distinct()

write.csv(onlysites, "Data/Natural_Resources_Canada_Data/Site_Data/UNIQUE_SITES.csv")


################################################################################
#create dataset with all variables but renamed DOC, PO4 as P, and NO3 as N

all_data <- chems_dataset %>%
  mutate(VARIABLE = ifelse(VARIABLE == "NITROGEN DISSOLVED NO3 & NO2" |
                             VARIABLE == "NITROGEN, NITRATE" |
                             VARIABLE == "NITROGEN TOTAL NITRATE" |
                             VARIABLE == "NITROGEN DISSOLVED NITRATE" |
                             #VARIABLE == "NITRITE AND NITRATE UNFILTERED" |
                             VARIABLE == "NITRITE AND NITRATE (AS N)" |
                             #VARIABLE == "NITRITE & NITRATE UNFILTERED" |
                             VARIABLE == "NITRATE AND NITRITE (AS N)" |
                             VARIABLE == "NITRATE (AS N)", "NO3 as N", VARIABLE)) %>%
  mutate(VARIABLE = ifelse(VARIABLE == "PHOSPHATE (AS P)" |
                             VARIABLE == "ORTHOPHOSPHATE DISSOLVED", "PO4 as P", VARIABLE)) %>%
  mutate(VARIABLE = ifelse(VARIABLE == "CARBON DISSOLVED ORGANIC", "DOC", VARIABLE)) %>%
  mutate(UNIT = UNIT_UNITE,
         UNIT = ifelse(is.na(UNIT), UNIT_UNITÉ, UNIT),
         UNIT = ifelse(is.na(UNIT), `UNIT_UNITÃ‰`, UNIT)) %>%
  select(DATE_TIME_HEURE, SITE_NO, VALUE_VALEUR, UNIT, VARIABLE) %>%
  rename(DATE_COL = DATE_TIME_HEURE, 
         SITE_ID = SITE_NO,
         RESULT = VALUE_VALEUR) %>%
  filter(RESULT > 0) %>%
  distinct() %>%
  drop_na() %>%
  group_by(DATE_COL, SITE_ID, VARIABLE, UNIT) %>%
  summarise(RESULT = mean(RESULT)) %>%
  ungroup() 
  

#call in sites data for sites that include DOC, NO3, and PO4
sites_w_cnp <- read.csv("Data/Natural_Resources_Canada_Data/Site_Data/UNIQUE_SITES.csv")
  
all_data2 <- all_data %>%
  left_join(sites_w_cnp) %>%
  drop_na(LAT) %>%
  mutate(UNIT = str_remove(all_data2$UNIT, "Â")) %>%
  select(-X)


write.csv(all_data2, "Data/other_vars_datasets/NRC.csv")


