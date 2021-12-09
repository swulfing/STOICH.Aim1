#script to read in, format, and save data from Canadian sites (had to be downloaded as individual csv files)

library(tidyverse)
library(data.table)

getwd()
#temporarily set working directory to file path
setwd("C:/Users/lrock1/OneDrive - University of Wyoming/PhD_code/STOICH.Aim1/Data/Natural_Resources_Canada_Data")

#create a list of the files from your target directory
file_list <- list.files(path="C:/Users/lrock1/OneDrive - University of Wyoming/PhD_code/STOICH.Aim1/Data/Natural_Resources_Canada_Data")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
chems_dataset <- data.frame()

#had to specify columns to get rid of the total column
for (i in 1:length(file_list)){
  temp_chems_data <- fread(file_list[i], stringsAsFactors = F) %>% #read in files using the fread function from the data.table package
  chems_dataset <- rbindlist(list(chems_dataset, temp_chems_data), use.names = T, fill = TRUE) #for each iteration, bind the new data to the building dataset
}

params <- as.data.frame(unique(chems_dataset$VARIABLE))
units <- as.data.frame(unique(chems_dataset$UNIT_UNITÉ))

#set working directory back to the main project file
setwd("C:/Users/lrock1/OneDrive - University of Wyoming/PhD_code/STOICH.Aim1")

CA_data <- chems_dataset %>%
  select(SITE_NO, DATE_TIME_HEURE, FLAG_MARQUEUR, VALUE_VALEUR, UNIT_UNITE, UNIT_UNITÉ, `UNIT_UNITÃ‰`, VARIABLE) %>% #select the columns that we want
  filter(VARIABLE == "CARBON DISSOLVED ORGANIC" |
          VARIABLE == "NITROGEN DISSOLVED NO3 & NO2" |
           VARIABLE == "NITROGEN, NITRATE" |
           VARIABLE == "NITROGEN TOTAL NITRATE" |
           VARIABLE == "NITROGEN DISSOLVED NITRATE" |
           VARIABLE == "NITRITE AND NITRATE UNFILTERED" |
           VARIABLE == "NITRITE AND NITRATE (AS N)" |
           VARIABLE == "NITRITE & NITRATE UNFILTERED" |
           VARIABLE == "NITRATE AND NITRITE (AS N)" |
           VARIABLE == "NITRATE (AS N)" |
         VARIABLE == "PHOSPHORUS TOTAL") %>%
  mutate(UNIT = UNIT_UNITE,
         UNIT = ifelse(is.na(UNIT), UNIT_UNITÉ, UNIT),
         UNIT = ifelse(is.na(UNIT), `UNIT_UNITÃ‰`, UNIT))


params <- as.data.frame(unique(CA_data$VARIABLE))
units <- as.data.frame(unique(CA_data$UNIT))

CA_data1 <- CA_data %>%
  select(DATE_TIME_HEURE, SITE_NO, VALUE_VALEUR, UNIT, VARIABLE) %>%
  rename(DATE_COL = DATE_TIME_HEURE, 
         SITE_ID = SITE_NO) %>%
  mutate(UNIT = "mg/L") %>%
  mutate(VARIABLE = ifelse(VARIABLE == "NITROGEN DISSOLVED NO3 & NO2" |
                             VARIABLE == "NITROGEN, NITRATE" |
                             VARIABLE == "NITROGEN TOTAL NITRATE" |
                             VARIABLE == "NITROGEN DISSOLVED NITRATE" |
                             VARIABLE == "NITRITE AND NITRATE UNFILTERED" |
                             VARIABLE == "NITRITE AND NITRATE (AS N)" |
                             VARIABLE == "NITRITE & NITRATE UNFILTERED" |
                             VARIABLE == "NITRATE AND NITRITE (AS N)" |
                             VARIABLE == "NITRATE (AS N)", "NO3 as N", VARIABLE))


CA_data2 <- CA_data1 %>%
  pivot_wider(id_cols = c(DATE_COL, SITE_ID, UNIT), names_from = VARIABLE, values_from = VALUE_VALEUR)
