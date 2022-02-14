#####Load packages####

library(tidyverse)
library(LAGOSNE)
library(lubridate)

####The database#####

#https://lagoslakes.org/

#Download the LAGOS dataset - you only need to do this once on your computer. This can take a couple minutes.
##lagosne_get(dest_folder = lagos_path())

#Then load in the dataset. Do this everytime you want to use LAGOS
lagos <- lagosne_load()

#This next line will bring up the help pages for the package
#if you look in epi_nutri, you can see all the water quality parameters available
help.search("datasets", package = "LAGOSNE")

####Filter for DOC, Nitrate, TP data####

lagos_data <- lagos$epi_nutr %>%
  dplyr::select(lagoslakeid, sampledate, doc, no2no3, srp) %>% #assumption srp is po4
  drop_na() |> #only keeps sites/data where doc, nitrate, and srp were concurrently collected
  filter(year(sampledate) > 1999) #data from 2000 or later

lagos_data.1 <- lagos_data %>%
  rename(SITE_ID = lagoslakeid,
         DATE_COL = sampledate,
         DOC = doc,
         `NO3 as N` = no2no3,
         `PO4 as P` = srp) %>%
  mutate(DOC = DOC / 1000,
         `NO3 as N` = `NO3 as N` / 1000,
         `PO4 as P` = `PO4 as P` / 1000, 
         UNITS = "mg/L")



#site info
sites <- lagos$locus %>%
  dplyr::select(lagoslakeid, nhd_lat, nhd_long) %>%
  rename(SITE_ID = lagoslakeid,
         LAT = nhd_lat,
         LON = nhd_long) %>%
  mutate(ECO_TYPE = "Lake") 


combine <- left_join(lagos_data.1, sites) %>%
  distinct() %>%
  mutate(SITE_ID = paste("LAGOS_",SITE_ID, sep = ""))



write.csv(combine, "DATA/Simplified_datasets_per_source/SIMPLE_LAGOS.csv")

#how many sites?
check.1 <- combine |>
  select(SITE_ID, LAT, LON) |>
  distinct()


#find sources -- this is to double check other sources and ensure we do not have duplicate data
 sources <- lagos$lagos_source_program
 
 
 
 
 ################################################################################
 ################################################################################
 #create dataset with all variables but renamed DOC, PO4 as P, and NO3 as N
all_data <- lagos$epi_nutr |>
  dplyr::select(lagoslakeid, sampledate, chla, colora, colort, doc, dkn, nh4, no2, no2no3, srp, tdn, tdp, tkn, tn, toc, ton, tp, secchi) |>
  filter(year(sampledate) > 1999) |> #data from 2000 or later
  rename(SITE_ID = lagoslakeid,
         DATE_COL = sampledate,
         DOC = doc,
         `NO3 as N` = no2no3,
         `PO4 as P` = srp) |>
  drop_na(DOC, `PO4 as P`, `NO3 as N`) |> #only keeps sites/data where doc, nitrate, and srp were concurrently collected
  mutate(DOC = DOC / 1000,
          `NO3 as N` = `NO3 as N` / 1000,
          `PO4 as P` = `PO4 as P` / 1000) #convert from ug/L to mg/L
 
 all_data2 <- all_data |> 
  pivot_longer(3:ncol(all_data), names_to = "VARIABLE", values_to = "RESULT") |>
   left_join(sites) |>
   distinct() |>
   mutate(SITE_ID = paste("LAGOS_",SITE_ID, sep = "")) |>
   mutate(UNIT = ifelse(VARIABLE %in% c('DOC', 'NO3 as N', 'PO4 as P'), "mg/L", NA),
          UNIT = ifelse(VARIABLE %in% c('colort', 'colora'), 'PCU', UNIT),
          UNIT = ifelse(VARIABLE %in% c('chla'), 'ug/L', UNIT),
          UNIT = ifelse(VARIABLE %in% c('secchi'), 'm', UNIT),
          UNIT = ifelse(VARIABLE %in% c('dkn', 'nh4', 'no2', 'tdn', 'tkn', 'tn', 'ton'), 'ug/L as N', UNIT),
          UNIT = ifelse(VARIABLE %in% c('toc'), 'ug/L as C', UNIT),
          UNIT = ifelse(VARIABLE %in% c('tdp', 'tp'), 'ug/L as P', UNIT)) |>
   drop_na(RESULT) #get rid of any empty cells

 #just double check we have all the sites
 check.2 <- all_data2 |>
   select(SITE_ID, LAT, LON) |>
   distinct() #YAY! all 92 are here :) 
 
 write.csv(all_data2, "Data/other_vars_datasets/LAGOS.csv")
 