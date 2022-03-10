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
# all_data <- lagos$epi_nutr |>
#   dplyr::select(lagoslakeid, sampledate, chla, colora, colort, doc, dkn, nh4, no2, no2no3, srp, tdn, tdp, tkn, tn, toc, ton, tp, secchi) |>
#   filter(year(sampledate) > 1999) |> #data from 2000 or later
#   rename(SITE_ID = lagoslakeid,
#          DATE_COL = sampledate,
#          DOC = doc,
#          `NO3 as N` = no2no3,
#          `PO4 as P` = srp) |>
#   drop_na(DOC, `PO4 as P`, `NO3 as N`) |> #only keeps sites/data where doc, nitrate, and srp were concurrently collected
#   mutate(DOC = DOC / 1000,
#           `NO3 as N` = `NO3 as N` / 1000,
#           `PO4 as P` = `PO4 as P` / 1000) #convert from ug/L to mg/L
#  
#  all_data2 <- all_data |> 
#   pivot_longer(3:ncol(all_data), names_to = "VARIABLE", values_to = "RESULT") |>
#    left_join(sites) |>
#    distinct() |>
#    mutate(SITE_ID = paste("LAGOS_",SITE_ID, sep = "")) |>
#    mutate(UNIT = ifelse(VARIABLE %in% c('DOC', 'NO3 as N', 'PO4 as P'), "mg/L", NA),
#           UNIT = ifelse(VARIABLE %in% c('colort', 'colora'), 'PCU', UNIT),
#           UNIT = ifelse(VARIABLE %in% c('chla'), 'ug/L', UNIT),
#           UNIT = ifelse(VARIABLE %in% c('secchi'), 'm', UNIT),
#           UNIT = ifelse(VARIABLE %in% c('dkn', 'nh4', 'no2', 'tdn', 'tkn', 'tn', 'ton'), 'ug/L as N', UNIT),
#           UNIT = ifelse(VARIABLE %in% c('toc'), 'ug/L as C', UNIT),
#           UNIT = ifelse(VARIABLE %in% c('tdp', 'tp'), 'ug/L as P', UNIT)) |>
#    drop_na(RESULT) #get rid of any empty cells
# 
#  #just double check we have all the sites
#  check.2 <- all_data2 |>
#    select(SITE_ID, LAT, LON) |>
#    distinct() #YAY! all 92 are here :) 
#  
#  write.csv(all_data2, "Data/other_vars_datasets/LAGOS.csv")
 
 
 
 
 
 
 ################################################################################
 ################################################################################
 #LAGOS-US data
 library(tidyverse)
 library(lubridate)
 
 
 #load LAGOS-US data (beta as of 02/14/22)
 lagos_nutrients <- read.csv("Data/LAGOS-US_beta/site_nutrientsalgae_epi.csv")
 lagos_carbon <- read.csv("Data/LAGOS-US_beta/site_claritycarbon_epi.csv")
 
 #subset to get DOC
 carbon <- lagos_carbon |>
   dplyr::select(1:10, 25)
 
 #subset to get NO3 and SRP 
 nutrients <- lagos_nutrients |>
   dplyr::select(1:10, 'no2no3n_ugl', 'srpp_ugl')
 
 #combine data
 lagos_data <- carbon |>
   left_join(nutrients) |>
   mutate(DATE_COL = as.Date(event_date)) |> #dates
   # bind_rows(lagosne1) |>
   filter(year(DATE_COL) > 1999) |> #limit to post 2000 data
   rename(SITE_ID = lagoslakeid, #renaming variables for consistency with other datasets
          DOC = doc_mgl,
          `NO3 as N` = no2no3n_ugl,
          `PO4 as P` = srpp_ugl) %>%
   mutate(`NO3 as N` = `NO3 as N` / 1000, #convert ug/L to mg/L
          `PO4 as P` = `PO4 as P` / 1000, 
          UNITS = "mg/L") |> 
   drop_na(DOC, `NO3 as N`, `PO4 as P`) |> #only keep concurrently collected DOC, nitrate, and phosphate
   mutate(SITE_ID = paste("LAGOS_",SITE_ID, sep = "")) |> #add "LAGOS" to site id
   rename(LAT = site_lat_decdeg,
          LON = site_lon_decdeg) |>
   mutate(ECO_TYPE = "Lake") |>
   select(SITE_ID, DATE_COL, DOC, `NO3 as N`, `PO4 as P`, UNITS, LAT, LON, ECO_TYPE)
 
 #now let's combine with the LAGOS-NE data and get rid of any overlaps
 lagos_NE <- read.csv("DATA/Simplified_datasets_per_source/SIMPLE_LAGOS.csv")
 
 
 full <- lagos_data |>
   rename(NO3.as.N = `NO3 as N`, #rename these so we can join the data
          PO4.as.P = `PO4 as P`) |>
   full_join(lagos_NE |>
               mutate(DATE_COL = as.Date(DATE_COL))) |>
   select(-X) |>
   distinct() #delete duplicates
 
 #check that sites are unique
 sites_check <- full |> 
   select(SITE_ID, LAT, LON) |>
   distinct() #221 lakes
 
 #double check
 check2 <- sites_check |>
   select(SITE_ID) |>
   distinct() #221!! Good! No weird lat, lons
 
 
 #overwrite previous LAGOS dataset
 write.csv(full, "DATA/Simplified_datasets_per_source/SIMPLE_LAGOS.csv")
 
 
 
 ################################################################################
 ################################################################################
 #create dataset with all variables but renamed DOC, PO4 as P, and NO3 as N
 all_C <- lagos_carbon |>
   rename(DOC = doc_mgl) |>
   drop_na(DOC) |> #get rid of any NAs right away
   select(lagoslakeid, event_date, site_lat_decdeg, site_lon_decdeg, DOC, colora_pcu, colort_pcu, turb_ntu, secchi_m, tss) |>
   mutate(SITE_ID = paste("LAGOS_",lagoslakeid, sep = ""),
          ECO_TYPE = 'Lake') |>
   rename(LAT = site_lat_decdeg,
          LON = site_lon_decdeg) |>
   mutate(DATE_COL = as.Date(event_date)) |>
   filter(year(DATE_COL) > 1999) |>
   pivot_longer(5:10, names_to = 'VARIABLE', values_to = 'RESULT') |>
   mutate(UNIT = ifelse(VARIABLE %in% c('DOC', 'tss'), 'mg/L', NA)) |>
   mutate(UNIT = ifelse(is.na(UNIT), str_extract(VARIABLE, '(?<=_).+'), UNIT)) |> #extract units from last part of variable names, e.g., secchi_m, extract 'm'
   drop_na(RESULT) |>
   select(-c(1:2)) |>
   filter(SITE_ID %in% full$SITE_ID) #only keep locations from the full simple dataset that inlcude all 3 DOC, nitrate, phosphate

 
 all_nutrients <- lagos_nutrients |>
   select(lagoslakeid, event_date, site_lat_decdeg, site_lon_decdeg, chla_ugl, no2no3n_ugl, srpp_ugl, nh4n_ugl, tkn_ugl, tn_ugl, ton_ugl, tp_ugl, microcystin_ugl) |>
   mutate(DATE_COL = as.Date(event_date)) |> #dates
   filter(year(DATE_COL) > 1999) |> #limit to post 2000 data
   rename(SITE_ID = lagoslakeid, #renaming variables for consistency with other datasets
          `NO3 as N` = no2no3n_ugl,
          `PO4 as P` = srpp_ugl) %>%
   mutate(`NO3 as N` = `NO3 as N` / 1000, #convert ug/L to mg/L
          `PO4 as P` = `PO4 as P` / 1000) |> 
   drop_na(`NO3 as N`, `PO4 as P`) |> #only keep concurrently collected nitrate, and phosphate
   mutate(SITE_ID = paste("LAGOS_",SITE_ID, sep = "")) |> #add "LAGOS" to site id
   rename(LAT = site_lat_decdeg,
          LON = site_lon_decdeg) |>
   mutate(ECO_TYPE = "Lake") |>
   select(-event_date) |>
   pivot_longer(4:12, names_to = "VARIABLE", values_to = "RESULT") |>
   drop_na(RESULT) |>
   mutate(UNIT = ifelse(VARIABLE %in% c('NO3 as N', 'PO4 as P'), 'mg/L', NA)) |>
   mutate(UNIT = ifelse(is.na(UNIT), str_extract(VARIABLE, '(?<=_).+'), UNIT)) |>#extract units from last part of variable names, e.g., secchi_m, extract 'm'
   filter(SITE_ID %in% full$SITE_ID) #only keep locations from the full simple dataset that inlcude all 3 DOC, nitrate, phosphate

 
 all_chems <- read.csv("Data/LAGOS-US_beta/site_chemicalphysical_epi.csv") |>
   select(lagoslakeid, event_date, site_lat_decdeg, site_lon_decdeg, alk_ueql, ca_mgl, cl_mgl, spcond_uscm, do_mgl, mg_mgl, ph_eq, ph_field, salinity_mgl, so4_mgl, temp_degc) |>
   mutate(DATE_COL = as.Date(event_date)) |> #dates
   filter(year(DATE_COL) > 1999) |> #limit to post 2000 data
   mutate(SITE_ID = paste("LAGOS_",lagoslakeid, sep = "")) |> #add "LAGOS" to site id
   rename(LAT = site_lat_decdeg,
          LON = site_lon_decdeg) |>
   mutate(ECO_TYPE = "Lake") |>
   select(-event_date, - lagoslakeid) |>
   pivot_longer(3:13, names_to = "VARIABLE", values_to = "RESULT") |>
   drop_na(RESULT) |>
   mutate(UNIT = str_extract(VARIABLE, '(?<=_).+')) |>#extract units from last part of variable names, e.g., secchi_m, extract 'm'
   filter(SITE_ID %in% full$SITE_ID) #only keep locations from the full simple dataset that inlcude all 3 DOC, nitrate, phosphate

 all_lagos_data <- rbind(all_C, all_nutrients, all_chems) 
 
#save to other vars data
write.csv(all_lagos_data, "Data/other_vars_datasets/LAGOS.csv")
 
 