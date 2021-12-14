#####Load packages####

library(tidyverse)
library(LAGOSNE)

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
#Question -- do we only want to keep rows that have all 3: DOC, nitrite/nitrate, TP?

lagos_data <- lagos$epi_nutr %>%
  dplyr::select(lagoslakeid, sampledate, doc, no2no3, tp) %>%
  drop_na() #only keeps sites/data where doc, nitrate, and tp were concurrently collected

lagos_data.1 <- lagos_data %>%
  rename(SITE_ID = lagoslakeid,
         DATE_COL = sampledate,
         DOC = doc,
         `NO3 as N` = no2no3,
         TP = tp) %>%
  mutate(DOC = DOC / 1000,
         `NO3 as N` = `NO3 as N` / 1000,
         TP = TP / 1000, 
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


# median_lagos <- lagos_data %>%
#   group_by(lagoslakeid) %>%
#   mutate(median_doc = median(doc),
#          median_no2no3 = median(no2no3),
#          median_tp = median(tp)) %>%
#   ungroup() %>%
#   select(-doc, -no2no3, - tp, -sampledate) %>%
#   distinct()
# 
# ggplot(median_lagos) +
#   geom_point(aes(median_doc, median_no2no3, color = median_tp)) +
#   #scale_y_log10() +
#   scale_color_viridis_c(direction = -1, "Total Phosphorus"~mu~g~L^-1) +
#   theme_bw() +
#   labs(x = "DOC"~mu~g~L^-1,
#        y = "Nitrate"~mu~g~L^-1)
# 
# 
# 
#find sources
 sources <- lagos$lagos_source_program
 
