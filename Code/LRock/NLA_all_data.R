#Script to organize and combine NLA files 

#script to build master datasheet
library(tidyverse)
library(lubridate)
library(sf)


#NLA data####
#QUESTION: is all NLA data in mg/L?? -- no it's not.. ANNOYING

#nla location info - more info in full metadata 
nla_sites_lakes <- read.csv("Data/NLA/NLA2012_combinedData.csv") %>%
  select(SITE_ID, LAT_DD83, LON_DD83) %>%
  distinct() %>%
  mutate(ECO_TYPE = "Lake")

nla_sites_rivers <- read.csv("Data/NLA/Metadata/Points_NLArs2008.csv") %>%
  select(SITE_ID, LAT_DD83, LON_DD83) %>%
  rbind(read.csv("Data/NLA/Metadata/Points_NLArs2019.csv")%>%
          select(SITE_ID, LAT_DD83, LON_DD83)) %>%
  rbind(read.csv("Data/NLA/Metadata/Points_NLAs2004.csv")%>%
          select(SITE_ID, LAT_DD83, LON_DD83)) %>%
  distinct() %>%
  mutate(ECO_TYPE = "River/Stream")

nla_2013_sites <- read.csv("Data/NLA/ChemData_NLArs2013.csv") %>%
  select(SITE_ID, LAT_DD83, LON_DD83) %>%
  distinct() %>%
  mutate(ECO_TYPE = "River/Stream")

nla_2017_sites <- read.csv("Data/NLA/Metadata/nla_2017_site_information-data.csv") %>%
  select(SITE_ID, LAT_DD83, LON_DD83) %>%
  distinct() %>%
  mutate(ECO_TYPE = "Lake")

NLA_sites <- nla_sites_lakes %>%
  rbind(nla_sites_rivers)%>%
  rbind(nla_2013_sites) %>% 
  rbind(nla_2017_sites) %>%
  distinct()


#nla data
nla_2012 <- read.csv("Data/NLA/NLA2012_combinedData.csv") %>%
  select(SITE_ID, DATE_COL, DOC_UNITS, DOC_RESULT, NITRATE_N_UNITS, NITRATE_N_RESULT, PTL_UNITS, PTL_RESULT, DOC_LAB_FLAG, NITRATE_N_LAB_FLAG, PTL_LAB_FLAG) %>%
  mutate(YEAR = 2012) %>%
  rename(DOC = DOC_RESULT,
         DOC_FLAG = DOC_LAB_FLAG,
         NO3 = NITRATE_N_RESULT,
         NO3_UNITS = NITRATE_N_UNITS,
         NO3_FLAG = NITRATE_N_LAB_FLAG,
         TP = PTL_RESULT,
         TP_UNITS = PTL_UNITS,
         TP_FLAG = PTL_LAB_FLAG)  %>%
  mutate(DATE_COL = as.Date(DATE_COL, format = "%m/%d/%Y"))


nla_rs_2008 <- read.csv("Data/NLA/ChemData_NLArs2008.csv") %>%
  select(SITE_ID, DATE_COL, DOC, NO3, PTL, DOC_ALERT, NO3_ALERT, PTL_ALERT) %>%
  mutate(YEAR = 2008) %>%
  rename(
    DOC_FLAG = DOC_ALERT,
    NO3_FLAG = NO3_ALERT,
    TP = PTL,
    TP_FLAG = PTL_ALERT) %>%
  mutate(TP_UNITS = "UG/L",
         DOC_UNITS = "MG/L",
         NO3_UNITS = "MG/L")%>%
  mutate(DATE_COL = as.Date(DATE_COL, format = "%d-%b-%y"))

nla_rs_2013 <- read.csv("Data/NLA/ChemData_NLArs2013.csv") %>%
  select(SITE_ID, DATE_COL, DOC_RESULT_UNITS, DOC_RESULT, NITRATE_N_RESULT_UNITS, NITRATE_N_RESULT, PTL_RESULT_UNITS, PTL_RESULT, DOC_NARS_FLAG, NITRATE_N_NARS_FLAG, PTL_NARS_FLAG) %>%
  mutate(YEAR = 2013)  %>%
  rename(DOC = DOC_RESULT,
         DOC_UNITS = DOC_RESULT_UNITS,
         DOC_FLAG = DOC_NARS_FLAG,
         NO3 = NITRATE_N_RESULT,
         NO3_UNITS = NITRATE_N_RESULT_UNITS,
         NO3_FLAG = NITRATE_N_NARS_FLAG,
         TP = PTL_RESULT,
         TP_UNITS = PTL_RESULT_UNITS,
         TP_FLAG = PTL_NARS_FLAG)  %>%
  mutate(DATE_COL = as.Date(DATE_COL, format = "%m/%d/%Y"))



nla_rs_2004 <- read.csv("Data/NLA/ChemData_NLAs2004.csv") %>%
  select(SITE_ID, DATE_COL, DOC, NO3, PTL, DOCF, NO3F, PTLF) %>%
  mutate(YEAR = 2004)  %>%
  rename(
    DOC_FLAG = DOCF,
    NO3_FLAG = NO3F,
    TP = PTL,
    TP_FLAG = PTLF) %>%
  mutate(TP_UNITS = "UG/L",
         DOC_UNITS = "MG/L",
         NO3_UNITS = "Ueq/L")  %>%
  mutate(DATE_COL = as.Date(DATE_COL, format = "%m/%d/%Y"))

nla_rs_2019 <- read.csv("Data/NLA/ChemData_NLArs2019.csv") %>%
  select(SITE_ID, DATE_COL, DOC_UNITS, DOC_RESULT, NITRATE_N_UNITS, NITRATE_N_RESULT, PTL_UNITS, PTL_RESULT, DOC_NARS_FLAG, NITRATE_N_NARS_FLAG, PTL_NARS_FLAG) %>%
  mutate(YEAR = 2019) %>%
  rename(DOC = DOC_RESULT,
         DOC_FLAG = DOC_NARS_FLAG,
         NO3 = NITRATE_N_RESULT,
         NO3_UNITS = NITRATE_N_UNITS,
         NO3_FLAG = NITRATE_N_NARS_FLAG,
         TP = PTL_RESULT,
         TP_UNITS = PTL_UNITS,
         TP_FLAG = PTL_NARS_FLAG) %>%
  mutate(DATE_COL = as.Date(DATE_COL, format = "%m/%d/%Y"))

#2017 data -- annoying as hell
nla_2017_results <- read.csv("Data/NLA/Original_ChemData_NLA2017.csv") %>%
  select(SITE_ID, DATE_COL, ANALYTE, RESULT, RESULT_UNITS, NARS_FLAG) %>%
  mutate(YEAR = 2017) %>%
  pivot_wider(id_cols = c(SITE_ID, DATE_COL), values_from = RESULT, names_from = ANALYTE) %>%
  rename(
    NO3 = NITRATE_N,
    TP= PTL) %>%
  select(-NITRATE_NITRITE_N)

nla_2017_units <- read.csv("Data/NLA/Original_ChemData_NLA2017.csv") %>%
  select(SITE_ID, DATE_COL, ANALYTE, RESULT, RESULT_UNITS, NARS_FLAG) %>%
  mutate(YEAR = 2017) %>%
  pivot_wider(id_cols = c(SITE_ID, DATE_COL), values_from = RESULT_UNITS, names_from = ANALYTE) %>%
  rename(DOC_UNITS = DOC,
         NO3_UNITS = NITRATE_N,
         TP_UNITS = PTL) %>%
  select(-NITRATE_NITRITE_N)


nla_2017_flags <- read.csv("Data/NLA/Original_ChemData_NLA2017.csv") %>%
  select(SITE_ID, DATE_COL, ANALYTE, RESULT, RESULT_UNITS, NARS_FLAG) %>%
  mutate(YEAR = 2017) %>%
  pivot_wider(id_cols = c(SITE_ID, DATE_COL), values_from = NARS_FLAG, names_from = ANALYTE) %>%
  rename(DOC_FLAG = DOC,
         NO3_FLAG = NITRATE_N,
         TP_FLAG = PTL) %>%
  select(-NITRATE_NITRITE_N)

nla_2017 <- nla_2017_results %>%
  left_join(nla_2017_units) %>%
  left_join(nla_2017_flags) %>%
  mutate(YEAR = 2017) %>%
  mutate(DATE_COL = as.Date(DATE_COL, format = "%d-%b-%y"))


###########FULL DATASET#########################################################


ALL_NLA_CNP <- nla_2017 %>%
  rbind(nla_2012) %>%
 # rbind(nla_rs_2004) %>% #get rid of this dataset :( It uses units of uEq/L but does not provide enough information to convert. Also, it seems to include MASSIVE outliers -- NO3 values that would be ~3000 mg N/L (if the calculation I tried was even correct)
  rbind(nla_rs_2008) %>%
  rbind(nla_rs_2013) %>%
  rbind(nla_rs_2019) %>%
  arrange(DATE_COL) %>%
  select(-DOC_FLAG, -NO3_FLAG, -TP_FLAG) %>%
  left_join(NLA_sites) %>%
  mutate(DOC = as.numeric(DOC),
         NO3 = as.numeric(NO3),
         TP = as.numeric(TP)) %>%
  mutate(DOC_UNITS = "mg/L",
         NO3_UNITS = "mg N/L")%>%
  mutate(TP = TP/1000) %>%
  mutate(TP_UNITS = "mg/L") %>%
  drop_na(LON_DD83) %>%
  rename(LAT = LAT_DD83,
         LON = LON_DD83)


#Not enough information to convert -- is this uEq/L NO3 or N?
#converting NO3 uEq/L to mg N/L
#molar mass of NO3 = 62.0049 g/mol
#valency of NO3 = -1, so use 1
#mg NO3/L to mg N/L =   * 0.2259
#NO3 mg N/L =[62.0049 ug/mmol * uEq/L (/1) /1000] *0.2259
# 



# units_TP <- ALL_NLA_CNP %>%
#   select(TP_UNITS) %>%
#   distinct()

#creating a 1 km buffer to lump overlapping sites that don't have lat, longs reported the same
NLA_SITES_1 <- ALL_NLA_CNP %>%
  select(SITE_ID, LAT_DD83, LON_DD83, ECO_TYPE) %>%
  distinct() #8354 sites -- but lots of overlaps

write.csv(NLA_SITES_1, "NLA_temporary.csv")

#rounded numbers and saved to new csv file ^ deleted above file too

test_check <- read.csv("C:/Users/linne/OneDrive/Documents/Book1.csv")

test_check2 <- test_check %>%
  select(LAT_DD83, LON_DD83) %>%
  distinct() %>%
  mutate(seq_id = paste("NLA_", seq(1:nrow(test_check2)), sep = "")) #6461 sites

sites <- test_check %>%
  left_join(test_check2) 


NLA_unique_sites <- sites %>%
  select(seq_id, LAT_DD83, LON_DD83, ECO_TYPE) %>%
  distinct() %>%
  rename(SITE_ID = seq_id)


write.csv(NLA_unique_sites, "Data/NLA/NLA_unique_sites.csv")


sites_1 <- sites %>%
  select(SITE_ID, seq_id)


ALL_NLA_CNP_1 <- ALL_NLA_CNP %>%
  left_join(sites_1)



ALL_NLA_CNP_1 <- ALL_NLA_CNP_1 %>%
  select(-SITE_ID) %>%
  rename(SITE_ID = seq_id)


write.csv(ALL_NLA_CNP_1, "Data/NLA/SIMPLE_NLA_CNP.csv")


















# 
# NLA_SITES_sf <- NLA_SITES_1 %>% 
#   st_as_sf(coords = c("LON_DD83", "LAT_DD83"), crs = 4326) %>%
#   rename(site_geometry = geometry)
# 
# circle_df <- NLA_SITES_1 %>%
#   st_as_sf(coords = c("LON_DD83", "LAT_DD83"), crs = 4326) %>%
#   st_transform(3035) %>%
#   st_buffer(dist = units::set_units(1, "kilometers")) %>%
#   rename(circle_geometry = geometry) 
# 
# # in_intersection <- st_intersection(circle_df) %>% 
# #   filter(n.overlaps == 1) %>%
# #   st_transform(4326) #%>%
#   # st_within(NLA_SITES_sf, .) %>%
#   # map_lgl(is_empty) %>%
#   # "!"() %>%
#   # which() %>%
#   # slice(NLA_SITES_sf, .) 
# 
# 
# plot(st_geometry(circle_df %>% filter(SITE_ID == "FW08NV012") %>% st_transform(4326)))
# plot(st_geometry(NLA_SITES_sf %>% filter(SITE_ID == "FW08NV012")), add = TRUE)
# 
# 
# sites_check <- NLA_SITES_sf %>%
#   left_join(as.data.frame(circle_df)) %>%
#   mutate(sequence = NA) %>%
#   reorder(site_geometry)
# 
# for(i in 1:nrow(sites_check)) {
#   
#   sites_check$sequence[i] = sites_check$site_geometry[i+1] - sites_check$site_geometry[i]
#   
# }

