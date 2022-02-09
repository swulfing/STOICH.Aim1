library(tidyverse)
library(lubridate)
#LTER datasets

# #NTL LTER -- no phosphate 
# a <- read.csv("Data/LTER_data_raw/chemphys.csv")
# a.1 <- a %>%
#   mutate(across(everything(),~replace(., .<0,NA))) %>%
#   rename_at(vars(ph:drp_sloh),~str_c("value_",.)) %>%
#   rename_at(vars(flagph:flagdrp_sloh),~str_c("error_",.)) %>%
#   rename_all(~str_replace_all(.,"flag","")) %>%
#   pivot_longer(-(lakeid:sampledate),names_to =c('.value','item'),names_sep ='_') %>%
#   filter(!is.na(value)&value>=0) %>%
#   filter(!str_detect(error,'A|K|L')|is.na(error)) # Remove suspect data
# # A sample suspect
# # L data and blind differ by more than 15%
# # K data suspect
# a.2 <-a.1 %>%
#   dplyr::select(-error) %>%
#   group_by(sampledate, lakeid, item) %>%
#   mutate(result = mean(value)) %>%
#   ungroup() %>%
#   dplyr::select(-value) %>%
#   distinct() %>%
#   pivot_wider(id_cols = c(sampledate, lakeid), names_from = item, values_from = result) %>%
#   drop_na() %>%
#   mutate(`NO3 as N` = no3no2 / 1000,
#          `PO4 as P` =  totpuf / 1000) %>%
#   mutate(UNITS = "mg/L",
#          sampledate = as.Date(sampledate),
#          ECO_TYPE = "Lake") %>%
#   rename(DATE_COL = sampledate, 
#          `NO3 as N` = no3no2,
#          DOC = doc,
#          TP = totpuf) %>%
#   dplyr::select(-totpf) %>%
#   filter(DATE_COL >= "2012-01-01") #avoid overlap with LAGOS
# 
# #NTL lakes need to have same site id as LAGOS lakes becuase data prior to 2012 is in LAGOS
# library(LAGOSNE)
# #lagosne_get()
# lagos <- lagosne_load()
# LTER_sites <- lagos$lakes_limno %>%
#   filter(meandepthsource == "WI_LTER_SECCHI") %>%
#   dplyr::select(lagoslakeid, nhd_lat, nhd_long, lagosname1) %>%
#   mutate(SITE_ID = paste("LAGOS_",lagoslakeid, sep = "")) %>%
#   rename(LAT = nhd_lat,
#          LON = nhd_long) %>%
#   mutate(lakeid = NA,
#          lakeid = #ifelse(#lagoslakeid == 4625, "AL",
#                          #ifelse(lagoslakeid == 5248, "BM",
#                                 #ifelse(lagoslakeid == 4664, "TR",
#                                        #ifelse(lagoslakeid == 906, "SP",
#                                               ifelse(lagoslakeid == 827, "WI",
#                                                      ifelse(lagoslakeid == 5371, "ME",
#                                                            # ifelse(lagoslakeid == 120948, "TB", 
#                                                                    ifelse(lagoslakeid == 4559, "MO",
#                                                                           ifelse(lagoslakeid == 2746, "FI",lakeid)))))
#                                                                                 # ifelse(lagoslakeid == 4722, "CR", lakeid)))))))))))
# #final for this one
# a.3 <- left_join(a.2, LTER_sites) %>%
#   drop_na() %>%
#   dplyr::select(-lakeid, -lagoslakeid, -lagosname1)
# 




b <- read.csv("Data/LTER_data_raw/knb-lter-nwt.213.1/soddsolu.mw.data.csv")
b.1 <- b %>%
  dplyr::select(LTER_site, samp_loc, date, NO3., PO4..., DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(PO4... = as.numeric(PO4...) * (94.9714/1000) * 0.3261) %>% #ueq PO4/L to mg P/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNITS = "mg/L",
         SITE_ID = "NWT_Soddie_Outlet",
         ECO_TYPE = "River/Stream",
         LAT = 40.047778,
         LON = -105.570833) %>%
  dplyr::select(-LTER_site, - samp_loc) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         `PO4 as P` = mean(PO4...)) %>%
  ungroup() %>%
  dplyr::select(-NO3., - PO4...) %>%
  distinct()

# NO phosphate
#c <- read.csv("Data/LTER_data_raw/knb-lter-nwt.110.7/inlesolu.nc.data.csv")
# c.1 <- c %>%
#   dplyr::select(LTER_site, date, NO3., TP, DOC) %>%
#   mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
#   mutate(PO4... = as.numeric(PO4...) * (94.9714/1000) * 0.3261) %>% #ueq PO4/L to mg P/L
#   mutate(DOC = as.numeric(DOC)) %>%
#   drop_na() %>%
#   mutate(UNITS = "mg/L",
#          SITE_ID = "NWT_Albion_Inlet",
#          ECO_TYPE = "River/Stream",
#          LAT = 40.047178,
#          LON = -105.607123) %>%
#   dplyr::select(-LTER_site) %>%
#   rename(DATE_COL = date) %>%
#   mutate(DATE_COL = as.Date(DATE_COL)) %>%
#   group_by(DATE_COL) %>%
#   mutate(`NO3 as N` = mean(NO3.),
#          DOC = mean(DOC),
#          `PO4 as P` = mean(PO4...)) %>%
#   ungroup() %>%
#   dplyr::select(-NO3., - PO4...) %>%
#   distinct() %>%
#   filter(year(DATE_COL) >= 2000)

d <- read.csv("Data/LTER_data_raw/knb-lter-nwt.9.4/sad007solu.mw.data.csv")
d.1 <- d %>%
  filter(year >= 2000) %>%
  dplyr::select(LTER_site, date, NO3., PO4..., DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(PO4... = as.numeric(PO4...) * (94.9714/1000) * 0.3261) %>% #ueq PO4/L to mg P/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNITS = "mg/L",
         SITE_ID = "NWT_Saddle_Outflow007",
         ECO_TYPE = "River/Stream",
         LAT = 40.05485646,
         LON = -105.5901358) %>%
  dplyr::select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         `PO4 as P` = mean(PO4...)) %>%
  ungroup() %>%
  dplyr::select(-NO3., -PO4...) %>%
  distinct() 



e <- read.csv("Data/LTER_data_raw/knb-lter-nwt.103.14/albisolu.nc.data.csv")
e.1 <- e %>%
  filter(year >= 2000) %>%
  dplyr::select(LTER_site, date, NO3., PO4..., DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(PO4... = as.numeric(PO4...) * (94.9714/1000) * 0.3261) %>% #ueq PO4/L to mg P/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNITS = "mg/L",
         SITE_ID = "NWT_Albion_GLV",
         ECO_TYPE = "River/Stream",
         LAT = 40.0428746797462,
         LON = -105.59229602595) %>%
  dplyr::select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         `PO4 as P` = mean(PO4...)) %>%
  ungroup() %>%
  dplyr::select(-NO3., -PO4...) %>%
  distinct() 


f <- read.csv("Data/LTER_data_raw/knb-lter-nwt.109.12/gre5solu.nc.data.csv")
f.1 <- f %>%
  filter(year >= 2000) %>%
  dplyr::select(LTER_site, date, NO3., PO4..., DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(PO4... = as.numeric(PO4...) * (94.9714/1000) * 0.3261) %>% #ueq PO4/L to mg P/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNITS = "mg/L",
         SITE_ID = "NWT_GreenLake_Outlet",
         ECO_TYPE = "River/Stream",
         LAT = 40.0536706761558,
         LON = -105.627913103381) %>%
  dplyr::select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         `PO4 as P` = mean(PO4...)) %>%
  ungroup() %>%
  dplyr::select(-NO3., -PO4...) %>%
  distinct() 



g <- read.csv("Data/LTER_data_raw/knb-lter-nwt.112.4/martsolu.nc.data.csv")
g.1 <- g %>%
  filter(year >= 2000) %>%
  dplyr::select(LTER_site, date, NO3., PO4..., DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(PO4... = as.numeric(PO4...) * (94.9714/1000) * 0.3261) %>% #ueq PO4/L to mg P/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNITS = "mg/L",
         SITE_ID = "NWT_MartinelliBasin_Outlet",
         ECO_TYPE = "River/Stream",
         LAT = 40.0506020197246,
         LON = -105.595805026708) %>%
  dplyr::select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         `PO4 as P` = mean(PO4...)) %>%
  ungroup() %>%
  dplyr::select(-NO3., -PO4...) %>%
  distinct() 


#NO phosphate data 
#h <- read.csv("C:/Users/linne/OneDrive/Desktop/done_lter/knb-lter-nwt.114.6/spilsolu.nc.data.csv")
# h.1 <- h %>%
#   filter(year >= 2000) %>%
#   dplyr::select(LTER_site, date, NO3., TP, DOC) %>%
#   mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
#   mutate(TP = as.numeric(TP) * 123.88/1000) %>% #umol/L to mg/L
#   mutate(DOC = as.numeric(DOC)) %>%
#   drop_na() %>%
#   mutate(UNITS = "mg/L",
#          SITE_ID = "NWT_Albion_Spillway",
#          ECO_TYPE = "River/Stream",
#          LAT = 40.04287468,
#          LON = -105.592296) %>%
#   dplyr::select(-LTER_site) %>%
#   rename(DATE_COL = date) %>%
#   mutate(DATE_COL = as.Date(DATE_COL)) %>%
#   group_by(DATE_COL) %>%
#   mutate(`NO3 as N` = mean(NO3.),
#          DOC = mean(DOC),
#          TP = mean(TP)) %>%
#   ungroup() %>%
#   dplyr::select(-NO3.) %>%
#   distinct() 




i <- read.csv("Data/LTER_data_raw/knb-lter-nwt.160.3/saddsolu.nc.data.csv")
i.1 <- i %>%
  filter(year >= 2000) %>%
  dplyr::select(LTER_site, date, NO3., PO4..., DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(PO4... = as.numeric(PO4...) * (94.9714/1000) * 0.3261) %>% #ueq PO4/L to mg P/L
  drop_na() %>%
  mutate(UNITS = "mg/L",
         SITE_ID = "NWT_Saddle_belowNiwot",
         ECO_TYPE = "River/Stream",
         LAT = 40.04903177,
         LON =-105.5923174) %>%
  dplyr::select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(as.numeric(DOC)),
         `PO4 as P` = mean(PO4...)) %>%
  ungroup() %>%
  dplyr::select(-NO3., -PO4...) %>%
  distinct() 



j <- read.csv("Data/LTER_data_raw/knb-lter-nwt.163.3/grrgsolu.nc.data.csv")
j.1 <- j %>%
  filter(year >= 2000) %>%
  dplyr::select(LTER_site, date, NO3., PO4..., DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(PO4... = as.numeric(PO4...) * (94.9714/1000) * 0.3261) %>% #ueq PO4/L to mg P/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNITS = "mg/L",
         SITE_ID = "NWT_GreenLake5_RockGlacier",
         ECO_TYPE = "River/Stream",
         LAT = 40.05213501,
         LON = -105.6289841) %>%
  dplyr::select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         `PO4 as P` = mean(PO4...)) %>%
  ungroup() %>%
  dplyr::select(-NO3., -PO4...) %>%
  distinct() 



k <- read.csv("Data/LTER_data_raw/knb-lter-nwt.162.1/flumesolu.nc.data.csv")
k.1 <- k %>%
  filter(year >= 2000) %>%
  dplyr::select(LTER_site, date, NO3., PO4..., DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(PO4... = as.numeric(PO4...) * (94.9714/1000) * 0.3261) %>% #ueq PO4/L to mg P/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNITS = "mg/L",
         SITE_ID = "NWT_Flume_Outflow",
         ECO_TYPE = "River/Stream",
         LAT = 40.01368597,
         LON = -105.5578493) %>%
  dplyr::select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         `PO4 as P` = mean(PO4...)) %>%
  ungroup() %>%
  dplyr::select(-NO3., -PO4...) %>%
  distinct() 



l <- read.csv("Data/LTER_data_raw/edi.1006.1/Waterchem.csv")
l.1 <- l %>%
 filter(Analysis == "DOC" |
          Analysis == "Nitrate" | 
          Analysis == "SRP") %>%
  mutate(Concentration = ifelse(units == "ug/L", Concentration / 1000, Concentration)) %>%
  mutate(Concentration = ifelse(Analysis == "SRP", Concentration * 0.3261, Concentration)) %>% #convert mg SRP/L to mg P/L
  mutate(DATE_COL = as.Date(Date),
         SITE_ID = paste("Huron_", Collection.Site.or.Zone, sep = "")) %>%
  mutate(SITE_ID = gsub(" ", "", SITE_ID)) %>%
  pivot_wider(id_cols = c(DATE_COL, SITE_ID), names_from = Analysis, values_from = Concentration) %>%
  rename(`NO3 as N` = Nitrate) %>%
  rename(`PO4 as P` = SRP) %>%
  drop_na() %>%
  mutate(UNITS = "mg/L",
         ECO_TYPE = "Lake")

l.sites <- read.csv("Data/LTER_data_raw/edi.1006.1/Site_locations.csv") %>%
  mutate(SITE_ID = paste("Huron_", Collection.Site.or.Zone, sep = "")) %>%
  mutate(SITE_ID = gsub(" ", "", SITE_ID)) %>%
  dplyr::select(-Collection.Site.or.Zone) %>%
  rename(LAT = Latitude.Centroid,
         LON = Longitude.Centroid) %>%
  mutate(SITE_ID = ifelse(SITE_ID == "Huron_PointUrie", "Huron_UriePoint", SITE_ID))

l.2 <- left_join(l.1, l.sites) %>%
  distinct()



m <- read.csv("Data/LTER_data_raw/knb-lter-nwt.104.13/ariksolu.nc.data.csv")
m.1 <- m %>%
  filter(year >= 2000) %>%
  dplyr::select(LTER_site, date, NO3., PO4..., DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(PO4... = as.numeric(PO4...) * (94.9714/1000) * 0.3261) %>% #ueq PO4/L to mg P/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNITS = "mg/L",
         SITE_ID = "NWT_Arikaree_Drain",
         ECO_TYPE = "River/Stream",
         LAT = 40.0505740259853,
         LON = -105.642890924836) %>%
  dplyr::select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         `PO4 as P` = mean(PO4...)) %>%
  ungroup() %>%
  dplyr::select(-NO3., -PO4...) %>%
  distinct() 



n <- read.csv("Data/LTER_data_raw/knb-lter-nwt.107.10/gre1solu.nc.data.csv")
n.1 <- n %>%
  filter(year >= 2000) %>%
  dplyr::select(LTER_site, date, NO3., PO4..., DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(PO4... = as.numeric(PO4...) * (94.9714/1000) * 0.3261) %>% #ueq PO4/L to mg P/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNITS = "mg/L",
         SITE_ID = "NWT_GreenLake1_Outflow",
         ECO_TYPE = "River/Stream",
         LAT = 40.0615,
         LON = -105.643) %>%
  dplyr::select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         `PO4 as P` = mean(PO4...)) %>%
  ungroup() %>%
  dplyr::select(-NO3., -PO4...) %>%
  distinct() 




o <- read.csv("Data/LTER_data_raw/knb-lter-nwt.108.12/gre4solu.nc.data.csv")
o.1 <- o %>%
  filter(year >= 2000) %>%
  dplyr::select(LTER_site, date, NO3., PO4..., DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(PO4... = as.numeric(PO4...) * (94.9714/1000) * 0.3261) %>% #ueq PO4/L to mg P/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNITS = "mg/L",
         SITE_ID = "NWT_GreenLake4_Outflow",
         ECO_TYPE = "River/Stream",
         LAT = 40.0558068853579,
         LON = -105.621371408807) %>%
  dplyr::select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         `PO4 as P` = mean(PO4...)) %>%
  ungroup() %>%
  dplyr::select(-NO3., -PO4...) %>%
  distinct() 



p <- read.csv("Data/LTER_data_raw/knb-lter-ntl.281.2/powers_dissertation_stream_chemistry.csv")
p.1 <- p %>%
  dplyr::select(sampledate, streamname, stationid, doc, no32_2, srp) %>%
  drop_na() %>%
  mutate(DATE_COL = as.Date(sampledate, format = "%m/%d/%Y")) %>%
  group_by(DATE_COL, streamname, stationid) %>%
  summarise(DOC = mean(doc),
            `NO3 as N` = mean(no32_2),
            `PO4 as P` = mean(srp)) %>%
  ungroup()%>%
  mutate(UNITS = "mg/L",
         ECO_TYPE = "River/Stream")

p.sites <- read.csv("Data/LTER_data_raw/knb-lter-ntl.281.2/powers_streams_and_stations.csv") %>%
  drop_na() %>%
  rename(LAT = lat_decimal,
         LON = long_decimal) %>%
  dplyr::select(-wb_id)

p.2 <- left_join(p.1, p.sites) %>%
  distinct() %>%
  mutate(SITE_ID = paste(streamname, stationid, sep = "_")) %>%
  dplyr::select(-streamname, - stationid)



####combine all datasets into one LTER dataset##################################
LTER_all <- rbind(b.1, d.1, e.1, f.1, g.1, i.1, j.1, k.1, l.2, m.1, n.1, o.1, p.2) %>%
  filter(`PO4 as P` >= 0)


write.csv(LTER_all, "Data/Simplified_datasets_per_source/SIMPLE_LTER.csv")

###############################################################################
###############################################################################
###############################################################################
###############################################################################
#getting all variables info

# #NTL LTER -- no phosphate 
# a <- read.csv("Data/LTER_data_raw/chemphys.csv")
# a.1 <- a %>%
#   mutate(across(everything(),~replace(., .<0,NA))) %>%
#   rename_at(vars(ph:drp_sloh),~str_c("value_",.)) %>%
#   rename_at(vars(flagph:flagdrp_sloh),~str_c("error_",.)) %>%
#   rename_all(~str_replace_all(.,"flag","")) %>%
#   pivot_longer(-(lakeid:sampledate),names_to =c('.value','item'),names_sep ='_') %>%
#   filter(!is.na(value)&value>=0) %>%
#   filter(!str_detect(error,'A|K|L')|is.na(error)) # Remove suspect data
# # A sample suspect
# # L data and blind differ by more than 15%
# # K data suspect
# a.2 <-a.1 %>%
#   dplyr::select(-error) %>%
#   group_by(sampledate, lakeid, item) %>%
#   mutate(result = mean(value)) %>%
#   ungroup() %>%
#   dplyr::select(-value) %>%
#   distinct() %>%
#   pivot_wider(id_cols = c(sampledate, lakeid), names_from = item, values_from = result) %>%
#   drop_na() %>%
#   mutate(`NO3 as N` = no3no2 / 1000,
#          `PO4 as P` =  totpuf / 1000) %>%
#   mutate(UNITS = "mg/L",
#          sampledate = as.Date(sampledate),
#          ECO_TYPE = "Lake") %>%
#   rename(DATE_COL = sampledate, 
#          `NO3 as N` = no3no2,
#          DOC = doc,
#          TP = totpuf) %>%
#   dplyr::select(-totpf) %>%
#   filter(DATE_COL >= "2012-01-01") #avoid overlap with LAGOS
# 
# #NTL lakes need to have same site id as LAGOS lakes becuase data prior to 2012 is in LAGOS
# library(LAGOSNE)
# #lagosne_get()
# lagos <- lagosne_load()
# LTER_sites <- lagos$lakes_limno %>%
#   filter(meandepthsource == "WI_LTER_SECCHI") %>%
#   dplyr::select(lagoslakeid, nhd_lat, nhd_long, lagosname1) %>%
#   mutate(SITE_ID = paste("LAGOS_",lagoslakeid, sep = "")) %>%
#   rename(LAT = nhd_lat,
#          LON = nhd_long) %>%
#   mutate(lakeid = NA,
#          lakeid = #ifelse(#lagoslakeid == 4625, "AL",
#                          #ifelse(lagoslakeid == 5248, "BM",
#                                 #ifelse(lagoslakeid == 4664, "TR",
#                                        #ifelse(lagoslakeid == 906, "SP",
#                                               ifelse(lagoslakeid == 827, "WI",
#                                                      ifelse(lagoslakeid == 5371, "ME",
#                                                            # ifelse(lagoslakeid == 120948, "TB", 
#                                                                    ifelse(lagoslakeid == 4559, "MO",
#                                                                           ifelse(lagoslakeid == 2746, "FI",lakeid)))))
#                                                                                 # ifelse(lagoslakeid == 4722, "CR", lakeid)))))))))))
# #final for this one
# a.3 <- left_join(a.2, LTER_sites) %>%
#   drop_na() %>%
#   dplyr::select(-lakeid, -lagoslakeid, -lagosname1)
# 




b <- read.csv("Data/LTER_data_raw/knb-lter-nwt.213.1/soddsolu.mw.data.csv")
b.1 <- b %>%
  #dplyr::select(LTER_site, samp_loc, date, NO3., PO4..., DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(PO4... = as.numeric(PO4...) * (94.9714/1000) * 0.3261) %>% #ueq PO4/L to mg P/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNIT = "see metadata, DOC, PO4, and NO3 are mg/L",
         SITE_ID = "NWT_Soddie_Outlet",
         ECO_TYPE = "River/Stream",
         LAT = 40.047778,
         LON = -105.570833) %>%
  dplyr::select(-LTER_site, - samp_loc) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         `PO4 as P` = mean(PO4...)) %>%
  ungroup() %>%
  dplyr::select(-NO3., - PO4...) %>%
  distinct() |>
  select(-year, -time) |>
  mutate_at(c(2:38,45:46), as.numeric) |>
  pivot_longer(c(2:38,45:46), names_to = "VARIABLE", values_to = "RESULT") |>
  select(-comments) |>
  filter(SITE_ID %in% LTER_all$SITE_ID) |>
  drop_na()

# NO phosphate
#c <- read.csv("Data/LTER_data_raw/knb-lter-nwt.110.7/inlesolu.nc.data.csv")
# c.1 <- c %>%
#   dplyr::select(LTER_site, date, NO3., TP, DOC) %>%
#   mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
#   mutate(PO4... = as.numeric(PO4...) * (94.9714/1000) * 0.3261) %>% #ueq PO4/L to mg P/L
#   mutate(DOC = as.numeric(DOC)) %>%
#   drop_na() %>%
#   mutate(UNITS = "mg/L",
#          SITE_ID = "NWT_Albion_Inlet",
#          ECO_TYPE = "River/Stream",
#          LAT = 40.047178,
#          LON = -105.607123) %>%
#   dplyr::select(-LTER_site) %>%
#   rename(DATE_COL = date) %>%
#   mutate(DATE_COL = as.Date(DATE_COL)) %>%
#   group_by(DATE_COL) %>%
#   mutate(`NO3 as N` = mean(NO3.),
#          DOC = mean(DOC),
#          `PO4 as P` = mean(PO4...)) %>%
#   ungroup() %>%
#   dplyr::select(-NO3., - PO4...) %>%
#   distinct() %>%
#   filter(year(DATE_COL) >= 2000)

d <- read.csv("Data/LTER_data_raw/knb-lter-nwt.9.4/sad007solu.mw.data.csv")
d.1 <- d %>%
  filter(year >= 2000) %>%
  #dplyr::select(LTER_site, date, NO3., PO4..., DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(PO4... = as.numeric(PO4...) * (94.9714/1000) * 0.3261) %>% #ueq PO4/L to mg P/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNIT = "see metadata, DOC, PO4, and NO3 are mg/L",
         SITE_ID = "NWT_Saddle_Outflow007",
         ECO_TYPE = "River/Stream",
         LAT = 40.05485646,
         LON = -105.5901358) %>%
  dplyr::select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         `PO4 as P` = mean(PO4...)) %>%
  ungroup() %>%
  dplyr::select(-NO3., -PO4...) %>%
  distinct() |>
  select(-year, -time, - comments, -local_site) |>
  mutate_at(c(2:38,44:45), as.numeric) |>
  pivot_longer(c(2:38,44:45), names_to = "VARIABLE", values_to = "RESULT") |>
 # select(-comments) |>
  filter(SITE_ID %in% LTER_all$SITE_ID) |>
  drop_na()




e <- read.csv("Data/LTER_data_raw/knb-lter-nwt.103.14/albisolu.nc.data.csv")
e.1 <- e %>%
  filter(year >= 2000) %>%
 # dplyr::select(LTER_site, date, NO3., PO4..., DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(PO4... = as.numeric(PO4...) * (94.9714/1000) * 0.3261) %>% #ueq PO4/L to mg P/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNIT = "see metadata, DOC, PO4, and NO3 are mg/L",
         SITE_ID = "NWT_Albion_GLV",
         ECO_TYPE = "River/Stream",
         LAT = 40.0428746797462,
         LON = -105.59229602595) %>%
  dplyr::select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         `PO4 as P` = mean(PO4...)) %>%
  ungroup() %>%
  dplyr::select(-NO3., -PO4...) %>%
  distinct() |>
  select(-year, -time, - comments, -local_site) |>
  mutate_at(c(2:38,44:45), as.numeric) |>
  pivot_longer(c(2:38,44:45), names_to = "VARIABLE", values_to = "RESULT") |>
  # select(-comments) |>
  filter(SITE_ID %in% LTER_all$SITE_ID) |>
  drop_na()


f <- read.csv("Data/LTER_data_raw/knb-lter-nwt.109.12/gre5solu.nc.data.csv")
f.1 <- f %>%
  filter(year >= 2000) %>%
 # dplyr::select(LTER_site, date, NO3., PO4..., DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(PO4... = as.numeric(PO4...) * (94.9714/1000) * 0.3261) %>% #ueq PO4/L to mg P/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNIT = "see metadata, DOC, PO4, and NO3 are mg/L",
         SITE_ID = "NWT_GreenLake_Outlet",
         ECO_TYPE = "River/Stream",
         LAT = 40.0536706761558,
         LON = -105.627913103381) %>%
  dplyr::select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         `PO4 as P` = mean(PO4...)) %>%
  ungroup() %>%
  dplyr::select(-NO3., -PO4...) %>%
  distinct() |>
  select(-year, -time, - comments, -local_site) |>
  mutate_at(c(2:38,44:45), as.numeric) |>
  pivot_longer(c(2:38,44:45), names_to = "VARIABLE", values_to = "RESULT") |>
  # select(-comments) |>
  filter(SITE_ID %in% LTER_all$SITE_ID) |>
  drop_na()



g <- read.csv("Data/LTER_data_raw/knb-lter-nwt.112.4/martsolu.nc.data.csv")
g.1 <- g %>%
  filter(year >= 2000) %>%
  #dplyr::select(LTER_site, date, NO3., PO4..., DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(PO4... = as.numeric(PO4...) * (94.9714/1000) * 0.3261) %>% #ueq PO4/L to mg P/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNIT = "see metadata, DOC, PO4, and NO3 are mg/L",
         SITE_ID = "NWT_MartinelliBasin_Outlet",
         ECO_TYPE = "River/Stream",
         LAT = 40.0506020197246,
         LON = -105.595805026708) %>%
  dplyr::select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         `PO4 as P` = mean(PO4...)) %>%
  ungroup() %>%
  dplyr::select(-NO3., -PO4...) %>%
  distinct() |>
  select(-year, -time, - comments, -local_site) |>
  mutate_at(c(2:38,44:45), as.numeric) |>
  pivot_longer(c(2:38,44:45), names_to = "VARIABLE", values_to = "RESULT") |>
  # select(-comments) |>
  filter(SITE_ID %in% LTER_all$SITE_ID) |>
  drop_na()


#NO phosphate data 
#h <- read.csv("C:/Users/linne/OneDrive/Desktop/done_lter/knb-lter-nwt.114.6/spilsolu.nc.data.csv")
# h.1 <- h %>%
#   filter(year >= 2000) %>%
#   dplyr::select(LTER_site, date, NO3., TP, DOC) %>%
#   mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
#   mutate(TP = as.numeric(TP) * 123.88/1000) %>% #umol/L to mg/L
#   mutate(DOC = as.numeric(DOC)) %>%
#   drop_na() %>%
#   mutate(UNITS = "mg/L",
#          SITE_ID = "NWT_Albion_Spillway",
#          ECO_TYPE = "River/Stream",
#          LAT = 40.04287468,
#          LON = -105.592296) %>%
#   dplyr::select(-LTER_site) %>%
#   rename(DATE_COL = date) %>%
#   mutate(DATE_COL = as.Date(DATE_COL)) %>%
#   group_by(DATE_COL) %>%
#   mutate(`NO3 as N` = mean(NO3.),
#          DOC = mean(DOC),
#          TP = mean(TP)) %>%
#   ungroup() %>%
#   dplyr::select(-NO3.) %>%
#   distinct() 




i <- read.csv("Data/LTER_data_raw/knb-lter-nwt.160.3/saddsolu.nc.data.csv")
i.1 <- i %>%
  filter(year >= 2000) %>%
 # dplyr::select(LTER_site, date, NO3., PO4..., DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(PO4... = as.numeric(PO4...) * (94.9714/1000) * 0.3261) %>% #ueq PO4/L to mg P/L
  drop_na() %>%
  mutate(UNIT = "see metadata, DOC, PO4, and NO3 are mg/L",
         SITE_ID = "NWT_Saddle_belowNiwot",
         ECO_TYPE = "River/Stream",
         LAT = 40.04903177,
         LON =-105.5923174) %>%
  dplyr::select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(as.numeric(DOC)),
         `PO4 as P` = mean(PO4...)) %>%
  ungroup() %>%
  dplyr::select(-NO3., -PO4...) %>%
  distinct()  |>
  select(-year, -time, - comments, -local_site) |>
  mutate_at(c(2:38,44:45), as.numeric) |>
  pivot_longer(c(2:38,44:45), names_to = "VARIABLE", values_to = "RESULT") |>
  # select(-comments) |>
  filter(SITE_ID %in% LTER_all$SITE_ID) |>
  drop_na()



j <- read.csv("Data/LTER_data_raw/knb-lter-nwt.163.3/grrgsolu.nc.data.csv")
j.1 <- j %>%
  filter(year >= 2000) %>%
 # dplyr::select(LTER_site, date, NO3., PO4..., DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(PO4... = as.numeric(PO4...) * (94.9714/1000) * 0.3261) %>% #ueq PO4/L to mg P/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNIT = "see metadata, DOC, PO4, and NO3 are mg/L",
         SITE_ID = "NWT_GreenLake5_RockGlacier",
         ECO_TYPE = "River/Stream",
         LAT = 40.05213501,
         LON = -105.6289841) %>%
  dplyr::select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         `PO4 as P` = mean(PO4...)) %>%
  ungroup() %>%
  dplyr::select(-NO3., -PO4...) %>%
  distinct()  |>
  select(-year, -time, - comments, -local_site) |>
  mutate_at(c(2:38,44:45), as.numeric) |>
  pivot_longer(c(2:38,44:45), names_to = "VARIABLE", values_to = "RESULT") |>
  # select(-comments) |>
  filter(SITE_ID %in% LTER_all$SITE_ID) |>
  drop_na()



k <- read.csv("Data/LTER_data_raw/knb-lter-nwt.162.1/flumesolu.nc.data.csv")
k.1 <- k %>%
  filter(year >= 2000) %>%
  #dplyr::select(LTER_site, date, NO3., PO4..., DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(PO4... = as.numeric(PO4...) * (94.9714/1000) * 0.3261) %>% #ueq PO4/L to mg P/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNIT = "see metadata, DOC, PO4, and NO3 are mg/L",
         SITE_ID = "NWT_Flume_Outflow",
         ECO_TYPE = "River/Stream",
         LAT = 40.01368597,
         LON = -105.5578493) %>%
  dplyr::select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         `PO4 as P` = mean(PO4...)) %>%
  ungroup() %>%
  dplyr::select(-NO3., -PO4...) %>%
  distinct() |>
  select(-year, -time, - comments, -local_site) |>
  mutate_at(c(2:38,44:45), as.numeric) |>
  pivot_longer(c(2:38,44:45), names_to = "VARIABLE", values_to = "RESULT") |>
  # select(-comments) |>
  filter(SITE_ID %in% LTER_all$SITE_ID) |>
  drop_na()



l <- read.csv("Data/LTER_data_raw/edi.1006.1/Waterchem.csv")
l.1 <- l %>%
  # filter(Analysis == "DOC" |
  #          Analysis == "Nitrate" | 
  #          Analysis == "SRP") %>%
  mutate(Concentration = ifelse(units == "ug/L" & Analysis %in% c('Nitrate', 'SRP'), Concentration / 1000, Concentration)) %>%
  mutate(Concentration = ifelse(Analysis == "SRP", Concentration * 0.3261, Concentration)) %>% #convert mg SRP/L to mg P/L
  mutate(DATE_COL = as.Date(Date),
         SITE_ID = paste("Huron_", Collection.Site.or.Zone, sep = "")) %>%
  mutate(SITE_ID = gsub(" ", "", SITE_ID)) %>%
  #pivot_wider(id_cols = c(DATE_COL, SITE_ID), names_from = Analysis, values_from = Concentration) %>%
  mutate(Analysis =ifelse(Analysis == 'Nitrate','NO3 as N', Analysis)) %>%
  mutate(Analysis =ifelse(Analysis == 'SRP','PO4 as P', Analysis)) %>%
  drop_na() %>%
  rename(UNIT = units,
         VARIABLE = Analysis,
         RESULT = Concentration) %>%
  mutate(UNIT = ifelse(VARIABLE %in% c('DOC', 'NO3 as N', 'PO4 as P'), 'mg/L', UNIT),
         ECO_TYPE = "Lake") %>%
  select(-Site.Label, -Collection.Site.or.Zone, -Date, -Type, - Collection.depth) %>%
  drop_na(RESULT)

l.sites <- read.csv("Data/LTER_data_raw/edi.1006.1/Site_locations.csv") %>%
  mutate(SITE_ID = paste("Huron_", Collection.Site.or.Zone, sep = "")) %>%
  mutate(SITE_ID = gsub(" ", "", SITE_ID)) %>%
  dplyr::select(-Collection.Site.or.Zone) %>%
  rename(LAT = Latitude.Centroid,
         LON = Longitude.Centroid) %>%
  mutate(SITE_ID = ifelse(SITE_ID == "Huron_PointUrie", "Huron_UriePoint", SITE_ID))

l.2 <- left_join(l.1, l.sites) %>%
  distinct() %>%
  filter(SITE_ID %in% LTER_all$SITE_ID)



m <- read.csv("Data/LTER_data_raw/knb-lter-nwt.104.13/ariksolu.nc.data.csv")
m.1 <- m %>%
  filter(year >= 2000) %>%
  #dplyr::select(LTER_site, date, NO3., PO4..., DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(PO4... = as.numeric(PO4...) * (94.9714/1000) * 0.3261) %>% #ueq PO4/L to mg P/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNIT = "see metadata, DOC, PO4, and NO3 are mg/L",
         SITE_ID = "NWT_Arikaree_Drain",
         ECO_TYPE = "River/Stream",
         LAT = 40.0505740259853,
         LON = -105.642890924836) %>%
  dplyr::select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         `PO4 as P` = mean(PO4...)) %>%
  ungroup() %>%
  dplyr::select(-NO3., -PO4...) %>%
  distinct() |>
  select(-year, -time, - comments, -local_site) |>
  mutate_at(c(2:38,44:45), as.numeric) |>
  pivot_longer(c(2:38,44:45), names_to = "VARIABLE", values_to = "RESULT") |>
  # select(-comments) |>
  filter(SITE_ID %in% LTER_all$SITE_ID) |>
  drop_na()



n <- read.csv("Data/LTER_data_raw/knb-lter-nwt.107.10/gre1solu.nc.data.csv")
n.1 <- n %>%
  filter(year >= 2000) %>%
  #dplyr::select(LTER_site, date, NO3., PO4..., DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(PO4... = as.numeric(PO4...) * (94.9714/1000) * 0.3261) %>% #ueq PO4/L to mg P/L
  mutate(DOC = as.numeric(DOC)) %>%
 # drop_na() %>%
  mutate(UNIT = "see metadata, DOC, PO4, and NO3 are mg/L",
         SITE_ID = "NWT_GreenLake1_Outflow",
         ECO_TYPE = "River/Stream",
         LAT = 40.0615,
         LON = -105.643) %>%
  dplyr::select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         `PO4 as P` = mean(PO4...)) %>%
  ungroup() %>%
  dplyr::select(-NO3., -PO4...) %>%
  distinct() |>
  select(-year, -time, - comments, -local_site) |>
  mutate_at(c(2:38,44:45), as.numeric) |>
  pivot_longer(c(2:38,44:45), names_to = "VARIABLE", values_to = "RESULT") |>
  # select(-comments) |>
  filter(SITE_ID %in% LTER_all$SITE_ID) |>
  drop_na()




o <- read.csv("Data/LTER_data_raw/knb-lter-nwt.108.12/gre4solu.nc.data.csv")
o.1 <- o %>%
  filter(year >= 2000) %>%
  #dplyr::select(LTER_site, date, NO3., PO4..., DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(PO4... = as.numeric(PO4...) * (94.9714/1000) * 0.3261) %>% #ueq PO4/L to mg P/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNIT = "see metadata, DOC, PO4, and NO3 are mg/L",
         SITE_ID = "NWT_GreenLake4_Outflow",
         ECO_TYPE = "River/Stream",
         LAT = 40.0558068853579,
         LON = -105.621371408807) %>%
  dplyr::select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         `PO4 as P` = mean(PO4...)) %>%
  ungroup() %>%
  dplyr::select(-NO3., -PO4...) %>%
  distinct() |>
  select(-year, -time, - comments, -local_site) |>
  mutate_at(c(2:38,44:45), as.numeric) |>
  pivot_longer(c(2:38,44:45), names_to = "VARIABLE", values_to = "RESULT") |>
  # select(-comments) |>
  filter(SITE_ID %in% LTER_all$SITE_ID) |>
  drop_na()




p <- read.csv("Data/LTER_data_raw/knb-lter-ntl.281.2/powers_dissertation_stream_chemistry.csv")
p.1 <- p %>%
  #dplyr::select(sampledate, streamname, stationid, doc, no32_2, srp) %>%
  #drop_na() %>%
  mutate(DATE_COL = as.Date(sampledate, format = "%m/%d/%Y")) %>%
  group_by(DATE_COL, streamname, stationid) %>%
  rename(DOC = doc,
            `NO3 as N` = no32_2,
            `PO4 as P` = srp) %>%
  ungroup()%>%
  mutate(UNIT = "see metadata, DOC, PO4, and NO3 are mg/L",
         ECO_TYPE = "River/Stream") |>
  pivot_longer(7:27, names_to = "VARIABLE", values_to = "RESULT") %>%
  drop_na(RESULT)

p.sites <- read.csv("Data/LTER_data_raw/knb-lter-ntl.281.2/powers_streams_and_stations.csv") %>%
  drop_na() %>%
  rename(LAT = lat_decimal,
         LON = long_decimal) %>%
  dplyr::select(-wb_id)

p.2 <- left_join(p.1, p.sites) %>%
  distinct() %>%
  mutate(SITE_ID = paste(streamname, stationid, sep = "_")) %>%
  dplyr::select(-streamname, - stationid) |>
  filter(SITE_ID %in% LTER_all$SITE_ID) |>
  select(-c(1:4))

####combine all datasets into one LTER dataset##################################
LTER_all_vars <- rbind(b.1, d.1, e.1, f.1, g.1, i.1, j.1, k.1, l.2, m.1, n.1, o.1, p.2) 


write.csv(LTER_all_vars, "Data/other_vars_datasets/LTER.csv")
