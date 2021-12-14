library(tidyverse)
library(lubridate)
#LTER datasets

#NTL LTER -- 
a <- read.csv("C:/Users/lrock1/Downloads/chemphys.csv")
a.1 <- a %>%
  mutate(across(everything(),~replace(., .<0,NA))) %>%
  rename_at(vars(doc:totpuf),~str_c("value_",.)) %>%
  rename_at(vars(flagdoc:flagtotpuf),~str_c("error_",.)) %>%
  rename_all(~str_replace_all(.,"flag","")) %>%
  pivot_longer(-(lakeid:sampledate),names_to =c('.value','item'),names_sep ='_') %>%
  filter(!is.na(value)&value>=0) %>%
  filter(!str_detect(error,'A|K|L')|is.na(error)) # Remove suspect data
# A sample suspect
# L data and blind differ by more than 15%
# K data suspect
a.2 <-a.1 %>%
  select(-error) %>%
  group_by(sampledate, lakeid, item) %>%
  mutate(result = mean(value)) %>%
  ungroup() %>%
  select(-value) %>%
  distinct() %>%
  pivot_wider(id_cols = c(sampledate, lakeid), names_from = item, values_from = result) %>%
  drop_na() %>%
  mutate(no3no2 = no3no2 / 1000,
         totpuf =  totpuf / 1000) %>%
  mutate(UNITS = "mg/L",
         sampledate = as.Date(sampledate),
         ECO_TYPE = "Lake") %>%
  rename(DATE_COL = sampledate, 
         `NO3 as N` = no3no2,
         DOC = doc,
         TP = totpuf) %>%
  select(-totpf) %>%
  filter(DATE_COL >= "2012-01-01") #avoid overlap with LAGOS

#NTL lakes need to have same site id as LAGOS lakes becuase data prior to 2012 is in LAGOS
library(LAGOSNE)
lagos <- lagosne_load()
LTER_sites <- lagos$lakes_limno %>%
  filter(meandepthsource == "WI_LTER_SECCHI") %>%
  select(lagoslakeid, nhd_lat, nhd_long, lagosname1) %>%
  mutate(SITE_ID = paste("LAGOS_",lagoslakeid, sep = "")) %>%
  rename(LAT = nhd_lat,
         LON = nhd_long) %>%
  mutate(lakeid = NA,
         lakeid = ifelse(lagoslakeid == 4625, "AL",
                         ifelse(lagoslakeid == 5248, "BM",
                                ifelse(lagoslakeid == 4664, "TR",
                                       ifelse(lagoslakeid == 906, "SP",
                                              ifelse(lagoslakeid == 827, "WI",
                                                     ifelse(lagoslakeid == 5371, "ME",
                                                            ifelse(lagoslakeid == 120948, "TB", 
                                                                   ifelse(lagoslakeid == 4559, "MO",
                                                                          ifelse(lagoslakeid == 2746, "FI",
                                                                                 ifelse(lagoslakeid == 4722, "CR", lakeid)))))))))))
#final for this one
a.3 <- left_join(a.2, LTER_sites) %>%
  drop_na() %>%
  select(-lakeid, -lagoslakeid, -lagosname1)





b <- read.csv("C:/Users/lrock1/Downloads/knb-lter-nwt.213.1/soddsolu.mw.data.csv")
b.1 <- b %>%
  select(LTER_site, samp_loc, date, NO3., TP, DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(TP = as.numeric(TP) * 123.88/1000) %>% #umol/L to mg/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNITS = "mg/L",
         SITE_ID = "NWT_Soddie_Outlet",
         ECO_TYPE = "River/Stream",
         LAT = 40.047778,
         LON = -105.570833) %>%
  select(-LTER_site, - samp_loc) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         TP = mean(TP)) %>%
  ungroup() %>%
  select(-NO3.) %>%
  distinct()

c <- read.csv("C:/Users/lrock1/Downloads/knb-lter-nwt.110.7/inlesolu.nc.data.csv")
c.1 <- c %>%
  select(LTER_site, date, NO3., TP, DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(TP = as.numeric(TP) * 123.88/1000) %>% #umol/L to mg/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNITS = "mg/L",
         SITE_ID = "NWT_Albion_Inlet",
         ECO_TYPE = "River/Stream",
         LAT = 40.047178,
         LON = -105.607123) %>%
  select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         TP = mean(TP)) %>%
  ungroup() %>%
  select(-NO3.) %>%
  distinct() %>%
  filter(year(DATE_COL) >= 2000)

d <- read.csv("C:/Users/lrock1/Downloads/knb-lter-nwt.9.4/sad007solu.mw.data.csv")
d.1 <- d %>%
  filter(year >= 2000) %>%
  select(LTER_site, date, NO3., TP, DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(TP = as.numeric(TP) * 123.88/1000) %>% #umol/L to mg/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNITS = "mg/L",
         SITE_ID = "NWT_Saddle_Outflow007",
         ECO_TYPE = "River/Stream",
         LAT = 40.05485646,
         LON = -105.5901358) %>%
  select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         TP = mean(TP)) %>%
  ungroup() %>%
  select(-NO3.) %>%
  distinct() 



e <- read.csv("C:/Users/lrock1/Downloads/knb-lter-nwt.103.14/albisolu.nc.data.csv")
e.1 <- e %>%
  filter(year >= 2000) %>%
  select(LTER_site, date, NO3., TP, DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(TP = as.numeric(TP) * 123.88/1000) %>% #umol/L to mg/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNITS = "mg/L",
         SITE_ID = "NWT_Albion_GLV",
         ECO_TYPE = "River/Stream",
         LAT = 40.0428746797462,
         LON = -105.59229602595) %>%
  select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         TP = mean(TP)) %>%
  ungroup() %>%
  select(-NO3.) %>%
  distinct() 


f <- read.csv("C:/Users/lrock1/Downloads/knb-lter-nwt.109.12/gre5solu.nc.data.csv")
f.1 <- f %>%
  filter(year >= 2000) %>%
  select(LTER_site, date, NO3., TP, DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(TP = as.numeric(TP) * 123.88/1000) %>% #umol/L to mg/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNITS = "mg/L",
         SITE_ID = "NWT_GreenLake_Outlet",
         ECO_TYPE = "River/Stream",
         LAT = 40.0536706761558,
         LON = -105.627913103381) %>%
  select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         TP = mean(TP)) %>%
  ungroup() %>%
  select(-NO3.) %>%
  distinct() 


g <- read.csv("C:/Users/lrock1/Downloads/knb-lter-nwt.112.4/martsolu.nc.data.csv")
g.1 <- g %>%
  filter(year >= 2000) %>%
  select(LTER_site, date, NO3., TP, DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(TP = as.numeric(TP) * 123.88/1000) %>% #umol/L to mg/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNITS = "mg/L",
         SITE_ID = "NWT_MartinelliBasin_Outlet",
         ECO_TYPE = "River/Stream",
         LAT = 40.0506020197246,
         LON = -105.595805026708) %>%
  select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         TP = mean(TP)) %>%
  ungroup() %>%
  select(-NO3.) %>%
  distinct() 


h <- read.csv("C:/Users/lrock1/Downloads/knb-lter-nwt.114.6/spilsolu.nc.data.csv")
h.1 <- h %>%
  filter(year >= 2000) %>%
  select(LTER_site, date, NO3., TP, DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(TP = as.numeric(TP) * 123.88/1000) %>% #umol/L to mg/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNITS = "mg/L",
         SITE_ID = "NWT_Albion_Spillway",
         ECO_TYPE = "River/Stream",
         LAT = 40.04287468,
         LON = -105.592296) %>%
  select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         TP = mean(TP)) %>%
  ungroup() %>%
  select(-NO3.) %>%
  distinct() 




i <- read.csv("C:/Users/lrock1/Downloads/knb-lter-nwt.160.3/saddsolu.nc.data.csv")
i.1 <- i %>%
  filter(year >= 2000) %>%
  select(LTER_site, date, NO3., TP, DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(TP = as.numeric(TP) * 123.88/1000) %>% #umol/L to mg/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNITS = "mg/L",
         SITE_ID = "NWT_Saddle_belowNiwot",
         ECO_TYPE = "River/Stream",
         LAT = 40.04903177,
         LON =-105.5923174) %>%
  select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         TP = mean(TP)) %>%
  ungroup() %>%
  select(-NO3.) %>%
  distinct() 



j <- read.csv("C:/Users/lrock1/Downloads/knb-lter-nwt.163.3/grrgsolu.nc.data.csv")
j.1 <- j %>%
  filter(year >= 2000) %>%
  select(LTER_site, date, NO3., TP, DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(TP = as.numeric(TP) * 123.88/1000) %>% #umol/L to mg/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNITS = "mg/L",
         SITE_ID = "NWT_GreenLake5_RockGlacier",
         ECO_TYPE = "River/Stream",
         LAT = 40.05213501,
         LON = -105.6289841) %>%
  select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         TP = mean(TP)) %>%
  ungroup() %>%
  select(-NO3.) %>%
  distinct() 



k <- read.csv("C:/Users/lrock1/Downloads/knb-lter-nwt.162.1/flumesolu.nc.data.csv")
k.1 <- k %>%
  filter(year >= 2000) %>%
  select(LTER_site, date, NO3., TP, DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(TP = as.numeric(TP) * 123.88/1000) %>% #umol/L to mg/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNITS = "mg/L",
         SITE_ID = "NWT_Flume_Outflow",
         ECO_TYPE = "River/Stream",
         LAT = 40.01368597,
         LON = -105.5578493) %>%
  select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         TP = mean(TP)) %>%
  ungroup() %>%
  select(-NO3.) %>%
  distinct() 



l <- read.csv("C:/Users/lrock1/Downloads/edi.1006.1/Waterchem.csv")
l.1 <- l %>%
 filter(Analysis == "DOC" |
          Analysis == "Nitrate" | 
          Analysis == "TP") %>%
  mutate(Concentration = ifelse(units == "ug/L", Concentration / 1000, Concentration)) %>%
  mutate(DATE_COL = as.Date(Date),
         SITE_ID = paste("Huron_", Collection.Site.or.Zone, sep = "")) %>%
  mutate(SITE_ID = gsub(" ", "", SITE_ID)) %>%
  pivot_wider(id_cols = c(DATE_COL, SITE_ID), names_from = Analysis, values_from = Concentration) %>%
  rename(`NO3 as N` = Nitrate) %>%
  drop_na() %>%
  mutate(UNITS = "mg/L",
         ECO_TYPE = "Lake")

l.sites <- read.csv("C:/Users/lrock1/Downloads/edi.1006.1/Site_locations.csv") %>%
  mutate(SITE_ID = paste("Huron_", Collection.Site.or.Zone, sep = "")) %>%
  mutate(SITE_ID = gsub(" ", "", SITE_ID)) %>%
  select(-Collection.Site.or.Zone) %>%
  rename(LAT = Latitude.Centroid,
         LON = Longitude.Centroid) %>%
  mutate(SITE_ID = ifelse(SITE_ID == "Huron_PointUrie", "Huron_UriePoint", SITE_ID))

l.2 <- left_join(l.1, l.sites) %>%
  distinct()



m <- read.csv("C:/Users/lrock1/Downloads/knb-lter-nwt.104.13/ariksolu.nc.data.csv")
m.1 <- m %>%
  filter(year >= 2000) %>%
  select(LTER_site, date, NO3., TP, DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(TP = as.numeric(TP) * 123.88/1000) %>% #umol/L to mg/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNITS = "mg/L",
         SITE_ID = "NWT_Arikaree_Drain",
         ECO_TYPE = "River/Stream",
         LAT = 40.0505740259853,
         LON = -105.642890924836) %>%
  select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         TP = mean(TP)) %>%
  ungroup() %>%
  select(-NO3.) %>%
  distinct() 



n <- read.csv("C:/Users/lrock1/Downloads/knb-lter-nwt.107.10/gre1solu.nc.data.csv")
n.1 <- n %>%
  filter(year >= 2000) %>%
  select(LTER_site, date, NO3., TP, DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(TP = as.numeric(TP) * 123.88/1000) %>% #umol/L to mg/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNITS = "mg/L",
         SITE_ID = "NWT_GreenLake1_Outflow",
         ECO_TYPE = "River/Stream",
         LAT = 40.0615,
         LON = -105.643) %>%
  select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         TP = mean(TP)) %>%
  ungroup() %>%
  select(-NO3.) %>%
  distinct() 




o <- read.csv("C:/Users/lrock1/Downloads/knb-lter-nwt.108.12/gre4solu.nc.data.csv")
o.1 <- o %>%
  filter(year >= 2000) %>%
  select(LTER_site, date, NO3., TP, DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * (62.0049/1000) * 0.2259) %>% #ueq NO3/L to mg N/L
  mutate(TP = as.numeric(TP) * 123.88/1000) %>% #umol/L to mg/L
  mutate(DOC = as.numeric(DOC)) %>%
  drop_na() %>%
  mutate(UNITS = "mg/L",
         SITE_ID = "NWT_GreenLake4_Outflow",
         ECO_TYPE = "River/Stream",
         LAT = 40.0558068853579,
         LON = -105.621371408807) %>%
  select(-LTER_site) %>%
  rename(DATE_COL = date) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  group_by(DATE_COL) %>%
  mutate(`NO3 as N` = mean(NO3.),
         DOC = mean(DOC),
         TP = mean(TP)) %>%
  ungroup() %>%
  select(-NO3.) %>%
  distinct() 



p <- read.csv("C:/Users/lrock1/Downloads/knb-lter-knz.50.13/glvwatsolu.dm.data.csv")

