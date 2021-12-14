#LTER datasets

#NTL LTER
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
         TP = totpuf,
         SITE_ID = lakeid) %>%
  select(-totpf) %>%
  filter(DATE_COL >= "2012-01-01") #avoid overlap with LAGOS
#need to get lat lons!!



#
b <- read.csv("C:/Users/lrock1/Downloads/knb-lter-nwt.213.1/soddsolu.mw.data.csv")
b.1 <- b %>%
  select(LTER_site, samp_loc, date, NO3., TP, DOC) %>%
  mutate(NO3. = as.numeric(NO3.) * 62.0049/1000) %>% #ueq/L to mg/L
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
