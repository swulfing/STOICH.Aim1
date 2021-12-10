#European data

library(tidyverse)
library(lubridate)

EU <- read.csv("C:/Users/lrock1/OneDrive/Desktop/EU_waterbase/data.csv")

EU.1 <- EU %>%
  filter(parameterWaterBodyCategory == "LW" | #lakes
           parameterWaterBodyCategory == "RW") %>% # rivers
  filter(observedPropertyDeterminandLabel == "Dissolved organic carbon (DOC)" |
           observedPropertyDeterminandLabel == "Nitrate" |
           observedPropertyDeterminandLabel == "Total phosphorus") #phosphate is available
  
water <- as.data.frame(unique(EU.1$procedureAnalysedMatrix)) #good, just water samples
status <- as.data.frame(unique(EU.1$metadata_observationStatus)) #get rid of Us

EU.2 <- EU.1 %>%
  filter(metadata_observationStatus == "A") %>%
  select(ï..monitoringSiteIdentifier, observedPropertyDeterminandLabel, resultUom, phenomenonTimeReferenceYear, resultMeanValue, parameterWaterBodyCategory) %>%
  rename(SITE_ID = ï..monitoringSiteIdentifier,
         DATE_COL = phenomenonTimeReferenceYear)

units <- as.data.frame(unique(EU.2$resultUom)) #all mg/L -- need to change nitrate to mg N/L instead of mg No3/L

EU.3 <- EU.2 %>%
  mutate(resultMeanValue = ifelse(observedPropertyDeterminandLabel == "Nitrate", resultMeanValue * 0.2259, resultMeanValue)) %>%
  filter(DATE_COL >= 2000) %>%
  distinct()

#site ids right now are more than just location, some of them differ (maybe based on sampling time or some other factor) so I need to get more detailed info from the spatial dataset
sites <- read.csv("C:/Users/linne/OneDrive/Desktop/EU_waterbase/spatial.csv")

sites.a <- sites %>%
  select(monitoringSiteIdentifier, lat, lon) %>%
  rename(SITE_ID = monitoringSiteIdentifier)
  
EU.i <- left_join(EU.3, sites.a) %>%
  mutate(across(c("lat", "lon"), ~na_if(., "")))

EU.i.2 <- EU.i %>%
  drop_na() #there are lots of NAs on locational info :( ~10K

site_unique <- EU.i.2 %>%
  select(lat, lon) %>%
  distinct() %>%
  mutate(SITE_ID = paste("EU_", seq(1:nrow(site_unique)), sep = "")) #renaming the SITE_ID because of the issue reported on line 31
  
EU.i.3 <- EU.i.2 %>%
  select(-SITE_ID) %>%
  left_join(site_unique) %>%
  distinct() %>%
  group_by(SITE_ID, DATE_COL, lat, lon, observedPropertyDeterminandLabel, parameterWaterBodyCategory) %>%
  mutate(Result = mean(resultMeanValue)) %>%
  ungroup() %>%
  select(-resultMeanValue) %>%
  distinct()

EU.4 <- EU.i.3 %>%
  pivot_wider(id_cols = c(DATE_COL, SITE_ID, lat, lon, parameterWaterBodyCategory), names_from = observedPropertyDeterminandLabel, values_from = Result)

#the finalized dataset
EU.5 <- EU.4 %>%
  drop_na() %>%
  rename(TP = `Total phosphorus`,
         `NO3 as N` = Nitrate,
         DOC = `Dissolved organic carbon (DOC)`,
         LAT = lat,
         LON = lon,
         ECO_TYPE = parameterWaterBodyCategory) %>%
  mutate(ECO_TYPE = ifelse(ECO_TYPE == "LW", "Lake", "River/Stream"),
         UNITS = "mg/L")

write.csv(EU.5, "Data/Simplified_datasets_per_source/SIMPLE_EU.csv")
