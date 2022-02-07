#European data

library(tidyverse)
library(lubridate)

EU <- read.csv("Data/EU_waterbase/data.csv")

EU.1 <- EU %>%
  filter(parameterWaterBodyCategory == "LW" | #lakes
           parameterWaterBodyCategory == "RW") %>% # rivers
  filter(observedPropertyDeterminandLabel == "Dissolved organic carbon (DOC)" |
           observedPropertyDeterminandLabel == "Nitrate" |
           observedPropertyDeterminandLabel == "Phosphate") #phosphate is available
  
water <- as.data.frame(unique(EU.1$procedureAnalysedMatrix)) #good, just water samples, W and W-DIS (dissolved)
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
sites <- read.csv("Data/EU_waterbase/spatial.csv")

sites.a <- sites %>%
  select(monitoringSiteIdentifier, lat, lon) %>%
  rename(SITE_ID = monitoringSiteIdentifier)
  
EU.i <- left_join(EU.3, sites.a) %>%
  mutate(across(c("lat", "lon"), ~na_if(., "")))

EU.i.2 <- EU.i %>%
  drop_na() #there are lots of NAs on locational info :( ~10K


site_unique <- EU.i.2 %>%
  select(lat, lon) %>%
  distinct()

site_unique <- site_unique %>%
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
  rename(`PO4 as P` = `Phosphate`,
         `NO3 as N` = Nitrate,
         DOC = `Dissolved organic carbon (DOC)`,
         LAT = lat,
         LON = lon,
         ECO_TYPE = parameterWaterBodyCategory) %>%
  mutate(ECO_TYPE = ifelse(ECO_TYPE == "LW", "Lake", "River/Stream"),
         UNITS = "mg/L")



write.csv(EU.5, "Data/Simplified_datasets_per_source/SIMPLE_EU.csv")


#how many sites that contain DOC, NO3, and PO4? 2602
check.1 <- EU.5 |>
  select(SITE_ID, LAT, LON) |>
  distinct()


################################################################################
#create dataset with all variables but renamed DOC, PO4 as P, and NO3 as N

all_data <- EU |>
  filter(parameterWaterBodyCategory %in% c("LW", "RW")) |> #lakes & rivers
  filter(metadata_observationStatus == "A") |>
  select(ï..monitoringSiteIdentifier, observedPropertyDeterminandLabel, resultUom, phenomenonTimeReferenceYear, resultMeanValue, parameterWaterBodyCategory) %>%
  rename(SITE_ID = ï..monitoringSiteIdentifier,
  DATE_COL = phenomenonTimeReferenceYear,
  VARIABLE = observedPropertyDeterminandLabel) |>
  mutate(resultMeanValue = ifelse(VARIABLE == "Nitrate", resultMeanValue * 0.2259, resultMeanValue)) |>
  filter(DATE_COL >= 2000) |>
  distinct() |>
  mutate(VARIABLE = ifelse(VARIABLE == 'Nitrate', 'NO3 as N', VARIABLE),
         VARIABLE = ifelse(VARIABLE == 'Phosphate', 'PO4 as P', VARIABLE),
         VARIABLE = ifelse(VARIABLE == 'Dissolved organic carbon (DOC)', 'DOC', VARIABLE)) |>
  mutate(resultUom = ifelse(VARIABLE %in% c('NO3 as N', 'PO4 as P', 'DOC'), "mg/L", resultUom)) 
  
units <- as.data.frame(unique(all_data$resultUom))

#only keep sites with all the cnp data
all_data2 <- all_data |>
  left_join(sites.a) |> #combine with site info
  select(-SITE_ID) # just keep lat and long because I renamed the sites on lines 31-50

all_data3 <- all_data2|> 
  rename(LAT = lat,
         LON = lon) |> #make sure columns have same names
  left_join(check.1) |>
  drop_na(SITE_ID) # get rid of any rows that are not at sites with the essential data

#jsut double check we have all the sites
check.2 <- all_data3 |>
  select(SITE_ID, LAT, LON) |>
  distinct() #YAY! all 2602 are here :) 

#finally, rename columns to match other datasets 
all_data4 <- all_data3 |>
  rename(ECO_TYPE = parameterWaterBodyCategory,
         UNIT = resultUom,
         RESULT = resultMeanValue) |>
  mutate(ECO_TYPE = ifelse(ECO_TYPE == "LW", "Lake", "River/Stream"))

#reorder columns:
all_dataFINAL <- all_data4 |>
  select(DATE_COL, SITE_ID, VARIABLE, UNIT, RESULT, LAT, LON, ECO_TYPE)

write.csv(all_dataFINAL, "Data/other_vars_datasets/EU.csv")


