library(tidyverse)
library(dataRetrieval)

#### doc ####
wqp <- readWQPdata(siteType = "Stream",
                       characteristicName = "Organic%20carbon")

wqpDOC <- wqp |>
  select(ActivityStartDate, MonitoringLocationIdentifier, CharacteristicName, ResultSampleFractionText, ResultMeasureValue, ResultMeasure.MeasureUnitCode, ResultStatusIdentifier) |>
  drop_na(ResultMeasureValue)

rm(wqp)

wqpDOC2 <- wqpDOC |>
  drop_na(ResultMeasureValue)

#keep only legitimate data
ids <- as.data.frame(unique(WQP.n1$ResultStatusIdentifier)) # seems fine for now


params <- as.data.frame(unique(wqpDOC2$CharacteristicName))
locations <- as.data.frame(unique(wqpDOC2$MonitoringLocationIdentifier)) #95033
fractions <- wqpDOC2 |> count(ResultSampleFractionText)

wqpDOC_3 <- wqpDOC2 |> 
  filter(ResultSampleFractionText %in% c("Dissolved","Filtered, lab", "Filterable", "Filtered, field"))

units <- wqpDOC_3 |> count(ResultMeasure.MeasureUnitCode)

wqpDOC_4 <- wqpDOC_3|>
  filter(ResultMeasure.MeasureUnitCode %in% c("mg/l", "ug/l", "ppm")) |>
  filter(ActivityStartDate >= "2000-01-01")
unique(wqpDOC_4$ResultStatusIdentifier) # "Final" "Preliminary" "Accepted"    "Historical"  "Validated"   "Provisional" "Unreviewed" 


wqpDOC_5 <- wqpDOC_4 |>
  mutate(DOC = ifelse(str_detect(ResultMeasureValue, "^<"), gsub("<", "", ResultMeasureValue), ResultMeasureValue)) |>
  mutate(DOC = ifelse(str_detect(ResultMeasureValue, "^\\."), gsub("\\.", "0.", ResultMeasureValue), DOC)) |>
  mutate(DOC = as.numeric(DOC)) |>
  filter(DOC > 0,
         !is.na(DOC)) |>
  mutate(DOC = ifelse(ResultMeasure.MeasureUnitCode == "ug/l", DOC/1000, DOC),
         UNITS = "mg/L",
         ECO_TYPE = "River/Stream") |>
  mutate(DATE_COL = as.Date(ActivityStartDate)) |>
  rename(SITE_ID = MonitoringLocationIdentifier) |>
  select(DATE_COL, SITE_ID, DOC, UNITS, ECO_TYPE) |>
  drop_na()
    
# Needs lat/long




#### nitrate ####
wqp <- readWQPdata(siteType = "Stream",
                   characteristicName = c("Nitrate", "Nitrate-N","Nitrate%20as%20N","Nitrate-Nitrite","Nitrate-nitrogen","Nitrate-Nitrogen","Nitrate%20%2B%20Nitrite"),
                   startDate = "2000-01-01")

wqpNO3 <- wqp |>
  select(ActivityStartDate, MonitoringLocationIdentifier, CharacteristicName, ResultSampleFractionText, ResultMeasureValue, ResultMeasure.MeasureUnitCode, ResultStatusIdentifier) |>
  drop_na(ResultMeasureValue)

rm(wqp)

#keep only legitimate data
ids <- as.data.frame(unique(wqpNO3$ResultStatusIdentifier)) 

wqpNO3_2 <- wqpNO3 |>
  filter(ResultStatusIdentifier != "Rejected")

#First find any NO3 that have concurrently collected DOC
overlapNO3_DOC <- wqpNO3_2 |>
  mutate(DATE_COL = as.Date(ActivityStartDate)) |>
  rename(SITE_ID = MonitoringLocationIdentifier) |>
  left_join(wqpDOC_5) |>
  drop_na()
fractions <- overlapNO3_DOC |> count(ResultSampleFractionText)
units <- overlapNO3_DOC |> count(ResultMeasure.MeasureUnitCode)
names <- overlapNO3_DOC |> count(CharacteristicName)


wqpNO3_DOC <- overlapNO3_DOC |>
  filter(ResultMeasure.MeasureUnitCode %in% c("mg/l as N", "mg/l asNO3", "mg N/l******") |
           CharacteristicName == "Nitrate as N") |>
  mutate(NO3.as.N = ifelse(str_detect(ResultMeasureValue, "^<"), gsub("<", "", ResultMeasureValue), ResultMeasureValue)) |>
  mutate(NO3.as.N = ifelse(str_detect(ResultMeasureValue, "^\\."), gsub("\\.", "0.", ResultMeasureValue), NO3.as.N)) |>
  mutate(NO3.as.N = as.numeric(NO3.as.N)) |>
  filter(NO3.as.N > 0,
         !is.na(NO3.as.N)) 
units <- wqpNO3_DOC |> count(ResultMeasure.MeasureUnitCode)


wqpnNO3_DOC_2 <- wqpNO3_DOC |>
  mutate(NO3.as.N = ifelse(ResultMeasure.MeasureUnitCode == "mg/l asNO3", NO3.as.N * 0.2259, NO3.as.N)) |> # convert NO3 as NO3 to NO3 as N
  mutate(NO3.as.N = ifelse(ResultMeasure.MeasureUnitCode == "ueq/L", NO3.as.N * 14.007/1000, NO3.as.N)) |> # convert NO3 as N ueq/L to NO3 as N mg/L
  select(DATE_COL, SITE_ID, DOC, NO3.as.N, UNITS, ECO_TYPE)






#### phosphate ####
wqp <- readWQPdata(siteType = "Stream",
                   characteristicName = c("Orthophosphate","Orthophosphate%20as%20PO4","Phosphate","Soluble%20Reactive%20Phosphorus%20(SRP)"),
                   startDate = "2000-01-01")

wqpPO4 <- wqp |>
  select(ActivityStartDate, MonitoringLocationIdentifier, CharacteristicName, ResultSampleFractionText, ResultMeasureValue, ResultMeasure.MeasureUnitCode, ResultStatusIdentifier) |>
  drop_na(ResultMeasureValue)

rm(wqp)

#keep only legitimate data
ids <- as.data.frame(unique(wqpPO4$ResultStatusIdentifier)) 

wqpPO4_2 <- wqpPO4 |>
  filter(ResultStatusIdentifier != "Rejected")



#First find any PO4 concurrently collected with DOC, NO3
WQP_add <- wqpPO4_2 |>
  mutate(DATE_COL = as.Date(ActivityStartDate)) |>
  rename(SITE_ID = MonitoringLocationIdentifier) |>
  left_join(wqpnNO3_DOC_2 ) |>
  drop_na()
fractions <- WQP_add |> count(ResultSampleFractionText)
units <- WQP_add |> count(ResultMeasure.MeasureUnitCode)
names <- WQP_add |> count(CharacteristicName)


WQP_add2 <- WQP_add |>
  mutate(PO4.as.P = ifelse(str_detect(ResultMeasureValue, "^<"), gsub("<", "", ResultMeasureValue), ResultMeasureValue)) |>
  mutate(PO4.as.P = ifelse(str_detect(ResultMeasureValue, "^\\."), gsub("\\.", "0.", ResultMeasureValue), PO4.as.P)) |>
  mutate(PO4.as.P = as.numeric(PO4.as.P)) |>
  filter(PO4.as.P > 0,
         !is.na(PO4.as.P)) |>
  filter(ResultMeasure.MeasureUnitCode != "mg/l") |>
  mutate(PO4.as.P = ifelse(ResultMeasure.MeasureUnitCode == "mg/l asPO4", PO4.as.P * 0.3261, PO4.as.P))




# keeping ids from this
ids <- as.data.frame(unique(wqpNO3_2$ResultStatusIdentifier)) 
ids
##    Accepted
##       Final
## Preliminary
##  Historical
##   Validated
##  Unreviewed
## Provisional
##         Raw




FINAL_no_latlong <- WQP_add2 |>
  select(DATE_COL, SITE_ID, DOC, NO3.as.N, PO4.as.P, UNITS, ECO_TYPE) |>
  distinct() |>
  group_by(DATE_COL, SITE_ID, UNITS, ECO_TYPE) |>
  summarise(DOC = mean(DOC),
            NO3.as.N = mean(NO3.as.N),
            PO4.as.P = mean(PO4.as.P)) |>
  distinct()
write.csv(FINAL_no_latlong, "Code/LRock/WQP_datapull_needlatlong.csv")


checksite <- readWQPdata("USGS-05458150",
   characteristicName = "Organic%20carbon")
#################################################################################
# add lat long to dataset 
source("Code/THE_DATA.R") # check out all the data

wqp <- read.csv("Code/LRock/WQP_datapull_needlatlong.csv")

site_list <- unique(wqp$SITE_ID) # 2772 rivers

site_info <- read.csv("C:/Users/lrock1/downloads/station/station.csv") |>
  select(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure) |>
  rename(SITE_ID = MonitoringLocationIdentifier,
         LAT = LatitudeMeasure,
         LON = LongitudeMeasure)

wqp1 <- wqp |>
  left_join(site_info) |>
  distinct()

# check the sites
library(sf)
library(ggspatial)
library(raster)
library(rnaturalearth)
library(rgeos)

states <- ne_countries(country = "United States of America", returnclass = "sf")

WQP_sites_sf <- wqp1 %>%
  st_as_sf(coords = c("LON", "LAT"), crs = 4326)


ggplot() +
  geom_sf(states, mapping = aes(), fill = "white") +
  geom_sf(WQP_sites_sf, mapping = aes(color = ECO_TYPE)) +
  theme_bw()

