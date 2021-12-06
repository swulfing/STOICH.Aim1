library(tidyverse)
library(scorecard)


WQP.n <- read.csv("C:/Users/linne/Downloads/result (1)/result.csv") 


WQP.n1 <- WQP.n %>%
  select(ActivityStartDate, MonitoringLocationIdentifier, CharacteristicName, ResultSampleFractionText, ResultMeasureValue, ResultMeasure.MeasureUnitCode, ResultStatusIdentifier)

#keep only legitimate data
ids <- as.data.frame(unique(WQP.n1$ResultStatusIdentifier))

WQP.n2 <- WQP.n1 %>%
  filter(ResultStatusIdentifier != "Provisional" &
           ResultStatusIdentifier != "Validated" &
           ResultStatusIdentifier != "Rejected" &
           ResultStatusIdentifier != "Preliminary")
ids <- as.data.frame(unique(WQP.n2$ResultStatusIdentifier))

#checking what we have
params <- as.data.frame(unique(WQP.n2$CharacteristicName))
locations <- as.data.frame(unique(WQP.n2$MonitoringLocationIdentifier))
fractions <- as.data.frame(unique(WQP.n2$ResultSampleFractionText))
units <- as.data.frame(unique(WQP.n2$ResultMeasure.MeasureUnitCode))

#get rid of anything with weird, non-convert-able units
WQP.n3 <- WQP.n2 %>%
  na_if("") %>%
  filter(!is.na(ResultMeasure.MeasureUnitCode) &
           ResultMeasure.MeasureUnitCode != "ueq/L" &
           ResultMeasure.MeasureUnitCode != "umol" &
           ResultMeasure.MeasureUnitCode != "None"&
           ResultMeasure.MeasureUnitCode != "count"&
           ResultMeasure.MeasureUnitCode != "MPN/100ml")
units <- as.data.frame(unique(WQP.n3$ResultMeasure.MeasureUnitCode))


WQP.n4 <- WQP.n3 %>%
  mutate(ResultMeasure.MeasureUnitCode = ifelse(ResultMeasure.MeasureUnitCode == "ppb", "ug/l", ResultMeasure.MeasureUnitCode),
         ResultMeasure.MeasureUnitCode = ifelse(ResultMeasure.MeasureUnitCode == "ppm", "mg/l", ResultMeasure.MeasureUnitCode),
         ResultMeasure.MeasureUnitCode = ifelse(ResultMeasure.MeasureUnitCode == "mg/L", "mg/l", ResultMeasure.MeasureUnitCode))


params <- as.data.frame(unique(WQP.n4$CharacteristicName))
locations <- as.data.frame(unique(WQP.n4$MonitoringLocationIdentifier))
fractions <- as.data.frame(unique(WQP.n4$ResultSampleFractionText))
units <- as.data.frame(unique(WQP.n4$ResultMeasure.MeasureUnitCode))  


#find real values - there are a lot of words and symbols in the dataset 
WQP.n5 <- WQP.n4 %>%
  mutate(ActivityStartDate = as.Date(ActivityStartDate)) %>%
  mutate(ResultMeasureValue = as.character(ResultMeasureValue)) %>%
  mutate(realResult = ifelse(str_detect(ResultMeasureValue, "^[<01234567789]"), "Y", "N")) %>% #find anything that begins with a number, or begins with a decimal (because apparently people report some numbers decimal first eyeroll)
  mutate(realResult = ifelse(str_detect(ResultMeasureValue, "\\."), "Y", realResult)) %>%
  filter(realResult == "Y")


units <- as.data.frame(unique(WQP.n5$ResultMeasure.MeasureUnitCode))
# check <- WQP.n6 %>%
#   filter(str_detect(Result, "^\\."))

WQP.n6 <- WQP.n5 %>%
  mutate(Result = ifelse(str_detect(ResultMeasureValue, "^<"), gsub("<", "", ResultMeasureValue), ResultMeasureValue)) %>%
  mutate(Result = ifelse(str_detect(ResultMeasureValue, "^\\."), gsub("\\.", "0.", ResultMeasureValue), Result)) 

units <- as.data.frame(unique(WQP.n6$ResultMeasure.MeasureUnitCode))

#keep values greater than 0 
WQP.n7 <- WQP.n6 %>%
  mutate(Result = as.numeric(Result)) %>%
  filter(Result > 0,
         !is.na(Result))

units <- as.data.frame(unique(WQP.n7$ResultMeasure.MeasureUnitCode))


WQP.n8 <- WQP.n7 %>%
  mutate(Result = ifelse(ResultMeasure.MeasureUnitCode == "mg/l asNO3", Result * 0.2259, Result),
         NO3_UNIT = ifelse(ResultMeasure.MeasureUnitCode == "mg/l asNO3", "mg/L", ResultMeasure.MeasureUnitCode),
         NO3_UNIT = ifelse(ResultMeasure.MeasureUnitCode == "mg/l as N", "mg/L", NO3_UNIT),
         Result = ifelse(ResultMeasure.MeasureUnitCode == "ug/l", Result / 1000, Result),
         NO3_UNIT = ifelse(CharacteristicName == "Nitrate-N", "mg/L", NO3_UNIT),
         NO3_UNIT = ifelse(CharacteristicName == "Nitrate as N", "mg/L", NO3_UNIT))

WQP.test <- WQP.n8 %>%
  filter(NO3_UNIT == "mg/L") %>%
  rename(DATE_COL = ActivityStartDate,
         NO3 = Result) %>%
  select(DATE_COL, MonitoringLocationIdentifier, NO3, NO3_UNIT) %>%
  filter(DATE_COL < Sys.Date())


sites.n <- read.csv("C:/Users/linne/Downloads/station (1)/station.csv") 
sites1 <- sites.n %>%
  select(MonitoringLocationIdentifier, MonitoringLocationTypeName, LatitudeMeasure, LongitudeMeasure)


combine <- left_join(WQP.test, sites1)  

combine1 <- combine %>%
  select(MonitoringLocationIdentifier, MonitoringLocationTypeName, LatitudeMeasure, LongitudeMeasure) %>%
  distinct() %>%
  rename(LAT = LatitudeMeasure,
         LON = LongitudeMeasure,
         ECO_TYPE = MonitoringLocationTypeName)

#############################################################################################################################################
WQP_all_data <- combine %>%
  rename(LAT = LatitudeMeasure,
         LON = LongitudeMeasure,
         ECO_TYPE = MonitoringLocationTypeName,
         SITE_ID = MonitoringLocationIdentifier) %>%
  mutate(ECO_TYPE = ifelse(ECO_TYPE == "Channelized Stream", "River/Stream", ECO_TYPE)) %>%
  mutate(Year = year(DATE_COL))

  
  
  
  


