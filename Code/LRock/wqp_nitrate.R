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
##pick up here###############################################################################################################

#keep only DOC
DOC <- WQP.n3 %>%
  filter(CharacteristicName == "Organic carbon") %>%
  filter(ResultSampleFractionText == "Dissolved")


WQP.n4 <- WQP.n3 %>%
  filter(CharacteristicName != "Organic carbon") %>%
  rbind(DOC) %>%
  mutate(ResultMeasure.MeasureUnitCode = ifelse(ResultMeasure.MeasureUnitCode == "ppb", "ug/l", ResultMeasure.MeasureUnitCode),
         ResultMeasure.MeasureUnitCode = ifelse(ResultMeasure.MeasureUnitCode == "ppm", "mg/l", ResultMeasure.MeasureUnitCode))


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

check <- WQP.n6 %>%
  filter(str_detect(Result, "^\\."))

WQP.n6 <- WQP.n5 %>%
  mutate(Result = ifelse(str_detect(ResultMeasureValue, "^<"), gsub("<", "", ResultMeasureValue), ResultMeasureValue)) %>%
  mutate(Result = ifelse(str_detect(ResultMeasureValue, "^\\."), gsub("\\.", "0.", ResultMeasureValue), Result)) 

#keep values greater than 0 
WQP.n7 <- WQP.n6 %>%
  mutate(Result = as.numeric(Result)) %>%
  filter(Result > 0,
         !is.na(Result))

