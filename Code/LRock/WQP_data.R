library(tidyverse)
library(scorecard)


WQP <- read.csv("C:/Users/linne/Downloads/result/result.csv") 


WQP1 <- WQP %>%
  select(ActivityStartDate, MonitoringLocationIdentifier, CharacteristicName, ResultSampleFractionText, ResultMeasureValue, ResultMeasure.MeasureUnitCode, ResultStatusIdentifier)
  
#keep only legitimate data
ids <- as.data.frame(unique(WQP1$ResultStatusIdentifier))

WQP2 <- WQP1 %>%
  filter(ResultStatusIdentifier != "Provisional" &
           ResultStatusIdentifier != "Validated" &
           ResultStatusIdentifier != "Rejected")
ids <- as.data.frame(unique(WQP2$ResultStatusIdentifier))

#checking what we have
params <- as.data.frame(unique(WQP2$CharacteristicName))
locations <- as.data.frame(unique(WQP2$MonitoringLocationIdentifier))
fractions <- as.data.frame(unique(WQP2$ResultSampleFractionText))
units <- as.data.frame(unique(WQP2$ResultMeasure.MeasureUnitCode))

#get rid of anything with weird, non-convert-able units
WQP3 <- WQP2 %>%
  na_if("") %>%
  filter(!is.na(ResultMeasure.MeasureUnitCode) &
           ResultMeasure.MeasureUnitCode != "%" &
           ResultMeasure.MeasureUnitCode != "mg/l CaCO3" &
           ResultMeasure.MeasureUnitCode != "None")
units <- as.data.frame(unique(WQP3$ResultMeasure.MeasureUnitCode))


#keep only DOC
DOC <- WQP3 %>%
  filter(CharacteristicName == "Organic carbon") %>%
  filter(ResultSampleFractionText == "Dissolved")


WQP4 <- WQP3 %>%
  filter(CharacteristicName != "Organic carbon") %>%
  rbind(DOC) %>%
  mutate(ResultMeasure.MeasureUnitCode = ifelse(ResultMeasure.MeasureUnitCode == "ppb", "ug/l", ResultMeasure.MeasureUnitCode),
         ResultMeasure.MeasureUnitCode = ifelse(ResultMeasure.MeasureUnitCode == "ppm", "mg/l", ResultMeasure.MeasureUnitCode))
  
  
params <- as.data.frame(unique(WQP4$CharacteristicName))
locations <- as.data.frame(unique(WQP4$MonitoringLocationIdentifier))
fractions <- as.data.frame(unique(WQP4$ResultSampleFractionText))
units <- as.data.frame(unique(WQP4$ResultMeasure.MeasureUnitCode))  


#find real values - there are a lot of words and symbols in the dataset 
WQP5 <- WQP4 %>%
 mutate(ActivityStartDate = as.Date(ActivityStartDate)) %>%
 mutate(ResultMeasureValue = as.character(ResultMeasureValue)) %>%
 mutate(realResult = ifelse(str_detect(ResultMeasureValue, "^[<01234567789]"), "Y", "N")) %>% #find anything that begins with a number, or begins with a decimal (because apparently people report some numbers decimal first eyeroll)
 mutate(realResult = ifelse(str_detect(ResultMeasureValue, "\\."), "Y", realResult)) %>%
 filter(realResult == "Y")

check <- WQP6 %>%
  filter(str_detect(Result, "^\\."))

WQP6 <- WQP5 %>%
  mutate(Result = ifelse(str_detect(ResultMeasureValue, "^<"), gsub("<", "", ResultMeasureValue), ResultMeasureValue)) %>%
  mutate(Result = ifelse(str_detect(ResultMeasureValue, "^\\."), gsub("\\.", "0.", ResultMeasureValue), Result)) 

#keep values greater than 0 
WQP7 <- WQP6 %>%
  mutate(Result = as.numeric(Result)) %>%
  filter(Result > 0,
         !is.na(Result))

#ready for pivoting 
#DOC
wqpDOC <- WQP7 %>%
  filter(CharacteristicName == "Organic carbon") %>%
  pivot_wider(id_cols = c(ActivityStartDate, MonitoringLocationIdentifier, ResultMeasure.MeasureUnitCode, ResultSampleFractionText), names_from = CharacteristicName, values_from = Result)%>%
  unnest() %>%
  rename(doc = `Organic carbon`,
         units = ResultMeasure.MeasureUnitCode)

units <- as.data.frame(unique(wqpDOC$units))

wqpDOC <- wqpDOC %>%
  mutate(DOC = ifelse(units == "ug/l", doc * 1000, doc)) %>%
  mutate(DOC_UNITS = "mg/L") %>%
  select(-c(doc, units, ResultSampleFractionText)) %>%
  rename(DATE_COL = ActivityStartDate)

  
#NO3
wqpNO3 <- WQP7 %>%
  filter(CharacteristicName == "Total Nitrogen, mixed forms") %>%
  pivot_wider(id_cols = c(ActivityStartDate, MonitoringLocationIdentifier, ResultMeasure.MeasureUnitCode, ResultSampleFractionText), names_from = CharacteristicName, values_from = Result)%>%
  unnest() %>%
  rename(no3 = `Total Nitrogen, mixed forms`,
         units = ResultMeasure.MeasureUnitCode)

units <- as.data.frame(unique(wqpNO3$units))

wqpNO3 <- wqpNO3 %>%
  mutate(NO3 = ifelse(units == "ug/l", no3 * 1000, no3)) %>%
  mutate(NO3_UNITS = "mg/L")  %>%
  select(-c(no3, units, ResultSampleFractionText)) %>%
  rename(DATE_COL = ActivityStartDate)

#TP
wqpTP <- WQP7 %>%
  filter(CharacteristicName == "Total Nitrogen, mixed forms") %>%
  pivot_wider(id_cols = c(ActivityStartDate, MonitoringLocationIdentifier, ResultMeasure.MeasureUnitCode, ResultSampleFractionText), names_from = CharacteristicName, values_from = Result)%>%
  unnest() %>%
  rename(tp = `Total Nitrogen, mixed forms`,
         units = ResultMeasure.MeasureUnitCode)

units <- as.data.frame(unique(wqpTP$units))

wqpTP <- wqpTP %>%
  mutate(TP = ifelse(units == "ug/l", tp * 1000, tp)) %>%
  mutate(TP_UNITS = "mg/L") %>%
  select(-c(tp, units, ResultSampleFractionText)) %>%
  rename(DATE_COL = ActivityStartDate)
   
   

#combine --full dataset
WQP_CNP <- full_join(wqpTP, wqpNO3, by = c("DATE_COL", "MonitoringLocationIdentifier")) %>%
  full_join(wqpDOC, by = c("DATE_COL", "MonitoringLocationIdentifier")) %>%
  distinct() %>%
  drop_na()



#now get location, site information

sites <- read.csv("C:/Users/linne/Downloads/station/station.csv") 
sites1 <- sites %>%
  select(MonitoringLocationIdentifier, MonitoringLocationTypeName, LatitudeMeasure, LongitudeMeasure)


combine <- left_join(WQP_CNP, sites1)  

combine1 <- combine %>%
  select(MonitoringLocationIdentifier, MonitoringLocationTypeName, LatitudeMeasure, LongitudeMeasure) %>%
  distinct() %>%
  rename(LAT = LatitudeMeasure,
         LON = LongitudeMeasure,
         ECO_TYPE = MonitoringLocationTypeName)


WQP_all_data <- combine %>%
  rename(LAT = LatitudeMeasure,
         LON = LongitudeMeasure,
         ECO_TYPE = MonitoringLocationTypeName,
         SITE_ID = MonitoringLocationIdentifier) %>%
  mutate(ECO_TYPE = ifelse(ECO_TYPE == "Channelized Stream", "River/Stream", ECO_TYPE)) %>%
  mutate(Year = year(DATE_COL))



#this dataset is too large as a single csv
split_list <- split_df(WQP_all_data, ratios = c(0.25, 0.25, 0.25, 0.25), name_dfs = c("A", "B", "C", "D"))
A <- split_list$A
B <- split_list$B
C <- split_list$C
D <- split_list$D

write.csv(A, "Data/WQP_CNP_A.csv")
write.csv(B, "Data/WQP_CNP_B.csv")
write.csv(C, "Data/WQP_CNP_C.csv")
write.csv(D, "Data/WQP_CNP_D.csv")



combine2 <- combine1 %>%
  rename(SITE_ID = MonitoringLocationIdentifier)

write.csv(combine2, "Data/WQP_unique_sites.csv")
