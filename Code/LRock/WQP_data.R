library(tidyverse)


WQP <- read.csv("C:/Users/linne/Downloads/result/result.csv") 


WQP1 <- WQP %>%
  select(MonitoringLocationIdentifier, CharacteristicName, ResultSampleFractionText, ResultMeasureValue, ResultMeasure.MeasureUnitCode, ResultStatusIdentifier)
  
