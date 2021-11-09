#script to build master datasheet
library(tidyverse)

#NLA data
nla_2012 <- read.csv("Data/NLA/NLA2012_combinedData.csv")
nla_2012.1 <- nla_2012 %>%
  select(SITE_ID, DOC_UNITS, DOC_RESULT, NITRATE_N_UNITS, NITRATE_N_RESULT, PTL_UNITS, PTL_RESULT, DOC_LAB_FLAG, NITRATE_N_LAB_FLAG, PTL_LAB_FLAG, LAT_DD83, LON_DD83)

nla_2017 <- read.csv("Data/NLA/Original_ChemData_NLA2017.csv") 
nla_2017.1 <- nla_2017 %>%
  select(SITE_ID, DATE_COL, ANALYTE, RESULT, RESULT_UNITS, NARS_FLAG)
