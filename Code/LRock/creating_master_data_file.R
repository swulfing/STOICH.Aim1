#script to build master datasheet
library(tidyverse)

#NLA data####
#QUESTION: is all NLA data in mg/L??

#nla location info - more info in full metadata 
nla_sites_lakes <- read.csv("Data/NLA/NLA2012_combinedData.csv") %>%
  select(SITE_ID, LAT_DD83, LON_DD83) %>%
  distinct() %>%
  mutate(ECO_TYPE = "Lake")

nla_sites_rivers <- read.csv("Data/NLA/Metadata/Points_NLArs2008.csv") %>%
  select(SITE_ID, LAT_DD83, LON_DD83) %>%
  rbind(read.csv("Data/NLA/Metadata/Points_NLArs2019.csv")%>%
          select(SITE_ID, LAT_DD83, LON_DD83)) %>%
  rbind(read.csv("Data/NLA/Metadata/Points_NLAs2004.csv")%>%
          select(SITE_ID, LAT_DD83, LON_DD83)) %>%
  distinct() %>%
  mutate(ECO_TYPE = "River/Stream")


#nla data
nla_2012 <- read.csv("Data/NLA/NLA2012_combinedData.csv") %>%
  select(SITE_ID, DOC_UNITS, DOC_RESULT, NITRATE_N_UNITS, NITRATE_N_RESULT, PTL_UNITS, PTL_RESULT, DOC_LAB_FLAG, NITRATE_N_LAB_FLAG, PTL_LAB_FLAG) %>%
  mutate(YEAR = 2012)

nla_2017 <- read.csv("Data/NLA/Original_ChemData_NLA2017.csv") %>%
  select(SITE_ID, DATE_COL, ANALYTE, RESULT, RESULT_UNITS, NARS_FLAG) %>%
  mutate(YEAR = 2017)

nla_rs_2008 <- read.csv("Data/NLA/ChemData_NLArs2008.csv") %>%
  select(SITE_ID, DOC, NO3, PTL, DOC_ALERT, NO3_ALERT, PTL_ALERT) %>%
  mutate(YEAR = 2008)

nla_rs_2013 <- read.csv("Data/NLA/ChemData_NLArs2013.csv") %>%
  select(SITE_ID, DOC_RESULT_UNITS, DOC_RESULT, NITRATE_N_RESULT_UNITS, NITRATE_N_RESULT, PTL_RESULT_UNITS, PTL_RESULT, DOC_NARS_FLAG, NITRATE_N_NARS_FLAG, PTL_NARS_FLAG) %>%
  mutate(YEAR = 2013)

nla_rs_2004 <- read.csv("Data/NLA/ChemData_NLAs2004.csv") %>%
  select(SITE_ID, DOC, NO3, PTL, DOCF, NO3F, PTLF) %>%
  mutate(YEAR = 2004)

nla_rs_2019 <- read.csv("Data/NLA/ChemData_NLArs2019.csv") %>%
  select(SITE_ID, DOC_UNITS, DOC_RESULT, NITRATE_N_UNITS, NITRATE_N_RESULT, PTL_UNITS, PTL_RESULT, DOC_NARS_FLAG, NITRATE_N_NARS_FLAG, PTL_NARS_FLAG) %>%
  mutate(YEAR = 2019)

