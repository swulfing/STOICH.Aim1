#script to build master datasheet
library(tidyverse)
library(lubridate)



#NLA data####
#QUESTION: is all NLA data in mg/L?? -- no it's not.. ANNOYING

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

NLA_sites <- nla_sites_lakes %>%
  rbind(nla_sites_rivers)


#nla data
nla_2012 <- read.csv("Data/NLA/NLA2012_combinedData.csv") %>%
  select(SITE_ID, DATE_COL, DOC_UNITS, DOC_RESULT, NITRATE_N_UNITS, NITRATE_N_RESULT, PTL_UNITS, PTL_RESULT, DOC_LAB_FLAG, NITRATE_N_LAB_FLAG, PTL_LAB_FLAG) %>%
  mutate(YEAR = 2012) %>%
  rename(DOC = DOC_RESULT,
         DOC_FLAG = DOC_LAB_FLAG,
         NO3 = NITRATE_N_RESULT,
         NO3_UNITS = NITRATE_N_UNITS,
         NO3_FLAG = NITRATE_N_LAB_FLAG,
         TP = PTL_RESULT,
         TP_UNITS = PTL_UNITS,
         TP_FLAG = PTL_LAB_FLAG)  %>%
  mutate(DATE_COL = as.Date(DATE_COL, format = "%m/%d/%Y"))


nla_rs_2008 <- read.csv("Data/NLA/ChemData_NLArs2008.csv") %>%
  select(SITE_ID, DATE_COL, DOC, NO3, PTL, DOC_ALERT, NO3_ALERT, PTL_ALERT) %>%
  mutate(YEAR = 2008) %>%
  rename(
    DOC_FLAG = DOC_ALERT,
    NO3_FLAG = NO3_ALERT,
    TP = PTL,
    TP_FLAG = PTL_ALERT) %>%
  mutate(TP_UNITS = "UG/L",
         DOC_UNITS = "MG/L",
         NO3_UNITS = "MG/L")%>%
  mutate(DATE_COL = as.Date(DATE_COL, format = "%d-%b-%y"))

nla_rs_2013 <- read.csv("Data/NLA/ChemData_NLArs2013.csv") %>%
  select(SITE_ID, DATE_COL, DOC_RESULT_UNITS, DOC_RESULT, NITRATE_N_RESULT_UNITS, NITRATE_N_RESULT, PTL_RESULT_UNITS, PTL_RESULT, DOC_NARS_FLAG, NITRATE_N_NARS_FLAG, PTL_NARS_FLAG) %>%
  mutate(YEAR = 2013)  %>%
  rename(DOC = DOC_RESULT,
         DOC_UNITS = DOC_RESULT_UNITS,
         DOC_FLAG = DOC_NARS_FLAG,
         NO3 = NITRATE_N_RESULT,
         NO3_UNITS = NITRATE_N_RESULT_UNITS,
         NO3_FLAG = NITRATE_N_NARS_FLAG,
         TP = PTL_RESULT,
         TP_UNITS = PTL_RESULT_UNITS,
         TP_FLAG = PTL_NARS_FLAG)  %>%
  mutate(DATE_COL = as.Date(DATE_COL, format = "%m/%d/%Y"))

nla_rs_2004 <- read.csv("Data/NLA/ChemData_NLAs2004.csv") %>%
  select(SITE_ID, DATE_COL, DOC, NO3, PTL, DOCF, NO3F, PTLF) %>%
  mutate(YEAR = 2004)  %>%
  rename(
         DOC_FLAG = DOCF,
         NO3_FLAG = NO3F,
         TP = PTL,
         TP_FLAG = PTLF) %>%
  mutate(TP_UNITS = "UG/L",
         DOC_UNITS = "MG/L",
         NO3_UNITS = "Ueq/L")  %>%
  mutate(DATE_COL = as.Date(DATE_COL, format = "%m/%d/%Y"))

nla_rs_2019 <- read.csv("Data/NLA/ChemData_NLArs2019.csv") %>%
  select(SITE_ID, DATE_COL, DOC_UNITS, DOC_RESULT, NITRATE_N_UNITS, NITRATE_N_RESULT, PTL_UNITS, PTL_RESULT, DOC_NARS_FLAG, NITRATE_N_NARS_FLAG, PTL_NARS_FLAG) %>%
  mutate(YEAR = 2019) %>%
  rename(DOC = DOC_RESULT,
         DOC_FLAG = DOC_NARS_FLAG,
         NO3 = NITRATE_N_RESULT,
         NO3_UNITS = NITRATE_N_UNITS,
         NO3_FLAG = NITRATE_N_NARS_FLAG,
         TP = PTL_RESULT,
         TP_UNITS = PTL_UNITS,
         TP_FLAG = PTL_NARS_FLAG) %>%
  mutate(DATE_COL = as.Date(DATE_COL, format = "%m/%d/%Y"))








#2017 data -- annoying as hell
nla_2017_results <- read.csv("Data/NLA/Original_ChemData_NLA2017.csv") %>%
  select(SITE_ID, DATE_COL, ANALYTE, RESULT, RESULT_UNITS, NARS_FLAG) %>%
  mutate(YEAR = 2017) %>%
  pivot_wider(id_cols = c(SITE_ID, DATE_COL), values_from = RESULT, names_from = ANALYTE) %>%
  rename(
         NO3 = NITRATE_N,
         TP= PTL) %>%
  select(-NITRATE_NITRITE_N)

nla_2017_units <- read.csv("Data/NLA/Original_ChemData_NLA2017.csv") %>%
  select(SITE_ID, DATE_COL, ANALYTE, RESULT, RESULT_UNITS, NARS_FLAG) %>%
  mutate(YEAR = 2017) %>%
  pivot_wider(id_cols = c(SITE_ID, DATE_COL), values_from = RESULT_UNITS, names_from = ANALYTE) %>%
  rename(DOC_UNITS = DOC,
         NO3_UNITS = NITRATE_N,
         TP_UNITS = PTL) %>%
  select(-NITRATE_NITRITE_N)


nla_2017_flags <- read.csv("Data/NLA/Original_ChemData_NLA2017.csv") %>%
  select(SITE_ID, DATE_COL, ANALYTE, RESULT, RESULT_UNITS, NARS_FLAG) %>%
  mutate(YEAR = 2017) %>%
  pivot_wider(id_cols = c(SITE_ID, DATE_COL), values_from = NARS_FLAG, names_from = ANALYTE) %>%
  rename(DOC_FLAG = DOC,
         NO3_FLAG = NITRATE_N,
         TP_FLAG = PTL) %>%
  select(-NITRATE_NITRITE_N)

nla_2017 <- nla_2017_results %>%
  left_join(nla_2017_units) %>%
  left_join(nla_2017_flags) %>%
  mutate(YEAR = 2017) %>%
  mutate(DATE_COL = as.Date(DATE_COL, format = "%d-%b-%y"))



ALL_NLA_CNP <- nla_2017 %>%
  rbind(nla_2012) %>%
  rbind(nla_rs_2004) %>%
  rbind(nla_rs_2008) %>%
  rbind(nla_rs_2013) %>%
  rbind(nla_rs_2019) %>%
  arrange(DATE_COL) %>%
  select(-DOC_FLAG, -NO3_FLAG, -TP_FLAG) %>%
  left_join(NLA_sites)



