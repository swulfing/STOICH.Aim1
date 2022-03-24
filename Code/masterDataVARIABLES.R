##Run script to create master dataset for all variables 


library(tidyverse)
library(lubridate)


#call in the data & ensure it is ready to use#####
temp <- (read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/other_vars_datasets/LAGOS_1.csv")) %>%
  bind_rows(read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/other_vars_datasets/NEON_1.csv")) %>%
  bind_rows(read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/other_vars_datasets/nrc_cleaned.csv")) %>%
  bind_rows(read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/other_vars_datasets/LTER_1.csv")) %>%
  bind_rows(read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/other_vars_datasets/EIDC.csv")) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  filter(year(DATE_COL) >= 2000) |> #get rid of any data pre 2000
  select(-X)

EU <- read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/other_vars_datasets/EU_filtered_cleaned.csv",
               sep = "'") %>%
  mutate(DATE_COL = paste(DATE_COL, "-01-01", sep = "")) %>%
  mutate(DATE_COL = as.Date(DATE_COL))

step1 <- rbind(EU, temp) 
#This dataset contains all concurrently collected DOC, nitrate as N, and phosphate as P data. units of everything are mg/L

rm(EU)
rm(temp)

step2 <- step1 %>%
  filter(RESULT > 0)

#some information about the dataset
# number.sites.VARS <- nrow(unique(ALL_CNP_VARS %>% dplyr::select(SITE_ID, LAT, LON, ECO_TYPE) %>%
#                               distinct()))
# number.lakes.VARS <- nrow(unique(ALL_CNP_VARS %>% dplyr::select(SITE_ID, LAT, LON, ECO_TYPE) %>%
#                               distinct() %>% filter(ECO_TYPE == "Lake")))
# number.rivers.VARS <- nrow(unique(ALL_CNP_VARS %>% dplyr::select(SITE_ID, LAT, LON, ECO_TYPE) %>%
#                                distinct() %>% filter(ECO_TYPE != "Lake")))

# check.that.sites.are.unique.VARS <- ALL_CNP_VARS %>% 
#   dplyr::select(SITE_ID, LAT, LON, ECO_TYPE) %>%
#   distinct() %>%
#   count(SITE_ID)

# standardiZing any names or units that are different. 
step3 <- step2 |>
  mutate(VARIABLE = ifelse(VARIABLE == 'SP_Cond', 'SP_COND', VARIABLE),
         VARIABLE = ifelse(VARIABLE == 'TIN as N', 'TIN', VARIABLE),
         VARIABLE = ifelse(VARIABLE == 'TOXIDN as N', 'TOXIDN', VARIABLE),
         VARIABLE = ifelse(VARIABLE == 'TC as C', 'TC', VARIABLE),
         VARIABLE = ifelse(VARIABLE == 'TN as N', 'TN', VARIABLE),
         VARIABLE = ifelse(VARIABLE == 'TON as N', 'TON', VARIABLE),
         VARIABLE = ifelse(VARIABLE == 'TDP as P', 'TDP', VARIABLE),
         VARIABLE = ifelse(VARIABLE == 'TP as P', 'TP', VARIABLE),
         VARIABLE = ifelse(VARIABLE == 'TPP as P', 'TPP', VARIABLE),
         VARIABLE = ifelse(VARIABLE == 'SULPHATE', 'SULFATE', VARIABLE),
         UNIT = ifelse(UNIT == 'mgl', 'mg/L', UNIT),
         UNIT = ifelse(UNIT == 'Unitless', NA, UNIT),
         UNIT = ifelse(UNIT == 'degc', 'deg_C', UNIT)) 

# Some sites without all three DOC, NO3, and PO4 were uploaded somehow -- adding those missing variables back in
source("Code/masterData.R")



step4 <- ALL_CNP |>
  select(-X) |>
  rename(`NO3 as N` = NO3.as.N,
         `PO4 as P` = PO4.as.P) |>
  pivot_longer(6:8, names_to = 'VARIABLE', values_to = 'RESULT')

ALL_CNP_VARS <- full_join(step3, step4) |>
  distinct()


rm(step1)
rm(step2)
rm(step3)
rm(step4)



# what are the units? 
UNITS_available <- ALL_CNP_VARS |>
  select(VARIABLE, UNIT) |>
  distinct()

# what proportion of sites have each variable? 
prop.sites <- ALL_CNP_VARS |>
  select(SITE_ID, VARIABLE, UNIT) |>
  unique() |>
  count(VARIABLE) |>
  rename(percent_sites = n) |>
  mutate(percent_sites = percent_sites/length(unique(ALL_CNP_VARS$SITE_ID)) * 100) |>
  left_join(UNITS_available)

rm(UNITS_available)
