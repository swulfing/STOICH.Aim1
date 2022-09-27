

### THIS IS THE SCRIPT TO USE FOR OBTAINING THE DATASET!!! ###

library(tidyverse)
library(lubridate)


#call in the data & ensure it is ready to use#####
temp <- (read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/Simplified_datasets_per_source/NLA.csv")) %>%
  bind_rows(read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/Simplified_datasets_per_source/WQP_rivers.csv")) %>%
  bind_rows(read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/other_vars_datasets/NEON_1.csv")) %>%
  bind_rows(read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/other_vars_datasets/nrc_cleaned.csv")) %>%
  bind_rows(read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/other_vars_datasets/LTER_1.csv")) %>%
  bind_rows(read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/other_vars_datasets/EIDC.csv")) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  filter(year(DATE_COL) >= 2000) |> #get rid of any data pre 2000
  select(-X, - X.1)

EU <- read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/other_vars_datasets/EU_filtered_cleaned.csv") %>%
  mutate(DATE_COL = paste(DATE_COL, "-01-01", sep = "")) %>%
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  select(-X)

step1 <- rbind(EU, temp) 
#This dataset contains all concurrently collected DOC, nitrate as N, and phosphate as P data. units of everything are mg/L

rm(EU)
rm(temp)

step2 <- step1 %>%
  filter(RESULT > 0)

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

#call in the data & ensure it is ready to use#####
temp <- #(read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/Simplified_datasets_per_source/NLA.csv")) %>%
  
  read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/Simplified_datasets_per_source/SIMPLE_NEON.csv") %>%
  
  bind_rows(read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/Simplified_datasets_per_source/SIMPLE_NRC.csv")) %>%
  
  bind_rows(read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/Simplified_datasets_per_source/SIMPLE_LTER.csv")) %>%
  
  bind_rows(read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/Simplified_datasets_per_source/SIMPLE_EIDC.csv")) %>%
  
  mutate(DATE_COL = as.Date(DATE_COL)) %>%
  
  filter(year(DATE_COL) >= 2000) |> #get rid of any data pre 2000
  
  select(-X, -X.1)



EU <- read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/Simplified_datasets_per_source/SIMPLE_EU.csv") %>%
  
  mutate(DATE_COL = paste(DATE_COL, "-01-01", sep = "")) %>%
  
  mutate(DATE_COL = as.Date(DATE_COL)) |>
  select(-X)


ALL_CNP <- rbind(EU, temp) 
#This dataset contains all concurrently collected DOC, nitrate as N, and phosphate as P data. units of everything are mg/L

rm(EU)
rm(temp)

ALL_CNP <- ALL_CNP %>%
  filter(NO3.as.N >0,
         PO4.as.P > 0,
         DOC >0 ) %>%
  rename(UNIT = UNITS)

step4 <- ALL_CNP |>
  rename(`NO3 as N` = NO3.as.N,
         `PO4 as P` = PO4.as.P) |>
  pivot_longer(6:8, names_to = 'VARIABLE', values_to = 'RESULT')

ALL_CNP_VARS <- full_join(step3, step4) |>
  distinct()


rm(step1)
rm(step2)
rm(step3)
rm(step4)
rm(ALL_CNP)

# # what are the units? 
# UNITS_available <- ALL_CNP_VARS |>
#   select(VARIABLE, UNIT) |>
#   distinct()
# 
# # what proportion of sites have each variable? 
# prop.sites <- ALL_CNP_VARS |>
#   select(SITE_ID, VARIABLE, UNIT) |>
#   unique() |>
#   count(VARIABLE) |>
#   rename(percent_sites = n) |>
#   mutate(percent_sites = percent_sites/length(unique(ALL_CNP_VARS$SITE_ID)) * 100) |>
#   left_join(UNITS_available)
# 
# rm(UNITS_available)


ALL_CNP <- ALL_CNP_VARS |>
  filter(VARIABLE %in% c("DOC", "NO3 as N", "PO4 as P", "TP")) |>
  #select(-TROPHIC_STATE) |>
  group_by(DATE_COL, SITE_ID, VARIABLE) |>
  mutate(RESULT = mean(RESULT)) |>
  ungroup() |>
  distinct() |>
  pivot_wider(names_from = VARIABLE, values_from = RESULT) |>
  drop_na(`NO3 as N`, DOC) |>
  rename(NO3.as.N = `NO3 as N`,
         PO4.as.P = `PO4 as P`)|>
  filter(!(is.na(PO4.as.P) & is.na(TP)))


rm(ALL_CNP_VARS)

ALL_CNP <- ALL_CNP |>
  select(-TROPHIC_STATE)

RIVERS <- ALL_CNP |>
  filter(ECO_TYPE != "Lake") |>
  select(-TP) |>
  drop_na()

LAKES <- ALL_CNP |>
  filter(ECO_TYPE == "Lake") |>
  select(-PO4.as.P) |>
  drop_na()


#some information about the dataset
number.sites <- nrow(unique(ALL_CNP %>% dplyr::select(SITE_ID, LAT, LON, ECO_TYPE) %>%
                              distinct()))
number.lakes <- nrow(unique(LAKES %>% dplyr::select(SITE_ID, LAT, LON, ECO_TYPE) %>%
                              distinct()))
number.rivers <- nrow(unique(RIVERS %>% dplyr::select(SITE_ID, LAT, LON, ECO_TYPE) %>%
                               distinct()))

# check.that.sites.are.unique <- ALL_CNP %>% 
#   dplyr::select(SITE_ID, LAT, LON, ECO_TYPE) %>%
#   distinct() %>%
#   count(SITE_ID)

# observations.per.site.of.CNP <- ALL_CNP %>%
#   count(SITE_ID) 
# 
# stats.per.site <- ALL_CNP %>%
#   group_by(SITE_ID) %>%
#   summarise(median.DOC = median(DOC),
#             median.NO3 = median(NO3.as.N),
#             median.PO4.as.P = median(PO4.as.P),
#             mean.DOC = mean(DOC),
#             mean.NO3 = mean(NO3.as.N),
#             mean.PO4.as.P = mean(PO4.as.P),
#             sd.DOC = sd(DOC),
#             sd.NO3 = sd(NO3.as.N),
#             sd.PO4.as.P = sd(PO4.as.P))


