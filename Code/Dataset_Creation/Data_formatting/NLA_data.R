

library(tidyverse)
library(lubridate)


# LAR has been using NLA data for another project and already has the data in a somewhat formatted state. This takes those csv files and further formats them to comply with our current data structure here :) 

nla07 <- read.csv("Data/NLA/NLA_2007.csv") |>
  select(DATE_COL, UNIQUE_ID, DOC_PPM, NO3N_PPM, PTL_PPB, LON_DD, LAT_DD) |>
  mutate(DATE_COL = as.Date(DATE_COL, format = "%m/%d/%Y"))
nla12 <- read.csv("Data/NLA/NLA_2012.csv") |>
  select(DATE_COL, UNIQUE_ID, DOC_PPM, NO3N_PPM, PTL_PPB, LON_DD, LAT_DD) |>
  mutate(DATE_COL = as.Date(DATE_COL, format = "%m/%d/%Y"))
nla17 <- read.csv("Data/NLA/NLA_2017.csv") |>
  select(DATE_COL, UNIQUE_ID, DOC_PPM, NO3N_PPM, PTL_PPB, LON_DD, LAT_DD) |>
  mutate(DATE_COL = as.Date(DATE_COL, format = "%d-%b-%y"))

NLA <- rbind(nla07, nla12) |>
  rbind(nla17) |>
  distinct() |>
  mutate(TP = PTL_PPB / 1000,
         UNIT = "mg/L",
         ECO_TYPE = "Lake") |>
  rename(DOC = DOC_PPM,
         LAT = LAT_DD,
         LON = LON_DD,
         SITE_ID = UNIQUE_ID) |>
  select(-PTL_PPB) |>
  mutate(TROPHIC_STATE = NA) |>
  mutate(TROPHIC_STATE = ifelse(TP <= 0.01, "Oligotrophic", 
                                ifelse(between(TP, 0.01, 0.025), "Mesotrophic",
                                       ifelse(TP > 0.025, "Eutrophic", TROPHIC_STATE)))) |>
  pivot_longer(cols = c(DOC, NO3N_PPM, TP), names_to = "VARIABLE", values_to = "RESULT") |>
  mutate(VARIABLE = ifelse(VARIABLE == "NO3N_PPM", "NO3 as N", VARIABLE))


write.csv(NLA, "Data/Simplified_datasets_per_source/NLA.csv")

