### Script to add trophic status to master data frames based on TP values ###

library(tidyverse)


## read in the data
lagos <- read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/other_vars_datasets/LAGOS_1.csv")
neon <- read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/other_vars_datasets/NEON_1.csv")
nrc <- read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/other_vars_datasets/nrc_cleaned.csv")
lter <- read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/other_vars_datasets/LTER_1.csv")
eidc <- read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/other_vars_datasets/EIDC.csv") 
EU <- read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/other_vars_datasets/EU_filtered_cleaned.csv",
               sep = "'") %>%
  mutate(DATE_COL = paste(DATE_COL, "-01-01", sep = "")) %>%
  mutate(DATE_COL = as.Date(DATE_COL))


## write function to add trophic status based on the endpoints that the US EPA uses to assess lakes and reservoirs
calc_trophic_state <- function(data) {
  
  data1 <- data |>
    filter(VARIABLE == "TP" |
             VARIABLE == "TP as P") |>
   # filter(ECO_TYPE == "Lake") |>
    select(SITE_ID, DATE_COL, ECO_TYPE, RESULT) |>
    mutate(TROPHIC_STATE = NA) |>
    mutate(TROPHIC_STATE = ifelse(RESULT <= 0.01, "Oligotrophic", 
                                  ifelse(between(RESULT, 0.01, 0.025), "Mesotrophic",
                                         ifelse(RESULT > 0.025, "Eutrophic", TROPHIC_STATE)))) |>
    select(-RESULT)
  
}


## run function through datasets
lagos_ts <- calc_trophic_state(lagos)
neon_ts <- calc_trophic_state(neon)
nrc_ts <- calc_trophic_state(nrc)
lter_ts <- calc_trophic_state(lter)
eidc_ts <- calc_trophic_state(eidc)
eu_ts <- calc_trophic_state(EU)


## combine trophic states with the datasets and overwrite the exisiting datasets (completed 04/18/2022)
lagos_new <- left_join(lagos, lagos_ts)


