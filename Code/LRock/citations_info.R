
library(tidyverse)
library(lubridate)


getinfo <- function(data) {
  
  count <- data |>
    mutate(DATE_COL = as.Date(DATE_COL)) |>
    filter(year(DATE_COL) >= 2000) |>
    select(ECO_TYPE, SITE_ID) |>
    distinct() |>
    count(ECO_TYPE)
  
  info <- data |>
    mutate(DATE_COL = as.Date(DATE_COL)) |>
    filter(year(DATE_COL) >= 2000) |>
    group_by(VARIABLE) |>
    summarise(median(RESULT))
  
  return(list(count, info))
    
}




NLA <- read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/Simplified_datasets_per_source/NLA.csv")
nlainfo <- getinfo(NLA)

NEON <- read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/other_vars_datasets/NEON_1.csv")
neoninfo<- getinfo(NEON)

NERC <- read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/other_vars_datasets/EIDC.csv")
nercinfo <- getinfo(NERC)

NRC <- read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/other_vars_datasets/nrc_cleaned.csv")
nrcinfo <- getinfo(NRC)

EU <- read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/other_vars_datasets/EU_filtered_cleaned.csv") |>
  mutate(DATE_COL = paste(DATE_COL, "-01-01", sep = ""))
euinfo <- getinfo(EU)

EDI <- read.csv("https://raw.githubusercontent.com/swulfing/STOICH.Aim1/main/Data/other_vars_datasets/LTER_1.csv")
ediinfo <- getinfo(EDI)
  
  
   