# sweden lake survey simplified data

library(tidyverse)
library(lubridate)

df <- read.csv("Data/swe_lake_data_2510222.csv")

df.1 <- df |>
  filter(VARIABLE %in% c("DOC", "NO3_as_N", "TP")) |>
  pivot_wider(names_from = VARIABLE, values_from = RESULT) |>
  rename(`NO3 as N`=NO3_as_N,
         UNITS = UNIT) |>
  drop_na() |>
  filter(year(DATE_COL) >= 2000)

## ?? check NO3 

df.2 <- df.1 |>
  select(DATE_COL, `NO3 as N`) |>
  drop_na()

## they don't collect NO3 after 1996


