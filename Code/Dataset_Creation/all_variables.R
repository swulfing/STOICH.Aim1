source("Code/masterData.R")
source("Code/masterDataVARIABLES.R")


variables <- All_CNP_VARS |>
  count(VARIABLE)

EU <- read.csv("Data/other_vars_datasets/EU.csv")|>
  count(VARIABLE)

lagos <- read.csv("Data/other_vars_datasets/LAGOS.csv")|>
  count(VARIABLE)

lter <- read.csv("Data/other_vars_datasets/LTER.csv")|>
  count(VARIABLE)

NEON <- read.csv("Data/other_vars_datasets/NEON.csv")|>
  count(VARIABLE)

NRC <- read.csv("Data/other_vars_datasets/NRC.csv")|>
  count(VARIABLE)
