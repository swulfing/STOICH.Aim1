#script to clean NEON dataset and add new parameters
library(tidyverse)

neon <- read.csv('Data/other_vars_datasets/NEON.csv') |>
  select(-X)

#which variables are available?
vars <- as.data.frame(unique(neon$VARIABLE))

vars_to_keep <- c('Ca', 'Cl', 'DIC', 'DOC', 'K', 'Mg', 'Mn', 'Na', 'NH4 - N', 'NO2 - N', 'NO3 as N', 'PO4 as P', 'pH', 'SO4', 'specificConductance', 'TDN', 'TDP', 'TN', 'TOC', 'TP', 'TPC', 'TPN')

#filter dataset to parameters of interest and fix units
neon1 <- neon |>
  filter(VARIABLE %in% vars_to_keep) |>
  mutate(VARIABLE = ifelse(VARIABLE == 'Ca', 'CALCIUM', VARIABLE),
         VARIABLE = ifelse(VARIABLE == 'Cl', 'CHLORIDE', VARIABLE),
         VARIABLE = ifelse(VARIABLE == 'K', 'POTASSIUM', VARIABLE),
         VARIABLE = ifelse(VARIABLE == 'Mg', 'MAGNESIUM', VARIABLE),
         VARIABLE = ifelse(VARIABLE == 'Mn', 'MANGANESE', VARIABLE),
         VARIABLE = ifelse(VARIABLE == 'Na', 'SODIUM', VARIABLE),
         VARIABLE = ifelse(VARIABLE == 'NH4 - N', 'NH4 as N', VARIABLE),
         VARIABLE = ifelse(VARIABLE == 'NO2 - N', 'NO2 as N', VARIABLE),
         VARIABLE = ifelse(VARIABLE == 'SO4', 'SULFATE', VARIABLE),
         VARIABLE = ifelse(VARIABLE == 'specificConductance', 'SP_COND', VARIABLE)) |>
  mutate(UNIT = ifelse(UNIT == 'milligramsPerLiter', "mg/L", UNIT),
         UNIT = ifelse(UNIT == 'pH', 'Unitless', UNIT),
         UNIT = ifelse(UNIT == 'microsiemensPerCentimeter', 'uS/cm', UNIT),
         UNIT = ifelse(UNIT == 'milligram', 'mg', UNIT))

#check vars and units
varsunits <- neon1 |>
  select(VARIABLE, UNIT) |>
  distinct()

write.csv(neon1, 'Data/other_vars_datasets/NEON_1.csv')
