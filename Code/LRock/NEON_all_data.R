#script that reads in files (stored on LAR's computer - not GitHub) and concatenate it all into a single .rds

library(tidyverse)
library(data.table)

setwd("C:/Users/lrock1/OneDrive - University of Wyoming/NEON_all_data")

#create a list of the files from your target directory
file_list <- list.files(path="C:/Users/lrock1/OneDrive - University of Wyoming/NEON_all_data")

#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
dataset <- data.frame()

#had to specify columns to get rid of the total column
for (i in 1:length(file_list)){
  neon_data <- fread(file_list[i], stringsAsFactors = F) #read in files using the fread function from the data.table package
  dataset <- rbindlist(list(dataset, neon_data), use.names = T) #for each iteration, bind the new data to the building dataset
}


