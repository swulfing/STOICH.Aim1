# how to take a spatial subset wrt to country boundaries
library(mgcv) # for in.out() function which we use to take subset
library(ggplot2) # for plotting and map_data() function which contains country boundaries

# load data
# source("Code/masterDataVARIABLES.R")
setwd("C://Users//david//Documents//STOICH//STOICH.Aim1//Code/DNguyen")
#write.csv(ALL_CNP, "ALL_CNP.csv", row.names = FALSE)
ALL_CNP <- read.csv("ALL_CNP.csv")

# let's get all data in France
french_map <- map_data("world", region = "France")

# get all observations in ALL_CNP which are within the boundary of france's borders
# logical vector with TRUE -> within border
french_points = in.out(as.matrix(french_map[,1:2]), # long, lat
                      as.matrix(ALL_CNP[,c("LON","LAT")])) # long, lat

# get french subset of cnp data
french_cnp <- ALL_CNP[french_points,]

# plot data
# just french map
ggplot() +
  geom_map(data = french_map, map = french_map,
           aes(long, lat, map_id = region),
           color = "white", fill = "lightgray") +
  coord_equal(ratio = 1) # keep lat long to same scale so shape isn't distorted

# plot map with french NO3 points on it
ggplot() +
  geom_map(data = french_map, map = french_map,
           aes(long, lat, map_id = region),
           color = "white", fill = "lightgray") +
  geom_point(data = french_cnp, 
             mapping = aes(x = LON, y = LAT, color = log(NO3.as.N))) +
  coord_equal(ratio = 1)
  
