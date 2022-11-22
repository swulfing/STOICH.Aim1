# library(fields)

# make grid of new locations to predict spatially varying terms
# filter so that only counties that have sampling locations are included
make_grid <- function(model, grid_size = 0.25) {
  
  # get smallest bounding rectangle which contains the sampling locations
  min_lon <- min(model$data$locs$LON)
  max_lon <- max(model$data$locs$LON)
  min_lat <- min(model$data$locs$LAT)
  max_lat <- max(model$data$locs$LAT)
  
  # create mesh in bounding rectangle
  grid_size_degrees <- grid_size
  new_grid <- 
    expand.grid(LON = seq(min_lon, max_lon, 
                          by = grid_size_degrees),
                LAT = seq(min_lat, max_lat, 
                          by = grid_size_degrees))
  
  # filter only cells that are within a country (e.g., filter out oceans)
  new_grid <- new_grid %>% 
    mutate(country = maps::map.where(database = "world", 
                                     x = LON, y = LAT)) %>% 
    filter(!is.na(country))
  
  # filter cells within countries with samples
  # Get all countries that contain sampling locations
  sample_countries <-
    maps::map.where(database = "world",
                    x = model$data$locs$LON,
                    y = model$data$locs$LAT) %>% unique()
  sample_countries <- sample_countries[!is.na(sample_countries)]
  
  # remove cells that are not contained in countries with samples
  new_grid <- new_grid[new_grid$country %in% sample_countries,]
  return(new_grid)
}

# calculate the standard deviation of predicted SVC
# input: 
# svc.mod: a varycoef::SVC_mle object
# newlocs: a matrix with columns LON, LAT of predicted points
# value:
# a matrix w/ columns of standard errors for each SVC_1, SVC_2, ...,  SVC_q
svc_sd <- function(svc.mod, newlocs) {
  # extract estimate range and variance for each svc
  covest <- cov_par(svc.mod)[-length(cov_par(svc.mod))]
  covest <- matrix(covest, nrow = 3, ncol = 2, byrow = TRUE)
  colnames(covest) <- c("range", "var")
  rownames(covest) <- paste0("SVC_", 1:3)
  
  # make matrix to store SVC prediction standard deviation
  svc_sd_mat <- matrix(0, nrow = nrow(newlocs), ncol = nrow(covest))
  colnames(svc_sd_mat) <- paste0(rownames(covest), "_sd")
  
  # make distance matrices
  # y is new points, x is old points
  locs <- svc.mod$data$locs # old locs
  D.yx = fields::rdist(newlocs,locs)
  D.xx = fields::rdist(locs,locs)
  D.yy = fields::rdist(newlocs,newlocs)
  
  # calculate sd for each SVC
  for (SVC_num in 1:nrow(covest)) {
    
    # if estimated variance of GP varying coefficient is 0 do not calculate sd
    if (covest[SVC_num, "var"] < 1e-6) next
    
    # for the second SVC3
    s.xx = covest[SVC_num, "var"]*exp(-D.xx/covest[SVC_num, "range"])
    s.yx = covest[SVC_num, "var"]*exp(-D.yx/covest[SVC_num, "range"])
    s.yy = covest[SVC_num, "var"]*exp(-D.yy/covest[SVC_num, "range"])
    
    # get standard deviation of predicted SVC values
    pre_sd.svc <- s.yy-s.yx %*% solve(s.xx,t(s.yx))
    svc_sd_mat[,SVC_num] <- sqrt(diag(pre_sd.svc))
  }
  
  return(svc_sd_mat)
}

# read in fitted models
# svc_na_lake <- readRDS("Code/DNguyen/models/svc_na_lake.RDS")
svc_na_lake <- readRDS("models/svc_na_lake_tp.RDS")
svc_na_river <- readRDS("models/svc_na_river.RDS")
svc_eu_lake <- readRDS("models/svc_eu_lake_tp.RDS")
svc_eu_river <- readRDS("models/svc_eu_river.RDS")

# set grid size for sd svc calculations
grid_size_degrees <- 0.25

# EU rivers
# get sd of predicted SVC for new locations
eu_river_grid <- make_grid(svc_eu_river, grid_size_degrees)[,-3] %>% as.matrix()

svc_sd_eu_river <- svc_sd(svc_eu_river, eu_river_grid)

# data frame of locs and SVC standard deviations
pred_sd_eu_river <- cbind(eu_river_grid, svc_sd_eu_river) %>% data.frame()

# # save sd of predicted SVC
saveRDS(pred_sd_eu_river, file = "pred_sd_eu_river.RDS")

# get predicted SVC1,2,3 for EU rivers
pred_eu_river <- predict(svc_eu_river, newlocs = eu_river_grid)
saveRDS(pred_eu_river, file = "pred_eu_river.RDS")


# NA rivers
svc_na_river$data$locs %>% 
  mutate(country = maps::map.where(database = "world", 
                                   x = LON, 
                                   y = LAT)) %>%
  group_by(country) %>%
  summarise(nsites = n()) %>%
  arrange(desc(nsites))

# na rivers
# get sd of predicted SVC for new locations
na_river_grid_big <- 
  make_grid(svc_na_river, grid_size_degrees)[,-3] %>% as.matrix()

na_river_grid <- 
  make_grid(svc_na_river, grid_size_degrees) %>% 
  filter(country %in% c("USA") ) %>%
  select(-country) %>%
  as.matrix()

# get sd
svc_sd_na_river <- svc_sd(svc_na_river, na_river_grid)

# data frame of locs and SVC standard deviations
pred_sd_na_river <- cbind(na_river_grid, svc_sd_na_river) %>% data.frame()

# # save sd of predicted SVC
saveRDS(pred_sd_na_river, file = "pred_sd_na_river.RDS")

# get predicted SVC1,2,3 for EU rivers
pred_na_river <- predict(svc_na_river, newlocs = na_river_grid)
saveRDS(pred_na_river, file = "pred_na_river.RDS")

# na lakes
svc_na_lake$data$locs %>% 
  mutate(country = maps::map.where(database = "world", 
                                   x = LON, 
                                   y = LAT)) %>%
  group_by(country) %>%
  summarise(nsites = n()) %>%
  arrange(desc(nsites))

# get sd of predicted SVC for new locations
na_lake_grid <- 
  make_grid(svc_na_lake, grid_size_degrees) %>% 
  filter(country == "USA") %>%
  select(-country) %>%
  as.matrix()

svc_sd_na_lake <- svc_sd(svc_na_lake, na_lake_grid)

# data frame of locs and SVC standard deviations
pred_sd_na_lake <- cbind(na_lake_grid, svc_sd_na_lake) %>% data.frame()

# # save sd of predicted SVC
saveRDS(pred_sd_na_lake, file = "pred_sd_na_lake.RDS")

# get predicted SVC1,2,3 for na rivers
pred_na_lake <- predict(svc_na_lake, newlocs = na_lake_grid)
saveRDS(pred_na_lake, file = "pred_na_lake.RDS")