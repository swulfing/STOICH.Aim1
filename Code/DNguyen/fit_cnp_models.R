library(tidyverse)
library(varycoef)
# library(mgcv)

# overview

# fit models:
# River/stream: France cluster
# lakes: Europe and North America
# Fixed effects: main effect of log(DOC) and log(PO4)
# SVC: include spatial intercept, DOC, and PO4
# gam: no spatial. Include S(DOC) and S(PO4)

# timing functions
tic <- function(){ start.time <<- Sys.time() }
toc <- function(){ round(Sys.time() - start.time) }

# read in data
setwd("/home/cresslerlab/dnguyen/svc")

# cnp_river <- read.csv("data/cnp_median_river.csv")
# cnp_lake <- read.csv("data/cnp_median_lake.csv")
# cnp <- rbind(cnp_river, cnp_lake)

cnp <- read.csv("data/ALL_CNP_med.csv")

# add region = Europe or North America
# cnp$region <- factor(ifelse(cnp$LON < -50, "North America", "Europe"))
# cnp$ECO_TYPE <- factor(cnp$ECO_TYPE)

# split between EU and NA, lakes and rivers
cnp_eu_river <- cnp %>% filter(ECO_TYPE == "River/Stream", region == "Europe")
cnp_eu_lake <- cnp %>% filter(ECO_TYPE == "Lake", region == "Europe") %>%
  filter(!is.na(TP_med))

cnp_na_river <- cnp %>% filter(ECO_TYPE == "River/Stream", region == "North America")
cnp_na_lake <- cnp %>% filter(ECO_TYPE == "Lake", region == "North America")


### ----
### fit SVC models

# # EU lakes with P04
# 
# # make design matrices
# # create input matrices
# y_eu_lake <- log(cnp_eu_lake$N_med)
# # Predictors
# X_eu_lake <- model.matrix(~ 1 + log(DOC_med) + log(P_med) + 
#                           log(DOC_med):log(P_med), 
#                   data = cnp_eu_lake)
# # locations (now in kilometers)
# locs_eu_lake <- cnp_eu_lake[, c("LON", "LAT")]
# # Spatial weights: allowing effect of PO4 and DOC to vary spatially.
# W_eu_lake <-  model.matrix(~ 1 + log(DOC_med) + log(P_med)
#                    , data = cnp_eu_lake)
# 
# # fit SVC model
# control <- SVC_mle_control(profileLik = T)
# 
# tic()
# svc_eu_lake <- SVC_mle(y = y_eu_lake, 
#                    X = X_eu_lake, W = W_eu_lake, 
#                    locs = locs_eu_lake,
#                    control = control, optim.control = list(maxit = 500)
# )
# runtime <- toc()
# runtime
# 
# summary(svc_eu_lake)
# 
# saveRDS(svc_eu_lake, "models/svc_eu_lake.RDS")


###------ 
# EU lakes with TP

# make design matrices
# create input matrices
y_eu_lake_tp <- log(cnp_eu_lake$N_med)
# Predictors
X_eu_lake_tp <- model.matrix(~ 1 + log(DOC_med) + log(TP_med) + 
                            log(DOC_med):log(TP_med), 
                          data = cnp_eu_lake)
# locations (now in kilometers)
locs_eu_lake_tp <- cnp_eu_lake[, c("LON", "LAT")]
# Spatial weights: allowing effect of PO4 and DOC to vary spatially.
W_eu_lake_tp <-  model.matrix(~ 1 + log(DOC_med) + log(TP_med)
                           , data = cnp_eu_lake)

# fit SVC model
control <- SVC_mle_control(profileLik = T)

tic()
svc_eu_lake_tp <- SVC_mle(y = y_eu_lake_tp, 
                       X = X_eu_lake_tp, W = W_eu_lake_tp, 
                       locs = locs_eu_lake_tp,
                       control = control, optim.control = list(maxit = 500)
)
runtime <- toc()
runtime

summary(svc_eu_lake_tp)

saveRDS(svc_eu_lake_tp, "models/svc_eu_lake_tp.RDS")


# # NA lakes using P04
# # make design matrices
# # create input matrices
# y_na_lake <- log(cnp_na_lake$N_med)
# # Predictors
# X_na_lake <- model.matrix(~ 1 + log(DOC_med) + log(P_med) + 
#                             log(DOC_med):log(P_med), #+ 
#                           # ECO_TYPE *log(DOC) + 
#                           # ECO_TYPE *log(`PO4.as.P`), 
#                           data = cnp_na_lake)
# # locations (now in kilometers)
# locs_na_lake <- cnp_na_lake[, c("LON", "LAT")]
# # Spatial weights: allowing effect of PO4 and DOC to vary spatially.
# W_na_lake <-  model.matrix(~ 1 + log(DOC_med) + log(P_med)
#                            , data = cnp_na_lake)
# 
# # fit SVC model
# control <- SVC_mle_control(profileLik = T)
# 
# tic()
# svc_na_lake <- SVC_mle(y = y_na_lake, 
#                        X = X_na_lake, W = W_na_lake, 
#                        locs = locs_na_lake,
#                        control = control, optim.control = list(maxit = 500)
# )
# runtime <- toc()
# runtime
# 
# summary(svc_na_lake)
# 
# saveRDS(svc_na_lake, "models/svc_na_lake.RDS")

# NA Lakes with TP instead of Phosphate

cnp_na_lake_tp <- 
  read.csv("data/ALL_CNP_med.csv") %>% 
  filter(ECO_TYPE == "Lake", region == "North America")

# make design matrices
# create input matrices
y_na_lake_tp <- log(cnp_na_lake_tp$N_med)
# Predictors
X_na_lake_tp <- model.matrix(~ 1 + log(DOC_med) + log(TP_med) + 
                            log(DOC_med):log(TP_med),  
                          data = cnp_na_lake_tp)
# locations (now in kilometers)
locs_na_lake_tp <- cnp_na_lake_tp[, c("LON", "LAT")]
# Spatial weights: allowing effect of PO4 and DOC to vary spatially.
W_na_lake_tp <-  model.matrix(~ 1 + log(DOC_med) + log(TP_med)
                           , data = cnp_na_lake_tp)

# fit SVC model
control <- SVC_mle_control(profileLik = T)

tic()
svc_na_lake_tp <- SVC_mle(y = y_na_lake_tp, 
                       X = X_na_lake_tp, W = W_na_lake_tp, 
                       locs = locs_na_lake_tp,
                       control = control, optim.control = list(maxit = 500)
)
runtime <- toc()
runtime

summary(svc_na_lake_tp)

saveRDS(svc_na_lake_tp, "models/svc_na_lake_tp.RDS")


# NA rivers
# make design matrices
# create input matrices
y_na_river <- log(cnp_na_river$N_med)
# Predictors
X_na_river <- model.matrix(~ 1 + log(DOC_med) + log(P_med) + 
                            log(DOC_med):log(P_med), #+ 
                          # ECO_TYPE *log(DOC) + 
                          # ECO_TYPE *log(`PO4.as.P`), 
                          data = cnp_na_river)
# locations (now in kilometers)
locs_na_river <- cnp_na_river[, c("LON", "LAT")]
# Spatial weights: allowing effect of PO4 and DOC to vary spatially.
W_na_river <-  model.matrix(~ 1 + log(DOC_med) + log(P_med)
                           , data = cnp_na_river)

# fit SVC model
control <- SVC_mle_control(profileLik = T)

tic()
svc_na_river <- SVC_mle(y = y_na_river, 
                       X = X_na_river, W = W_na_river, 
                       locs = locs_na_river,
                       control = control, optim.control = list(maxit = 500)
)
runtime <- toc()
runtime

summary(svc_na_river)

saveRDS(svc_na_river, "models/svc_na_river.RDS")

# eu rivers
# make design matrices
# create input matrices
y_eu_river <- log(cnp_eu_river$N_med)
# Predictors
X_eu_river <- model.matrix(~ 1 + log(DOC_med) + log(P_med) + 
                             log(DOC_med):log(P_med), #+ 
                           # ECO_TYPE *log(DOC) + 
                           # ECO_TYPE *log(`PO4.as.P`), 
                           data = cnp_eu_river)
# locations (now in kilometers)
locs_eu_river <- cnp_eu_river[, c("LON", "LAT")]
# Spatial weights: allowing effect of PO4 and DOC to vary spatially.
W_eu_river <-  model.matrix(~ 1 + log(DOC_med) + log(P_med)
                            , data = cnp_eu_river)

# fit SVC model
control <- SVC_mle_control(profileLik = T)

### take subset of data
# set.seed(123)
# nsamples <- 1000
# sample_eu_river <- sample( 1:length(y_eu_river), nsamples, replace = FALSE)

sample_eu_river <- 1:length(y_eu_river)

tic()
svc_eu_river <- SVC_mle(y = y_eu_river[sample_eu_river], 
                        X = X_eu_river[sample_eu_river,], W = W_eu_river[sample_eu_river,], 
                        locs = locs_eu_river[sample_eu_river,],
                        control = control, optim.control = list(maxit = 500)
)
runtime <- toc()
runtime

summary(svc_eu_river)

saveRDS(svc_eu_river, "models/svc_eu_river.RDS")

### ----
# end fit SVC models