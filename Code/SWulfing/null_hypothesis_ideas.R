library(ggplot2)
library(tidyverse)

setwd("~/STOICH.Aim1/Data/NLA")


ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

test_data <- data.frame(read.csv("ChemData_NLArs2013.csv"))

model_2013 <- lm(NITRATE_N_RESULT~DOC_RESULT, data = test_data)
summary(model_2013)

ggplot(test_data) +
  geom_point(aes(log(PTL_RESULT), NITRATE_N_RESULT, color = DOC_RESULT * 1000)) +
  scale_color_viridis_c("TP"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "NRSA 2013",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))
#Graphing against phosporus shows htat high vals of ptl do dictate some pattern of Nitrate
#hist(test_data$NITRATE_N_RESULT) Definitely not normal. poisson but continuous?
#glm model and then mess with distributions
ggplotRegression(lm( NITRATE_N_RESULT ~ DOC_RESULT, data = test_data))

model1 <- lm( NITRATE_N_RESULT ~ DOC_RESULT + PTL_RESULT, data = test_data)
summary(model1)
cor(test_data$DOC_RESULT, test_data$PTL_RESULT)
plot(model1) #This will give you residtuals. A good thing to understand. Seeing patterns in residuals is bad. Confirms we need different error dist and then do model selection

#Try a bunch of chemistry with this to make sure not significant (make sure that they're not correlated with eachother)
#Model selection next to penalize number of params in model (AIC)


#Taking Log of Data
test_dataLOGS <- test_data %>%
  mutate(log_Nitrate = log(NITRATE_N_RESULT)) %>%
  mutate(log_DOC = log(DOC_RESULT))
                                   # Duplicate data
test_dataLOGS$log_Nitrate[is.na(test_dataLOGS$log_Nitrate) | test_dataLOGS$log_Nitrate == "-Inf"] <- NA

ggplot(test_dataLOGS) +
  geom_point(aes(log_DOC, log_Nitrate, color = PTL_RESULT * 1000)) +
  scale_color_viridis_c("TP"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "NRSA 2013",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))

logModel_2013 <- lm(log_Nitrate~DOC_RESULT, data = test_dataLOGS)
summary(logModel_2013)

ggplotRegression(lm(log_Nitrate ~ log_DOC, data = test_dataLOGS))

#Looking only at low DOC's (1st quantile ranve)
boxplot(test_data$DOC_RESULT)
boxplot(test_dataLOGS$log_DOC)

quantile(test_data$DOC_RESULT)

lowDOC_data <- test_data %>%
  filter(DOC_RESULT < quantile(test_data$DOC_RESULT)[2])

ggplot(lowDOC_data) +
  geom_point(aes(DOC_RESULT, NITRATE_N_RESULT, color = PTL_RESULT * 1000)) +
  scale_color_viridis_c("TP"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "NRSA 2013",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))

#Should I log DOC or not? What about PTL
log_lowDOC_data <-  lowDOC_data %>%
  mutate(log_Nitrate = log(NITRATE_N_RESULT)) %>%
  mutate(log_DOC = log(DOC_RESULT)) %>%
  mutate(log_PTL = log(PTL_RESULT))

log_lowDOC_data$log_Nitrate[is.na(log_lowDOC_data$log_Nitrate) | log_lowDOC_data$log_Nitrate == "-Inf"] <- NA
log_lowDOC_data$log_PTL[is.na(log_lowDOC_data$log_PTL) | log_lowDOC_data$log_PTL == "-Inf"] <- NA

ggplot(log_lowDOC_data) +
  geom_point(aes(DOC_RESULT, log_Nitrate, color = PTL_RESULT * 1000)) +
  scale_color_viridis_c("TP"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "NRSA 2013",
       x = "DOC"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1)))  
  
ggplotRegression(lm(log_Nitrate ~ PTL_RESULT, data = test_dataLOGS))

ggplot(log_lowDOC_data) +
  geom_point(aes(log(PTL_RESULT), log_Nitrate)) +
  scale_color_viridis_c("TP"~(mu~g~L^-1)) +
  theme_bw() +
  labs(title = "NRSA 2013",
       x = "PTL"~(mg~L^-1)) +
  ylab(expression(Nitrogen - NO[3]~(mg~L^-1))) 

ggplotRegression(lm(log_Nitrate ~ log_PTL, data = log_lowDOC_data))  
