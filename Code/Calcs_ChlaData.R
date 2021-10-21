library("dplyr")
library("ggplot2")
library("rnaturalearth")
library("rnaturalearthdata")
library("VennDiagram")

datapoints <-read.csv("C:/Users/sophi/Documents/STOICH/STOICH.Aim1/ChemData_Chla.csv")
points <- data.frame(datapoints)

##DOC TP##
mod1 = lm(TP~DOC, data = points)
modsum = summary(mod1)
r2 = round(modsum$adj.r.squared, digits = 4)
pVal = signif(modsum$coefficients[2,4], digits = 3)
lab1 = paste("pValue ", pVal)
lab2 = paste("RSq ", r2)

plot(points$DOC, points$TP, main="CHLA",
     xlab="DOC ", ylab="TP", pch=19, ylim = c(0,1))
abline(mod1)
legend("topright", c(lab1, lab2), cex=0.8)

##DOC TN##

mod2 = lm(TN~DOC, data = points)
modsum = summary(mod2)
r2 = round(modsum$adj.r.squared, digits = 4)
pVal = signif(modsum$coefficients[2,4], digits = 3)
lab1 = paste("pValue ", pVal)
lab2 = paste("RSq ", r2)

plot(points$DOC, points$TN, main="CHLA",
     xlab="DOC ", ylab="TN", pch=19, ylim = c(0,10))
abline(mod2)
legend("topright", c(lab1, lab2), cex=0.8)

##TN TP
mod3 = lm(TP~TN, data = points)
modsum = summary(mod3)
r2 = round(modsum$adj.r.squared, digits = 4)
pVal = signif(modsum$coefficients[2,4], digits = 3)
lab1 = paste("pValue ", pVal)
lab2 = paste("RSq ", r2)

plot(points$TN, points$TP, main="CHLA",
     xlab="TN ", ylab="TP", pch=19)
abline(mod3)
legend("topright", c(lab1, lab2), cex=0.8)


# DOC NP Ratio
mod4 = lm(NtoP~DOC, data = points)
modsum = summary(mod4)
r2 = round(modsum$adj.r.squared, digits = 4)
pVal = signif(modsum$coefficients[2,4], digits = 3)
lab1 = paste("pValue ", pVal)
lab2 = paste("RSq ", r2)

plot(points$DOC, points$NtoP, main="CHLA",
     xlab="DOC ", ylab="NtoP", pch=19, ylim = c(0,250))
abline(mod4)
legend("topright", c(lab1, lab2), cex=0.8)


##Medians Calculations##
cleaned_rows <- points %>%
  group_by(UniqueLakeName) %>%
  summarise(Lat = mean(Lat),
            Lon = mean(Lon),
            DOC_Median = median(DOC, na.rm = TRUE),
            TN_Median = median(TN, na.rm = TRUE),
            TP_Median = median(TP, na.rm = TRUE),
            TNtoTPRatio_Median = median(NtoP, na.rm = TRUE),
            pH = median(pH, na.rm = TRUE))

write.csv(cleaned_rows,"C:/Users/sophi/Documents/STOICH/STOICH.Aim1/MedianCalcs/ChlaData_Medians.csv", row.names = FALSE)

##Venn Diagram of Data##
# counts <- read.csv("C:/Users/sophi/Documents/STOICH/STOICH.Aim1/MedianCalcs/ChlaData_Medians.csv")
# 
# DOC_Set <- counts$DOC_Median
# TP_Set <- counts$TP_Median
# TN_Set <- counts$TN_Median
# 
# DOC_Set[is.na(DOC_Set)] <- ""
# TP_Set[is.na(TP_Set)] <- ""
# TN_Set[is.na(TN_Set)] <- ""
# 
# v2 <- venn.diagram(list(DOC=DOC_Set, TP=TP_Set, TN=TN_Set),
# fill = c("red", "blue", "yellow"),
# alpha = c(0.5, 0.5, 0.5),
# filename=NULL)
# 
# grid.newpage()
# grid.draw(v2)
# dev.off()
# 

##Mapping##
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

sites <- data.frame(datapoints)

ggplot(data = world) +
  geom_sf() +
  geom_point(data = sites, aes(x = Lon, y = Lat), size = 1, 
             shape = 23, fill = "blue")
