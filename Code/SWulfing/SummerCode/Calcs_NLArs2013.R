library("dplyr")
library("ggplot2")
library("rnaturalearth")
library("rnaturalearthdata")
library("VennDiagram")

datapoints <-read.csv("Data/NLA/ChemData_NLArs2013.csv")
points <- data.frame(datapoints)

##DOC TP##
mod1 = lm(PTL_RESULT~DOC_RESULT, data = points)
modsum = summary(mod1)
r2 = round(modsum$adj.r.squared, digits = 4)
pVal = signif(modsum$coefficients[2,4], digits = 3)
lab1 = paste("pValue ", pVal)
lab2 = paste("RSq ", r2)

plot(points$DOC_RESULT, points$PTL_RESULT, main="NLArs2013",
     xlab="DOC ", ylab="PTL", pch=19)
abline(mod1)
legend("topright", c(lab1, lab2), cex=0.8)

##DOC TN##

mod2 = lm(NITRATE_N_RESULT~DOC_RESULT, data = points)
modsum = summary(mod2)
r2 = round(modsum$adj.r.squared, digits = 4)
pVal = signif(modsum$coefficients[2,4], digits = 3)
lab1 = paste("pValue ", pVal)
lab2 = paste("RSq ", r2)

plot(points$DOC_RESULT, points$NITRATE_N_RESULT, main="NLArs2013",
     xlab="DOC ", ylab="NO3", pch=19)
abline(mod2)
legend("topright", c(lab1, lab2), cex=0.8)

##TN TP
mod3 = lm(PTL_RESULT~NITRATE_N_RESULT, data = points)
modsum = summary(mod3)
r2 = round(modsum$adj.r.squared, digits = 4)
pVal = signif(modsum$coefficients[2,4], digits = 3)
lab1 = paste("pValue ", pVal)
lab2 = paste("RSq ", r2)

plot(points$NITRATE_N_RESULT, points$PTL_RESULT, main="NLArs2013",
     xlab="NO3", ylab="PTL", pch=19)
abline(mod3)
legend("topright", c(lab1, lab2), cex=0.8)


# DOC NP Ratio
mod4 = lm(NO3toP~DOC, data = points)
modsum = summary(mod4)
r2 = round(modsum$adj.r.squared, digits = 4)
pVal = signif(modsum$coefficients[2,4], digits = 3)
lab1 = paste("pValue ", pVal)
lab2 = paste("RSq ", r2)

plot(points$DOC, points$NO3toTP, main="NLArs2013",
     xlab="DOC ", ylab="NO3:P", pch=19)
abline(mod4)
legend("topright", c(lab1, lab2), cex=0.8)

##Medians Calculations##
cleaned_rows <- points %>%
  group_by(SITE_ID) %>%
  summarise(Lat = mean(LAT_DD83),
            Lon = mean(LON_DD83),
            DOC_Median = median(DOC_RESULT, na.rm = TRUE),
            NO3_Median = median(NITRATE_N_RESULT, na.rm = TRUE),
            TP_Median = median(PTL_RESULT, na.rm = TRUE),
            NO3toTP_Median = median(NO3toTP, na.rm = TRUE))

write.csv(cleaned_rows,"Data/MedianCalcs/NLArs2013_Medians.csv", row.names = FALSE)

##Venn Diagram of Data##
#counts <- read.csv("Data/BrazilData_Medians.csv")

#DOC_Set <- counts$DOC_Median
#PO4_Set <- counts$PO4_Median
#NO3_Set <- counts$NO3_Median

#DOC_Set[is.na(DOC_Set)] <- ""
#PO4_Set[is.na(PO4_Set)] <- ""
#NO3_Set[is.na(NO3_Set)] <- ""

#v2 <- venn.diagram(list(DOC=DOC_Set, PO4=PO4_Set, NO3=NO3_Set),
                   #fill = c("red", "blue", "yellow"),
                   #alpha = c(0.5, 0.5, 0.5),
                   #filename=NULL)

#grid.newpage()
#grid.draw(v2)
#dev.off()


##Mapping##
datapoints <- read.csv("Data/MedianCalcs/NLArs2013_Medians.csv")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

sites <- data.frame(datapoints)

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-125, -65), ylim = c(25, 50), expand = FALSE) +
  geom_point(data = sites, aes(x = Lon, y = Lat), size = 1, 
             shape = 23, fill = "blue")
