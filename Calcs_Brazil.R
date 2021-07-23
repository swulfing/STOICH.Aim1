library("dplyr")
library("ggplot2")
library("rnaturalearth")
library("rnaturalearthdata")
library("VennDiagram")

datapoints <-read.csv("C:/Users/sophi/Documents/STOICH/STOICH.Aim1/ChemData_Brazil.csv")
points <- data.frame(datapoints)

##DOC PO4##
mod1 = lm(PO4~DOC, data = points)
modsum = summary(mod1)
r2 = round(modsum$adj.r.squared, digits = 4)
pVal = signif(modsum$coefficients[2,4], digits = 3)
lab1 = paste("pValue ", pVal)
lab2 = paste("RSq ", r2)

plot(points$DOC, points$PO4, main="Brazil",
     xlab="DOC ", ylab="PO4", pch=19)
abline(mod1)
legend("topright", c(lab1, lab2), cex=0.8)

##DOC NO3##

mod2 = lm(NO3~DOC, data = points)
modsum = summary(mod2)
r2 = round(modsum$adj.r.squared, digits = 4)
pVal = signif(modsum$coefficients[2,4], digits = 3)
lab1 = paste("pValue ", pVal)
lab2 = paste("RSq ", r2)

plot(points$DOC, points$NO3, main="Brazil",
     xlab="DOC ", ylab="NO3", pch=19)
abline(mod2)
legend("topright", c(lab1, lab2), cex=0.8)

##NO3 PO4
mod3 = lm(PO4~NO3, data = points)
modsum = summary(mod3)
r2 = round(modsum$adj.r.squared, digits = 4)
pVal = signif(modsum$coefficients[2,4], digits = 3)
lab1 = paste("pValue ", pVal)
lab2 = paste("RSq ", r2)

plot(points$NO3, points$PO4, main="Brazil",
     xlab="NO3 ", ylab="PO4", pch=19)
abline(mod3)
legend("topright", c(lab1, lab2), cex=0.8)

 
# plot(points$DOC, points$NtoP, main="Brazil",
#      xlab="DOC", ylab="N:P", pch=19)
mod4 = lm(NtoP~DOC, data = points)
modsum = summary(mod4)
r2 = round(modsum$adj.r.squared, digits = 4)
pVal = signif(modsum$coefficients[2,4], digits = 3)
lab1 = paste("pValue ", pVal)
lab2 = paste("RSq ", r2)

plot(points$DOC, points$NtoP, main="Brazil",
     xlab="DOC ", ylab="NtoP", pch=19)
abline(mod4)
legend("topright", c(lab1, lab2), cex=0.8)

##Medians Calculations##
cleaned_rows <- points %>%
  group_by(Station, Lon,Lat) %>%
  summarise(DOC_Median = median(DOC, na.rm = TRUE),
            NO3_Median = median(NO3, na.rm = TRUE),
            PO4_Median = median(PO4, na.rm = TRUE),
            pH = median(pH, na.rm = TRUE),
            NO3toP_Median = median(NtoP, na.rm = TRUE))

write.csv(cleaned_rows,"C:/Users/sophi/Documents/STOICH/STOICH.Aim1/MedianCalcs/BrazilData_Medians.csv", row.names = FALSE)

##Venn Diagram of Data##
#counts <- read.csv("C:/Users/sophi/Documents/STOICH/STOICH.Aim1/MedianCalcs/BrazilData_Medians.csv")

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
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

sites <- data.frame(datapoints)

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-85, -30), ylim = c(-35, 15), expand = FALSE) +
  geom_point(data = sites, aes(x = Lon, y = Lat), size = 1,
             shape = 23, fill = "blue")
##Plots##
# counts <- read.csv("C:/Users/sophi/Documents/STOICH/STOICH.Aim1/MedianCalcs/BrazilData_Medians.csv")
# counts<-data.frame(counts)
# 
# plot(counts$DOC_Median, counts$PO4_Median, main="Brazil Medians",
#      xlab="DOC ", ylab="PO4", pch=19)
# 
# plot(counts$DOC_Median, counts$NO3_Median, main="Brazil Medians",
#      xlab="DOC ", ylab="NO3", pch=19)
# 
# plot(counts$NO3_Median, counts$PO4_Median, main="Brazil Medians",
#      xlab="NO3", ylab="PO4", pch=19)
