library("dplyr")
library("ggplot2")
library("rnaturalearth")
library("rnaturalearthdata")
library("VennDiagram")

datapoints <-read.csv("C:/Users/sophi/Documents/STOICH/STOICH.Aim1/ChemData_NLArs2019.csv")
points <- data.frame(datapoints)

##DOC TP##
mod1 = lm(PTL_RESULT~DOC_RESULT, data = points)
modsum = summary(mod1)
r2 = round(modsum$adj.r.squared, digits = 4)
pVal = signif(modsum$coefficients[2,4], digits = 3)
lab1 = paste("pValue ", pVal)
lab2 = paste("RSq ", r2)

plot(points$DOC_RESULT, points$PTL_RESULT, main="NLArs2019",
     xlab="DOC", ylab="PTL", pch=19)
abline(mod1)
legend("topright", c(lab1, lab2), cex=0.8)

##DOC_RESULT TN##

mod2 = lm(NTL_RESULT~DOC_RESULT, data = points)
modsum = summary(mod2)
r2 = round(modsum$adj.r.squared, digits = 4)
pVal = signif(modsum$coefficients[2,4], digits = 3)
lab1 = paste("pValue ", pVal)
lab2 = paste("RSq ", r2)

plot(points$DOC_RESULT, points$NTL_RESULT, main="NLArs2019",
     xlab="DOC ", ylab="NTL", pch=19)
abline(mod2)
legend("topright", c(lab1, lab2), cex=0.8)

##TN TP
mod3 = lm(PTL_RESULT~NTL_RESULT, data = points)
modsum = summary(mod3)
r2 = round(modsum$adj.r.squared, digits = 4)
pVal = signif(modsum$coefficients[2,4], digits = 3)
lab1 = paste("pValue ", pVal)
lab2 = paste("RSq ", r2)

plot(points$NTL_RESULT, points$PTL_RESULT, main="NLArs2019",
     xlab="NTL", ylab="PTL", pch=19)
abline(mod3)
legend("topright", c(lab1, lab2), cex=0.8)


# DOC_RESULT NP Ratio
mod4 = lm(NtoP~DOC_RESULT, data = points)
modsum = summary(mod4)
r2 = round(modsum$adj.r.squared, digits = 4)
pVal = signif(modsum$coefficients[2,4], digits = 3)
lab1 = paste("pValue ", pVal)
lab2 = paste("RSq ", r2)

plot(points$DOC_RESULT, points$NtoP, main="NLArs2019",
     xlab="DOC ", ylab="NtoP", pch=19, ylim = c(0,.4))
abline(mod4)
legend("topright", c(lab1, lab2), cex=0.8)

###NITRATE Calcs##
##DOC NITRATE##

mod2 = lm(NITRATE_N_RESULT~DOC_RESULT, data = points)
modsum = summary(mod2)
r2 = round(modsum$adj.r.squared, digits = 4)
pVal = signif(modsum$coefficients[2,4], digits = 3)
lab1 = paste("pValue ", pVal)
lab2 = paste("RSq ", r2)

plot(points$DOC_RESULT, points$NITRATE_N_RESULT, main="NLArs2019",
     xlab="DOC ", ylab="NO3", pch=19)
abline(mod2)
legend("topright", c(lab1, lab2), cex=0.8)

##NITRATE_N_RESULT TP
mod3 = lm(PTL_RESULT~NITRATE_N_RESULT, data = points)
modsum = summary(mod3)
r2 = round(modsum$adj.r.squared, digits = 4)
pVal = signif(modsum$coefficients[2,4], digits = 3)
lab1 = paste("pValue ", pVal)
lab2 = paste("RSq ", r2)

plot(points$NITRATE_N_RESULT, points$PTL_RESULT, main="NLArs2019",
     xlab="NO3", ylab="PTL", pch=19)
abline(mod3)
legend("topright", c(lab1, lab2), cex=0.8)


# DOC_RESULT NO3:P Ratio
mod4 = lm(N03toP~DOC_RESULT, data = points)
modsum = summary(mod4)
r2 = round(modsum$adj.r.squared, digits = 4)
pVal = signif(modsum$coefficients[2,4], digits = 3)
lab1 = paste("pValue ", pVal)
lab2 = paste("RSq ", r2)

plot(points$DOC_RESULT, points$NO3toP, main="NLArs2019",
     xlab="DOC ", ylab="NO3toP", pch=19)
abline(mod4)
legend("topright", c(lab1, lab2), cex=0.8)

##Medians Calculations##
cleaned_rows <- points %>%
  group_by(SITE_ID) %>%
  summarise(DOC_Median = median(DOC_RESULT, na.rm = TRUE),
            NO3_Median = median(NITRATE_N_RESULT, na.rm = TRUE),
            TN_Median = median(NTL_RESULT, na.rm = TRUE),
            TP_Median = median(PTL_RESULT, na.rm = TRUE),
            TNtoTP_Median = median(NtoP, na.rm = TRUE),
            NO3toTP_Median = median(NO3toP, na.rm = TRUE))

write.csv(cleaned_rows,"C:/Users/sophi/Documents/STOICH/STOICH.Aim1/MedianCalcs/NLArs2019_Medians.csv", row.names = FALSE)

##Venn Diagram of Data##
#counts <- read.csv("C:/Users/sophi/Documents/STOICH/ResearchAim1/BrazilData_Medians.csv")

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
datapoints <-read.csv("C:/Users/sophi/Documents/STOICH/STOICH.Aim1/NLArs2019_points.csv")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

sites <- data.frame(datapoints)

ggplot(data = world) +
  geom_sf() +
  coord_sf(xlim = c(-125, -65), ylim = c(25, 50), expand = FALSE) +
  geom_point(data = sites, aes(x = LON_DD83, y = LAT_DD83), size = 1, 
             shape = 23, fill = "blue")
