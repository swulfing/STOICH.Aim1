library("dplyr")
library("ggplot2")
library("rnaturalearth")
library("rnaturalearthdata")
library("VennDiagram")

datapoints <-read.csv("C:/Users/sophi/Documents/STOICH/ResearchAim1/ChemData_Chla.csv")
points <- data.frame(datapoints)

##Medians Calculations##
cleaned_rows <- points %>%
  group_by(UniqueLakeName) %>%
  summarise(Lat = mean(Lat),
            Lon = mean(Lon),
            DOC_Median = median(DOC, na.rm = TRUE),
            TN_Median = median(TN, na.rm = TRUE),
            TP_Median = median(TP, na.rm = TRUE),
            pH = median(pH, na.rm = TRUE))

write.csv(cleaned_rows,"C:/Users/sophi/Documents/STOICH/ResearchAim1/MedianCalcs/ChlaData_Medians.csv", row.names = FALSE)

##Venn Diagram of Data##
counts <- read.csv("C:/Users/sophi/Documents/STOICH/ResearchAim1/MedianCalcs/ChlaData_Medians.csv")

DOC_Set <- counts$DOC_Median
TP_Set <- counts$TP_Median
TN_Set <- counts$TN_Median

DOC_Set[is.na(DOC_Set)] <- ""
TP_Set[is.na(TP_Set)] <- ""
TN_Set[is.na(TN_Set)] <- ""

v2 <- venn.diagram(list(DOC=DOC_Set, TP=TP_Set, TN=TN_Set),
fill = c("red", "blue", "yellow"),
alpha = c(0.5, 0.5, 0.5),
filename=NULL)

grid.newpage()
grid.draw(v2)
dev.off()


##Mapping##
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

sites <- data.frame(datapoints)

ggplot(data = world) +
  geom_sf() +
  geom_point(data = sites, aes(x = Lon, y = Lat), size = 1, 
             shape = 23, fill = "blue")
