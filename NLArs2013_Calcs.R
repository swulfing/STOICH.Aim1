library("dplyr")
library("ggplot2")
library("rnaturalearth")
library("rnaturalearthdata")
library("VennDiagram")

datapoints <-read.csv("C:/Users/sophi/Documents/STOICH/STOICH.Aim1/ChemData_NLArs2013.csv")
points <- data.frame(datapoints)

##Medians Calculations##
cleaned_rows <- points %>%
  group_by(SITE_ID) %>%
  summarise(Lat = mean(LAT_DD83),
            Lon = mean(LON_DD83),
            DOC_Median = median(DOC, na.rm = TRUE),
            TN_Median = median(NTL_UG_L, na.rm = TRUE),
            TP_Median = median(PTL, na.rm = TRUE))

write.csv(cleaned_rows,"C:/Users/sophi/Documents/STOICH/STOICH.Aim1/MedianCalcs/NLArs2013_Medians.csv", row.names = FALSE)

##Venn Diagram of Data##
#counts <- read.csv("C:/Users/sophi/Documents/STOICH/STOICH.Aim1/BrazilData_Medians.csv")

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
#world <- ne_countries(scale = "medium", returnclass = "sf")
#class(world)

#sites <- data.frame(datapoints)

#ggplot(data = world) +
#  geom_sf() +
#  coord_sf(xlim = c(-85, -30), ylim = c(-35, 15), expand = FALSE) +
#  geom_point(data = sites, aes(x = Lon, y = Lat), size = 1, 
#             shape = 23, fill = "blue")
