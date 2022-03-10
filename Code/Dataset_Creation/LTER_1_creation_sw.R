library(dplyr)
setwd("~/STOICH.Aim1")

LTER_data <- read.csv("Data/other_vars_datasets/LTER.csv")
LTER_df <- data.frame(LTER_data)

#Renaming/converting

#Note, I found some metadata sets with srp but couldn't find in combined datasets: Huron, BIG SPRING
#Also see "Code/SWulfing/LTER_DataNotes for synthesis of types of data being used by each site

print(data.frame(unique(LTER_df$VARIABLE)))
#print(unique(LTER_df$SITE_ID))


for (i in 1:nrow(LTER_df)){
  if(LTER_df$VARIABLE[i] == "cond" | LTER_df$VARIABLE[i] == "ysi_cond"){
    LTER_df$VARIABLE[i] <-   "SP_COND"
  }
  else if(LTER_df$VARIABLE[i] == "NH4."){
    #I THINK these measurements are just in NH4, not NH as N. Didn't say explicitly
    LTER_df$VARIABLE[i] <-   "NH4 as N"
    #Convert from meq/L and need to convert to NH4 as N
    LTER_df$RESULT[i] <- LTER_df$RESULT[i] * (18.04/1.2878)
  }
  else if(LTER_df$VARIABLE[i] == "Ca.."){
    #Units in meq/L
    LTER_df$VARIABLE[i] <-   "CALCIUM"
    LTER_df$RESULT[i] <- LTER_df$RESULT[i] * 20.04
  }
  else if(LTER_df$VARIABLE[i] == "Mg.."){
    #Units in meq/L
    LTER_df$VARIABLE[i] <-   "MAGNESIUM"
    LTER_df$RESULT[i] <- LTER_df$RESULT[i] * 12.15
  }
  else if(LTER_df$VARIABLE[i] == "Na."){
    #Units in meq/L
    LTER_df$VARIABLE[i] <-   "SODIUM"
    LTER_df$RESULT[i] <- LTER_df$RESULT[i] * 22.99
  }
  else if(LTER_df$VARIABLE[i] == "K."){
    #Units in meq/L
    LTER_df$VARIABLE[i] <-   "POTASSIUM"
    LTER_df$RESULT[i] <- LTER_df$RESULT[i] * 39.1
  }
  else if(LTER_df$VARIABLE[i] == "Cl."){
    #Units in meq/L
    LTER_df$VARIABLE[i] <-   "CHLORIDE"
    LTER_df$RESULT[i] <- LTER_df$RESULT[i] * 35.45
  }
  else if(LTER_df$VARIABLE[i] == "SO4.."){
    #Units in meq/L
    LTER_df$VARIABLE[i] <-   "SULFATE"
    LTER_df$RESULT[i] <- LTER_df$RESULT[i] * 48.03
  }
  else if(LTER_df$VARIABLE[i] == "TN" | 
          LTER_df$VARIABLE[i] == "PN" |
          LTER_df$VARIABLE[i] == "DON"){
    ##Units in mmol/L
    LTER_df$RESULT[i] <- LTER_df$RESULT[i] * (10/2.80112)
  }
  else if(LTER_df$VARIABLE[i] == "TDN" & grepl("NWT", LTER_df$SITE_ID[i])){
    #only NWT datasets have this in mmol/l
      LTER_df$RESULT[i] <- LTER_df$RESULT[i] * (10/2.80112)
      LTER_df$UNIT[i] <- "test"
  }
  else if(LTER_df$VARIABLE[i] == "TP" | LTER_df$VARIABLE[i] == "TDP"){
    #Convert from mmol FOR NWT and ug/L FOR HURON
    if(grepl("NWT", LTER_df$SITE_ID[i])){
      LTER_df$RESULT[i] <- LTER_df$RESULT[i] * (10/3.095975)
      LTER_df$UNIT[i] <- "test"
    }
    else if (grepl("Huron", LTER_df$SITE_ID[i])){
      LTER_df$RESULT[i] <- LTER_df$RESULT[i] / 1000
      LTER_df$UNIT[i] <- "test"
    }
  }
  else if(LTER_df$VARIABLE[i] == "DOP" |
          LTER_df$VARIABLE[i] == "IP" |
          LTER_df$VARIABLE[i] == "PP"){
    #Convert from mmol/L
    LTER_df$RESULT[i] <- LTER_df$RESULT[i] * (10/3.095975)
    
  }
  else if(LTER_df$VARIABLE[i] == "Chla"){
    LTER_df$VARIABLE[i] <-   "CHLA"    
    LTER_df$RESULT[i] <- LTER_df$RESULT[i] / 1000
  }
  else if(LTER_df$VARIABLE[i] == "Ammonium"){
    #convert to AS N and from ug/L
    LTER_df$VARIABLE[i] <-   "NH4 as N"    
    LTER_df$RESULT[i] <- LTER_df$RESULT[i] / 1.2878 / 1000
  }
  else if(LTER_df$VARIABLE[i] == "cl"){
    #rename
    LTER_df$VARIABLE[i] <-   "CHLORIDE"
  }
  else if(LTER_df$VARIABLE[i] == "nh4_n"){
    #rename
    LTER_df$VARIABLE[i] <-   "NH4 as N"
  }
  else if(LTER_df$VARIABLE[i] == "no3"){
    #Convert to NO3 as N
    LTER_df$VARIABLE[i] <-   "NO3 as N"
    LTER_data$RESULT[i] <- LTER_df$RESULT[i] * 0.2259
  }
  else if(LTER_df$VARIABLE[i] == "so4"){
    LTER_df$VARIABLE[i] <-   "SULFATE"
    #rename
  }
  else if(LTER_df$VARIABLE[i] == "tdn"){
    LTER_df$VARIABLE[i] <-   "TDN"
    #rename
  }
  else if(LTER_df$VARIABLE[i] == "tdp"){
    LTER_df$VARIABLE[i] <-   "TDP"
     #rename
  }
  else if(LTER_df$VARIABLE[i] == "tn_1"){
    LTER_df$VARIABLE[i] <-   "TN"
    #rename
  }
  else if(LTER_df$VARIABLE[i] == "tp"){
    LTER_df$VARIABLE[i] <-   "TP"
    #rename
  }
  else if(LTER_df$VARIABLE[i] == "wtemp"){
    LTER_df$VARIABLE[i] <-   "TEMP"
    #rename
  }
  else if(LTER_df$VARIABLE[i] == "do"){
    LTER_df$VARIABLE[i] <-   "DO"
    #rename
  }
  else if(LTER_df$VARIABLE[i] == "ph_field"){
    LTER_df$VARIABLE[i] <-   "pH"
    #rename
  }
  else if(LTER_df$VARIABLE[i] == "no2"){#rename
    LTER_df$VARIABLE[i] <-   "NO2 as N"
    }
  
}
#conversions from this website https://www.knowyourh2o.com/outdoor-3/conversion-factors-for-water-quality
#http://www.endmemo.com/medical/unitconvert/Phosphorus.php


print(unique(LTER_df$VARIABLE))

#for loop for units
for (i in 1:nrow(LTER_df)){
  if(LTER_df$VARIABLE[i] == "pH"){
    LTER_df$UNIT[i] <- NA
  }
  else if(LTER_df$VARIABLE[i] == "SP_COND"){
    #uS/cm
    LTER_df$UNIT[i] <- "uS/cm"
  }
  else if(LTER_df$VARIABLE[i] == "TEMP"){
    #deg_C
    LTER_df$UNIT[i] <- "deg_C"
  }
  else {
    LTER_df$UNIT[i] <- "mg/L"
  }
}

#Remove params we won't use

# (unique(LTER_df$VARIABLE[!LTER_df$VARIABLE %in% c("DOC", 
#                                 "CHLA",
#                                 "NO3 as N",
#                                 "NO2 as N",
#                                 "PO4 as P",
#                                 "NH4 as N",
#                                 "TKN",
#                                 "TP",
#                                 "TN",
#                                 "TDN",
#                                 "TDP",
#                                 "PN",
#                                 "IN",
#                                 "CALCIUM",
#                                 "CHLORIDE",
#                                 "SP_COND",
#                                 "MAGNESIUM",
#                                 "pH",
#                                 "SULFATE",
#                                 "TEMP",
#                                 "DO",
#                                 "DOP",
#                                 "IP",
#                                 "PP",
#                                 "TOC",
#                                 "POC",
#                                 "DON",
#                                 "SODIUM", 
#                                 "POTASSIUM")]))

LTER_1 <-subset(LTER_df, VARIABLE %in% c("DOC", 
                                           "CHLA",
                                           "NO3 as N",
                                           "NO2 as N",
                                           "PO4 as P",
                                           "NH4 as N",
                                           "TKN",
                                           "TP",
                                           "TN",
                                           "TDN",
                                           "TDP",
                                           "PN",
                                           "CALCIUM",
                                           "CHLORIDE",
                                           "SP_COND",
                                           "MAGNESIUM",
                                           "pH",
                                           "SULFATE",
                                           "TEMP",
                                           "DO",
                                           "DOP",
                                           "IP",
                                           "PP",
                                           "TOC",
                                           "POC",
                                           "DON",
                                           "SODIUM", 
                                           "POTASSIUM"))
print(data.frame(unique(LTER_1$VARIABLE)))

LTER_1 <- LTER_1[,3:9]
#Write csv into other_vars_datasets folder
write.csv(LTER_1, "Data/other_vars_datasets/LTER_1.csv", row.names = FALSE)
















