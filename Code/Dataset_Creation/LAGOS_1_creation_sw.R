library(dplyr)
setwd("~/STOICH.Aim1")

LAGOS_data <- read.csv("Data/other_vars_datasets/LAGOS.csv")
LAGOS_df <- data.frame(LAGOS_data)

#Renaming/converting
print(data.frame(unique(LAGOS_df$VARIABLE)))

for (i in 1:nrow(LAGOS_df)){
  if(LAGOS_df$VARIABLE[i] == "chla_ugl"){
    LAGOS_df$VARIABLE[i] <-   "CHLA"
    LAGOS_df$RESULT[i] <- LAGOS_df$RESULT[i]/1000
    LAGOS_df$UNIT[i] <- "mg/L"
    }
  else if (LAGOS_df$VARIABLE[i] == "nh4n_ugl"){
    LAGOS_df$VARIABLE[i] <- "NH4 as N"
    LAGOS_df$RESULT[i] <- LAGOS_df$RESULT[i]/1000
    LAGOS_df$UNIT[i] <- "mg/L"
  }
  else if (LAGOS_df$VARIABLE[i] == "tkn_ugl"){
    LAGOS_df$VARIABLE[i] <- "TKN"
    LAGOS_df$RESULT[i] <- LAGOS_df$RESULT[i]/1000
    LAGOS_df$UNIT[i] <- "mg/L"
  }
  else if (LAGOS_df$VARIABLE[i] == "tp_ugl"){
    LAGOS_df$VARIABLE[i] <- "TP"
    LAGOS_df$RESULT[i] <- LAGOS_df$RESULT[i]/1000
    LAGOS_df$UNIT[i] <- "mg/L"
  }
  else if (LAGOS_df$VARIABLE[i] == "tn_ugl"){
    LAGOS_df$VARIABLE[i] <- "TN"
    LAGOS_df$RESULT[i] <- LAGOS_df$RESULT[i]/1000
    LAGOS_df$UNIT[i] <- "mg/L"
  }
  else if (LAGOS_df$VARIABLE[i] == "ca_mgl"){
    LAGOS_df$VARIABLE[i] <-  "CALCIUM"
  }
  else if (LAGOS_df$VARIABLE[i] == "cl_mgl"){
    LAGOS_df$VARIABLE[i] <-  "CHLORIDE"
  }
  else if (LAGOS_df$VARIABLE[i] == "spcond_uscm"){
    LAGOS_df$VARIABLE[i] <-  "SP_COND"
    LAGOS_df$UNIT[i] <- "uS/cm"
  }
  else if (LAGOS_df$VARIABLE[i] == "mg_mgl"){
    LAGOS_df$VARIABLE[i] <-  "MAGNESIUM"
  }
  else if (LAGOS_df$VARIABLE[i] == "ph_field"){#I kept the field measurements. Also has equilibrated pH: ph_eq
    LAGOS_df$VARIABLE[i] <-  "pH"
    LAGOS_df$UNIT[i] <- NA
  }
  else if (LAGOS_df$VARIABLE[i] == "so4_mgl"){
    LAGOS_df$VARIABLE[i] <-  "SULFATE"
  }
  else if (LAGOS_df$VARIABLE[i] == "temp_degc"){
    LAGOS_df$VARIABLE[i] <-  "TEMP"
  }
  else if (LAGOS_df$VARIABLE[i] == "do_mgl"){
    LAGOS_df$VARIABLE[i] <-  "DO"
  }
}
print(unique(LAGOS_df$VARIABLE))


#Remove params we won't use

LAGOS_1 <-subset(LAGOS_df, VARIABLE %in% c("DOC",
                                           "CHLA",
                                           "NO3 as N",
                                           "PO4 as P",
                                           "NH4 as N",
                                           "TKN",
                                           "TP",
                                           "TN",
                                           "CALCIUM",
                                           "CHLORIDE",
                                           "SP_COND",
                                           "MAGNESIUM",
                                           "pH",
                                           "SULFATE",
                                           "TEMP",
                                           "DO"))
print(data.frame(unique(LAGOS_1$VARIABLE)))

LAGOS_1 <- LAGOS_1[,2:9]
#Write csv into other_vars_datasets folder
write.csv(LAGOS_1, "Data/other_vars_datasets/LAGOS_1.csv", row.names = FALSE)
















