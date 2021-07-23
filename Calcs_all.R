library("ggplot2")

brazil <-data.frame(read.csv("C:/Users/sophi/Documents/STOICH/STOICH.Aim1/MedianCalcs/BrazilData_Medians.csv"))
Chla <-data.frame(read.csv("C:/Users/sophi/Documents/STOICH/STOICH.Aim1/MedianCalcs/ChlaData_Medians.csv"))
NLA2007 <-data.frame(read.csv("C:/Users/sophi/Documents/STOICH/STOICH.Aim1/MedianCalcs/NLA2007_Medians.csv"))
NLArs2008 <-data.frame(read.csv("C:/Users/sophi/Documents/STOICH/STOICH.Aim1/MedianCalcs/NLArs2008_Medians.csv"))
NLArs2013 <-data.frame(read.csv("C:/Users/sophi/Documents/STOICH/STOICH.Aim1/MedianCalcs/NLArs2013_Medians.csv"))
NLArs2019 <-data.frame(read.csv("C:/Users/sophi/Documents/STOICH/STOICH.Aim1/MedianCalcs/NLArs2019_Medians.csv"))
NLAs2004 <-data.frame(read.csv("C:/Users/sophi/Documents/STOICH/STOICH.Aim1/MedianCalcs/NLAs2004_Medians.csv"))
NLAwl2016 <-data.frame(read.csv("C:/Users/sophi/Documents/STOICH/STOICH.Aim1/MedianCalcs/NLAwl2016_Medians.csv"))


##All Data##

#DOC TP
All.DOC.TP<- ggplot(NULL, aes(DOC_Median, TP_Median))+
  geom_point(data = Chla, col = "red")+
  geom_point(data = NLA2007, col = "orange")+
  geom_point(data = NLArs2008, col = "yellow")+
  geom_point(data = NLArs2013, col = "green")+
  geom_point(data = NLArs2019, col = "blue")+
  geom_point(data = NLAs2004, col = "purple")+
  geom_point(data = NLAwl2016, col = "pink")
  
All.DOC.TP  

#DOC TN
All.DOC.TN<- ggplot(NULL, aes(DOC_Median, TN_Median))+
  geom_point(data = Chla, col = "red")+
  geom_point(data = NLA2007, col = "orange")+
  geom_point(data = NLArs2008, col = "yellow")+
  geom_point(data = NLArs2013, col = "green")+
  geom_point(data = NLArs2019, col = "blue")+
  geom_point(data = NLAs2004, col = "purple")+
  geom_point(data = NLAwl2016, col = "pink")

All.DOC.TN

#TN to TP
All.TN.TP<- ggplot(NULL, aes(TN_Median, TP_Median))+
  geom_point(data = Chla, col = "red")+
  geom_point(data = NLA2007, col = "orange")+
  geom_point(data = NLArs2008, col = "yellow")+
  geom_point(data = NLArs2013, col = "green")+
  geom_point(data = NLArs2019, col = "blue")+
  geom_point(data = NLAs2004, col = "purple")+
  geom_point(data = NLAwl2016, col = "pink")

All.TN.TP


#Doc to NP Ratio
#DOC TN
All.DOC.TNTP<- ggplot(NULL, aes(DOC_Median, TNtoTP_Median))+
  geom_point(data = Chla, col = "red")+
  geom_point(data = NLA2007, col = "orange")+
  geom_point(data = NLArs2008, col = "yellow")+
  geom_point(data = NLArs2013, col = "green")+
  geom_point(data = NLArs2019, col = "blue")+
  geom_point(data = NLAs2004, col = "purple")+
  geom_point(data = NLAwl2016, col = "pink")

All.DOC.TNTP

##Lakes##


##Rivers##


##NO3 Data##