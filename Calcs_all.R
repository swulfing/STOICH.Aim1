library("ggplot2")

brazil <-data.frame(read.csv("C:/Users/sophi/Documents/STOICH/STOICH.Aim1/MedianCalcs/BrazilData_Medians.csv"))
Chla <-data.frame(read.csv("C:/Users/sophi/Documents/STOICH/STOICH.Aim1/MedianCalcs/ChlaData_Medians.csv"))
NLA2012 <-data.frame(read.csv("C:/Users/sophi/Documents/STOICH/STOICH.Aim1/MedianCalcs/NLA2012_Medians.csv"))
NLA2017 <-data.frame(read.csv("C:/Users/sophi/Documents/STOICH/STOICH.Aim1/MedianCalcs/NLA2017_Medians.csv"))
NLArs2008 <-data.frame(read.csv("C:/Users/sophi/Documents/STOICH/STOICH.Aim1/MedianCalcs/NLArs2008_Medians.csv"))
NLArs2013 <-data.frame(read.csv("C:/Users/sophi/Documents/STOICH/STOICH.Aim1/MedianCalcs/NLArs2013_Medians.csv"))
NLArs2019 <-data.frame(read.csv("C:/Users/sophi/Documents/STOICH/STOICH.Aim1/MedianCalcs/NLArs2019_Medians.csv"))
NLAs2004 <-data.frame(read.csv("C:/Users/sophi/Documents/STOICH/STOICH.Aim1/MedianCalcs/NLAs2004_Medians.csv"))

##All Data##

#DOC TP
All.DOC.TP<- ggplot(NULL, aes(DOC_Median, TP_Median))+
  geom_point(data = NLA2012, col = "blue", pch = 1)+
  geom_point(data = NLA2017, col = "blue", pch = 1)+
  geom_point(data = NLArs2008, col = "green", pch = 1)+
  geom_point(data = NLArs2013, col = "green", pch = 1)+
  geom_point(data = NLArs2019, col = "green", pch = 1)+
  geom_point(data = NLAs2004, col = "green", pch = 1)+
  labs(x = "DOC",
       y = "TP")

All.DOC.TP  


#DOC TN
Rivers.Nitrate.DOC.NO3<- ggplot(NULL, aes(DOC_Median, NO3_Median))+
  geom_point(data = NLArs2008, col = "green", pch = 1)+
  geom_point(data = NLArs2013, col = "green", pch = 1)+
  geom_point(data = NLArs2019, col = "green", pch = 1)+
  geom_point(data = NLAs2004, col = "green", pch = 1)+
  xlim(0,50)+
  ylim(0,10000)+
  labs(x = "DOC",
       y = "NO3")

Rivers.Nitrate.DOC.NO3

Lakes.Nitrate.DOC.NO3<- ggplot(NULL, aes(DOC_Median, NO3_Median))+
  geom_point(data = NLA2012, col = "blue", pch = 1)+
  geom_point(data = NLA2017, col = "blue", pch = 1)+
  xlim(0,100)+
  ylim(0,10)+
  labs(x = "DOC",
       y = "NO3")

Lakes.Nitrate.DOC.NO3

#TN to TP
Nitrate.TP<- ggplot(NULL, aes(NO3_Median, TP_Median))+
  geom_point(data = NLArs2008, col = "green", pch = 1)+
  geom_point(data = NLArs2013, col = "green", pch = 1)+
  geom_point(data = NLArs2019, col = "green", pch = 1)+
  geom_point(data = NLAs2004, col = "green", pch = 1)+
  geom_point(data = NLA2012, col = "blue", pch = 1)+
  geom_point(data = NLA2017, col = "blue", pch = 1)+
  labs(x = "NO3",
       y = "TP")

Nitrate.TP


#Doc to NP Ratio
#DOC TN
Rivers.Nitrate.DOC.TNTP<- ggplot(NULL, aes(DOC_Median, NO3toTP_Median))+
  geom_point(data = NLAs2004, col = "green", pch = 1)+
  geom_point(data = NLArs2008, col = "green", pch = 1)+
  geom_point(data = NLArs2013, col = "green", pch = 1)+
  geom_point(data = NLArs2019, col = "green", pch = 1)+
  xlim(0,50)+
  ylim(0,50)+
  labs(y = "NO3:TP",
       x = "DOC")
  
Rivers.Nitrate.DOC.TNTP

Lakes.Nitrate.DOC.TNTP<- ggplot(NULL, aes(DOC_Median, NO3toTP_Median))+
  geom_point(data = NLA2012, col = "blue", pch = 1)+
  geom_point(data = NLA2017, col = "blue", pch = 1)+
  xlim(0,30)+
  ylim(0,.5)+
  labs(y = "NO3:TP",
       x = "DOC")

Lakes.Nitrate.DOC.TNTP
