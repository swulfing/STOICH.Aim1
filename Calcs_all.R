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
  geom_point(data = NLAwl2016, col = "pink")+
  labs(x = "DOC",
       y = "TP")

All.DOC.TP  

#DOC TN
All.DOC.TN<- ggplot(NULL, aes(DOC_Median, TN_Median))+
  geom_point(data = Chla, col = "red")+
  geom_point(data = NLA2007, col = "orange")+
  geom_point(data = NLArs2008, col = "yellow")+
  geom_point(data = NLArs2013, col = "green")+
  geom_point(data = NLArs2019, col = "blue")+
  geom_point(data = NLAs2004, col = "purple")+
  geom_point(data = NLAwl2016, col = "pink")+
  labs(x = "DOC",
       y = "TN")

All.DOC.TN

#TN to TP
All.TN.TP<- ggplot(NULL, aes(TN_Median, TP_Median))+
  geom_point(data = Chla, col = "red")+
  geom_point(data = NLA2007, col = "orange")+
  geom_point(data = NLArs2008, col = "yellow")+
  geom_point(data = NLArs2013, col = "green")+
  geom_point(data = NLArs2019, col = "blue")+
  geom_point(data = NLAs2004, col = "purple")+
  geom_point(data = NLAwl2016, col = "pink")+
  +
  labs(x = "TN",
       y = "TP")

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
  geom_point(data = NLAwl2016, col = "pink")+
  labs(x = "DOC",
       y = "TN:TP")

All.DOC.TNTP

##Lakes##
#DOC TP
All.DOC.TP<- ggplot(NULL, aes(DOC_Median, TP_Median))+
  geom_point(data = Chla, col = "red")+
  geom_point(data = NLA2007, col = "orange")+
  geom_point(data = NLArs2008, col = "yellow")+
  geom_point(data = NLArs2013, col = "green")+
  geom_point(data = NLArs2019, col = "blue")+
  geom_point(data = NLAs2004, col = "purple")+
  geom_point(data = NLAwl2016, col = "pink")+
  labs(x = "DOC",
       y = "TP")

All.DOC.TP  

#DOC TN
Type.DOC.TN<- ggplot(NULL, aes(DOC_Median, TN_Median))+
  geom_point(data = NLArs2008, col = "green")+
  geom_point(data = NLArs2013, col = "green")+
  geom_point(data = NLArs2019, col = "green")+
  geom_point(data = NLAs2004, col = "green")+
  geom_point(data = Chla, col = "blue")+
  geom_point(data = NLA2007, col = "blue")+
  #geom_point(data = NLAwl2016, col = "pink")+
  labs(x = "DOC",
       y = "TN")

Type.DOC.TN

#TN to TP
Type.TN.TP<- ggplot(NULL, aes(TN_Median, TP_Median))+
  geom_point(data = NLArs2008, col = "green")+
  geom_point(data = NLArs2013, col = "green")+
  geom_point(data = NLArs2019, col = "green")+
  geom_point(data = NLAs2004, col = "green")+
  geom_point(data = Chla, col = "blue")+
  geom_point(data = NLA2007, col = "blue")+
  #geom_point(data = NLAwl2016, col = "pink")+
  labs(x = "TN",
       y = "TP")

Type.TN.TP


#Doc to NP Ratio
All.DOC.TNTP<- ggplot(NULL, aes(DOC_Median, TNtoTP_Median))+
  geom_point(data = Chla, col = "blue")+
  geom_point(data = NLA2007, col = "blue")+
  geom_point(data = NLArs2008, col = "green")+
  geom_point(data = NLArs2013, col = "green")+
  geom_point(data = NLArs2019, col = "green")+
  geom_point(data = NLAs2004, col = "green")+
  geom_point(data = NLAwl2016, col = "pink")+
  labs(x = "DOC",
       y = "TN:TP")

All.DOC.TNTP

##NO3 Data##

#DOC TP
Nitrate.DOC.TP<- ggplot(NULL, aes(DOC_Median, TP_Median))+
  geom_point(data = NLA2007, col = "red")+
  geom_point(data = NLArs2008, col = "yellow")+
  geom_point(data = NLArs2019, col = "blue")+
  geom_point(data = NLAs2004, col = "purple")+
  labs(x = "DOC",
       y = "TP")

Nitrate.DOC.TP  

#DOC TN
Nitrate.DOC.NO3<- ggplot(NULL, aes(DOC_Median, NO3_Median))+
  #geom_point(data = NLA2007, col = "red")+
  geom_point(data = NLArs2008, col = "yellow")+
  geom_point(data = NLArs2019, col = "blue")+
  geom_point(data = NLAs2004, col = "purple")+
  labs(x = "DOC",
       y = "NO3")

Nitrate.DOC.NO3

#TN to TP
Nitrate.TN.TP<- ggplot(NULL, aes(TNO3_Median, TP_Median))+
  geom_point(data = NLA2007, col = "red")+
  geom_point(data = NLArs2008, col = "yellow")+
  geom_point(data = NLArs2019, col = "blue")+
  geom_point(data = NLAs2004, col = "purple")+
  labs(x = "DOC",
       y = "TP")

Nitrate.TN.TP


#Doc to NP Ratio
#DOC TN
Nitrate.DOC.TNTP<- ggplot(NULL, aes(DOC_Median, NO3toTP_Median))+
  #geom_point(data = NLA2007, col = "red")+
  geom_point(data = NLArs2008, col = "yellow")+
  geom_point(data = NLArs2019, col = "blue")+
  geom_point(data = NLAs2004, col = "purple")+
  labs(x = "DOC",
       y = "NO3/TP")

Nitrate.DOC.TNTP



