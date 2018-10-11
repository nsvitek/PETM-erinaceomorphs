# Script to create standardized bins for Bighorn Basin data, based on meter level. 

#this function can probably be improved further by tighter links
#between the bins object and the binsBHB function. 

bins<-c("Cf-3", "Wa-M", "Wa-0 1", "Wa-0 2","Wa-0 3","Wa-1","Wa-2") %>% 
  factor(.,levels=.)

#Biozone meter levels agreed upon with J.I.B. as of 8 June 2018
binsBHB<-function(dataframe,meter_level){
  dataframe$bin2<-NA
  dataframe$bin2[which(meter_level<5)]<-"Cf-3"
  dataframe$bin2[which(meter_level>5&meter_level<14.46)]<-"Wa-M"
  dataframe$bin2[which(meter_level>14.45&meter_level<25)]<-"Wa-0 1"
  dataframe$bin2[which(meter_level>25&meter_level<41)]<-"Wa-0 2"
  dataframe$bin2[which(meter_level>41&meter_level<46)]<-"Wa-0 3"
  dataframe$bin2[which(meter_level>46&meter_level<60)]<-"Wa-1"
  dataframe$bin2[which(meter_level>60.000001)]<-"Wa-2"
  dataframe$bin2<-factor(dataframe$bin2,levels=bins)
  return(dataframe)
}
  
binmean<-c(mean(c(0.1,5)),mean(c(5,14.45)),mean(c(14.45,25)),
           mean(c(25,41)),mean(c(41,46)),mean(c(46,60)),mean(c(60,75)))
