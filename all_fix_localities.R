# Script to clean up locality information in the all_eulipotyphla spreadsheet. Used once. 

# install.packages("pillar")
library(tidyverse)
library(readxl)
library(ggplot2)

datadir<-"D:/Dropbox/Documents/Dissertation/sys_eulipotyphla"
setwd(datadir)
localities<-read_excel("../notes/db/Master Locality Database.xlsx")
raw.data<-read_excel("all_eulipotyphla.xlsx")

#focus on lowers
low.data<-raw.data
#which localities can't yet be linked to Locality Database?
which(unique(low.data$Locality) %in% localities$Locality == FALSE) #initial check

#manual fixes
low.data$Locality[which(low.data$Locality=="v08080a"|low.data$Locality=="v08080b")]<-"v08080"
low.data$Locality[which(low.data$Locality=="v08087B"|low.data$Locality=="v08087b?")]<-"v08087b"
low.data$Locality[which(low.data$Locality=="v15055")]<-"v15055A"
low.data$Locality[which(low.data$Locality=="UW V-73037")]<-"v16001A"
low.data$Locality[which(low.data$Locality=="UW V-73027")]<-"v16003"
low.data$Locality[which(low.data$Locality=="UW V-73034")]<-"v16008"
low.data$Locality[which(low.data$Locality=="WY15059")]<-"v15059"
low.data$Locality[which(low.data$Locality=="V16002A")]<-"v16002A"
low.data$Locality[which(low.data$Locality=="v17005")]<-"v17005A"


#UW V-73023 is at Bown's 31 M level as compared to 30M of v-73027
unique(low.data$Locality)[c(81)] #okay


#Now sub in meter level, biozone
for (row in 1:nrow(low.data)){
  local.link<-which(localities$Locality %in% low.data$Locality[row]) #find matching row
  if (localities$Biozone[local.link] %>% unique %>% length > 1){
    paste("Conflicting biozone info for site",low.data$Locality[row]) %>%
      print #check for conflict in site database
  }
  else{
    #if specimen biozone blank, put in locality biozone
    low.data$Faunal.Zone[row]<-localities$Biozone[local.link[1]]
    low.data$Meter.Level[row]<-localities$MeterLevel[local.link[1]]
  }
}

low.data$Meter.Level[which(low.data$Locality=="v15055A")]

write.csv(low.data,"all_eulipotyphla2.csv")

#Other manual changes:
#UW V-73023 is at Bown's 31 M level as compared to 30M of v-73027
#v14046 has no meter level but is close to Amy's Hill, also put at 27
#v15048 and v15058 both Wa-2, arbitrarily placed at 70m level

