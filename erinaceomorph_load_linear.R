#Load up data, clean
setwd(datadir)
raw.data<-read_excel("../all_eulipotyphla.xlsx")

#get only taxon and tooth positions that were measured (i.e., not NA).
c.data<-filter(raw.data,id.new==taxon) %>% 
  filter(.,!is.na(length1)) %>% filter(.,length1!="NA") %>%
  filter(.,element.new==tooth[1]|element.new==tooth[2])

#fix biozones
c.data$Faunal.Zone<-tolower(c.data$Faunal.Zone)
# fix meter levels to at least be consistent
c.data$Faunal.Zone[which(c.data$Locality=="v07055")]<-"wa-1"
c.data$Faunal.Zone[which(c.data$Meter.Level==72.52)]<-"wa-2"

#make meter level numeric
c.data$Meter.Level<-as.numeric(c.data$Meter.Level)
#make measurements numeric
c.data[,c(23:61)]<- lapply(c.data[,c(23:61)], function(x) as.numeric(as.character(x)))

#take averages of triplicate measurements
c.data$length<-cbind(c.data$length1,c.data$length2,c.data$length3) %>% apply(.,1,mean)
c.data$tri.w<-cbind(c.data$trigonid_width1,c.data$trigonid_width2,c.data$trigonid_width3) %>% apply(.,1,mean)
c.data$tal.w<-cbind(c.data$talonid_width1,c.data$talonid_width2,c.data$talonid_width3) %>% apply(.,1,mean)
c.data$tri.l<-cbind(c.data$trigonid_length1,c.data$trigonid_length2,c.data$trigonid_length3) %>% apply(.,1,mean)

c.data$ic.eh<-cbind(c.data$ic_ento_hypocu1,c.data$ic_ento_hypocu2,c.data$ic_ento_hypocu3) %>% apply(.,1,mean)
c.data$ic.hh<-cbind(c.data$ic_hypoco_hypocu1,c.data$ic_hypoco_hypocu2,c.data$ic_hypoco_hypocu3) %>% apply(.,1,mean)
c.data$tri.h<-cbind(c.data$trigonid_height1,c.data$trigonid_height2,c.data$trigonid_height3) %>% apply(.,1,mean)
c.data$tal.h<-cbind(c.data$talonid_notch1,c.data$talonid_notch2,c.data$talonid_notch3) %>% apply(.,1,mean)
c.data$ic.me<-cbind(c.data$ic_meta_ento1,c.data$ic_meta_ento2,c.data$ic_meta_ento3) %>% apply(.,1,mean)
c.data$met.l<-cbind(c.data$metaconid_length1,c.data$metaconid_length2,c.data$metaconid_length3) %>% apply(.,1,mean)
c.data$met.w<-cbind(c.data$meta_width1,c.data$meta_width2,c.data$meta_width3) %>% apply(.,1,mean)
c.data$ento.l<-cbind(c.data$ento_length1,c.data$ento_length2,c.data$ento_length3) %>% apply(.,1,mean)

c.data$cant<-cbind(c.data$angle_canting1,c.data$angle_canting2,c.data$angle_canting3) %>% apply(.,1,mean)

c.data$heights<-c.data$tri.h/c.data$tal.h
c.data$ic.me.l<-c.data$ic.me/c.data$length
c.data$met.l.l<-c.data$met.l/c.data$length
c.data$met.w.l<-c.data$met.w/c.data$length
c.data$ic.hh.l<-c.data$ic.hh/c.data$length
c.data$tri.w.l<-c.data$tri.w/c.data$length
c.data$tal.w.l<-c.data$tal.w/c.data$length
c.data$tri.h.l<-c.data$tri.h/c.data$length
c.data$met.w.tri<-c.data$met.w/c.data$tri.w
c.data$ento.l.l<-c.data$ento.l/c.data$length

#calculate crown area
c.data$lnlxw<-(c.data$length*c.data$tal.w) %>% log(.)

# m1 crown area ------
m1.measures<-filter(raw.data,id.new==taxon) %>% 
  filter(.,!is.na(length1)) %>% filter(.,length1!="NA") %>%
  filter(.,element.new=="M/1"|element.new=="M/1*")

#fix biozones
m1.measures$Faunal.Zone<-tolower(m1.measures$Faunal.Zone)
# fix meter levels to at least be consistent
m1.measures$Faunal.Zone[which(m1.measures$Locality=="v07055")]<-"wa-1"
m1.measures$Faunal.Zone[which(m1.measures$Meter.Level==72.52)]<-"wa-2"

#make meter level numeric
m1.measures$Meter.Level<-as.numeric(m1.measures$Meter.Level)
#make measurements numeric
m1.measures[,c(23:61)]<- lapply(m1.measures[,c(23:61)], function(x) as.numeric(as.character(x)))

m1.measures$length<-cbind(m1.measures$length1,m1.measures$length2,m1.measures$length3) %>% apply(.,1,mean)
m1.measures$tal.w<-cbind(m1.measures$talonid_width1,m1.measures$talonid_width2,m1.measures$talonid_width3) %>% apply(.,1,mean)


m1.measures$lnlxw<-(m1.measures$length*m1.measures$tal.w) %>% log(.)

# stratigraphic cleanup ----------
#do some quick renames to ease plotting
colnames(c.data)[which(colnames(c.data)=="Meter.Level")]<-"meter_level"

#try bins again
c.data<-binsBHB(c.data, c.data$meter_level)
c.data<-bins.forMS(c.data)
c.data<-binsMinimal(c.data)

# same for m1 data -------
#do some quick renames to ease plotting
colnames(m1.measures)[which(colnames(m1.measures)=="Meter.Level")]<-"meter_level"

#binning
m1.measures<-binsBHB(m1.measures, m1.measures$meter_level)
m1.measures<-bins.forMS(m1.measures)
m1.measures<-binsMinimal(m1.measures)

warnings()