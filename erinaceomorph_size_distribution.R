# !diagnostics off
library(reshape2) #to melt CVs later
library(ggthemes) #to get Paul Tol colors

#needs erinaceomorph_dependencies

erin.col<-ggthemes_data$ptol$qualitative[[3]] #Tol colors for critters

#Load up data, clean

setwd(datadir)
raw.data<-read_excel("../all_eulipotyphla.xlsx")

#get only M/1s that were measured (i.e., not NA).
#remove all data in Cf-3 and Wa-m, also. Not enough data there.
c.data<-filter(raw.data,!is.na(length1)) %>% filter(.,length1!="NA") %>%
  filter(.,element.new=="M/1"|element.new=="M/1*") %>% 
  filter(.,Faunal.Zone!="Cf-3"&Faunal.Zone!="Wa-M") %>%
  filter(.,Faunal.Zone!="YPM30655"&Faunal.Zone!="USGS3676") #remove comparatives

c.data$Faunal.Zone %>% unique

# fixes for format ------
#biozones to lower. Inherited choice.
c.data$Faunal.Zone<-tolower(c.data$Faunal.Zone)

#make meter level numeric
c.data$Meter.Level<-as.numeric(c.data$Meter.Level)
#make measurements numeric
c.data[,c(23:61)]<- lapply(c.data[,c(23:61)], function(x) as.numeric(as.character(x)))

# process measurements ------
#take averages of triplicate measurements
c.data$length<-cbind(c.data$length1,c.data$length2,c.data$length3) %>% apply(.,1,mean)
c.data$tal.w<-cbind(c.data$talonid_width1,c.data$talonid_width2,c.data$talonid_width3) %>% apply(.,1,mean)
#calculate crown area
c.data$lnlxw<-(c.data$length*c.data$tal.w) %>% log(.)

c.data$tal.w[which(c.data$id.new=="Pontifactor")] %>% mean(.,na.rm=TRUE)
c.data$tal.w[which(c.data$id.new=="Talpavoides"&c.data$Faunal.Zone!="Wa-0")] %>% 
  mean(.,na.rm=TRUE)

c.data$length[which(c.data$id.new=="Pontifactor")] %>% mean(.,na.rm=TRUE)
c.data$length[which(c.data$id.new=="Talpavoides"&c.data$Faunal.Zone!="Wa-0")] %>% 
  mean(.,na.rm=TRUE)

c.data$lnlxw[which(c.data$id.new=="Pontifactor")] %>% mean(.,na.rm=TRUE)
c.data$lnlxw[which(c.data$id.new=="Talpavoides"&c.data$Faunal.Zone!="Wa-0")] %>% 
  mean(.,na.rm=TRUE)
# make bins -----
#make meter level numeric
#do some quick renames to ease plotting
colnames(c.data)[which(colnames(c.data)=="Meter.Level")]<-"meter_level"

#try bins again
c.data$bin2<-NA
c.data$bin2[which(c.data$meter_level<5)]<-"Cf-3"
c.data$bin2[which(c.data$meter_level>5&c.data$meter_level<14.46)]<-"Wa-M"
c.data$bin2[which(c.data$meter_level>14.45&c.data$meter_level<25)]<-"Wa-0 1"
c.data$bin2[which(c.data$meter_level>25&c.data$meter_level<41)]<-"Wa-0 2"
c.data$bin2[which(c.data$meter_level>41&c.data$meter_level<46)]<-"Wa-0 3"
c.data$bin2[which(c.data$meter_level>46&c.data$meter_level<69.999)]<-"Wa-1"
c.data$bin2[which(c.data$meter_level>69.999)]<-"Wa-2"
c.data$bin2<-factor(c.data$bin2,levels=bins)

# calculate per bin ----
by_bin<-group_by(c.data,id.new,bin2)
sizes<-summarise(by_bin,area=mean(lnlxw, na.rm=TRUE))

# substitute presences -----
#extend Centetodon patratus to Wa-2. It's reported from Four Mile
sizes[nrow(sizes)+1,]<-c("Centetodon patratus","Wa-2",
                         sizes$area[which(sizes$id.new=="Centetodon patratus")])

#Geolabididae small should be extended to Wa-0 1
sizes[nrow(sizes)+1,]<-c("Geolabididae small","Wa-0 1",
                         sizes$area[which(sizes$id.new=="Geolabididae small")])

#Leptacodon donkroni should be extended into Wa-2,and Wa-0 3
ld.area<-sizes$area[which(sizes$id.new=="Leptacodon donkroni"&(
             sizes$bin2=="Wa-0 2"|sizes$bin2=="Wa-1"))] %>%  as.numeric %>% mean

sizes[nrow(sizes)+1,]<-c("Leptacodon donkroni","Wa-0 3",ld.area)
sizes[nrow(sizes)+1,]<-c("Leptacodon donkroni","Wa-2",ld.area)

#Plagioctenodon krausae should be extended into Wa-0 3
pk.area<-sizes$area[which(sizes$id.new=="Plagioctenodon krausae"&(
  sizes$bin2=="Wa-0 2"|sizes$bin2=="Wa-1"))] %>%  as.numeric %>% mean

sizes[nrow(sizes)+1,]<-c("Plagioctenodon krausae","Wa-0 3",pk.area)

#Geolabididae medium M/1s are provisional, but should extended to Wa-0 3
sizes[nrow(sizes)+1,]<-c("Geolabididae medium?","Wa-0 3",
                         sizes$area[which(sizes$id.new=="Geolabididae medium?")])

#Talpavoides should extended to Wa-0 3
td.area<-sizes$area[which(sizes$id.new=="Talpavoides"&(
  sizes$bin2=="Wa-0 2"|sizes$bin2=="Wa-1"))] %>%  as.numeric %>% mean

sizes[nrow(sizes)+1,]<-c("Talpavoides","Wa-0 3", td.area)

#Plagioctenoides should extended to Wa-0 3 and Wa-1
pg.area<-sizes$area[which(sizes$id.new=="Plagioctenoides"&(
  sizes$bin2=="Wa-0 2"|sizes$bin2=="Wa-2"))] %>%  as.numeric %>% mean

sizes[nrow(sizes)+1,]<-c("Plagioctenoides","Wa-0 3", pg.area)
sizes[nrow(sizes)+1,]<-c("Plagioctenoides","Wa-1", pg.area)

#Plagioctenodon savagei should be extended into Wa-2
sizes[nrow(sizes)+1,]<-c("Plagioctenodon savagei","Wa-2",
                         sizes$area[which(sizes$id.new=="Plagioctenodon savagei")])

#add plotting meter level ----
sizes$meter<-0
sizes$meter[which(sizes$bin2=="Wa-0 1")]<-mean(c(14.45,25))
sizes$meter[which(sizes$bin2=="Wa-0 2")]<-mean(c(25,41))
sizes$meter[which(sizes$bin2=="Wa-0 3")]<-mean(c(41,46))
sizes$meter[which(sizes$bin2=="Wa-1")]<-mean(c(46,69.999))
sizes$meter[which(sizes$bin2=="Wa-2")]<-mean(c(69.999,75))

# plot -----
#create color grouping variable
sizes$highlight<-"all other"
sizes$highlight[which(sizes$id.new=="Colpocherus")]<-"Colpocherus"
sizes$highlight[which(sizes$id.new=="Macrocranion")]<-"Macrocranion"
sizes$highlight[which(sizes$id.new=="Talpavoides")]<-"Talpavoides"

sizes$area<-as.numeric(sizes$area)

#take out Batodonoides: too small, confuses plot
sizes2<-sizes[-which(sizes$id.new=="Geolabididae small"),]
#quickplot
ggplot(data=sizes2, aes(x=area,y=meter,fill=highlight))+
  geom_point(size=4,shape=21) +
  scale_fill_manual(name="Taxon",values=c("black",wes_palette("Moonrise2")))

# Now start incorporating CV
# from Sakai 1981
CV<-mean(c(4.25,6.05))
#use a single estimate of standard deviation
#get to SD of ln(area) from CV of area by dividing by 100: Lewontin 1966
std.dev<-(sizes2$area*(CV/100)) %>% mean 
sizes2$upper.sd<-sizes2$area+(std.dev*2) #2 standard deviations
sizes2$lower.sd<-sizes2$area-(std.dev*2)

#make things a tiny bit easier?
sizes2$bin4<-as.character(sizes2$bin2)
sizes2$bin4[which(sizes2$bin4=="Wa-0 1")]<-"early Wa-0"
sizes2$bin4[which(sizes2$bin4=="Wa-0 2")]<-"mid-Wa-0"
sizes2$bin4[which(sizes2$bin4=="Wa-0 3")]<-"late Wa-0"
sizes2$bin4<-factor(sizes2$bin4,levels=c("early Wa-0","mid-Wa-0","late Wa-0","Wa-1","Wa-2"))
# sizes3<-melt(data=as.data.frame(sizes2),id.vars=c("id.new","bin2","highlight","meter"))
# 
# ggplot(data=sizes3, aes(x=bin2,y=value,bin=id.new,fill=highlight))+
#   geom_boxplot(position=position_dodge(width=0),width=3,alpha=.75) +
#   coord_flip()+
#   scale_fill_manual(name="Taxon",values=c("black",erin.col)) +
#   xlab("ln(L x W)") + ylab("Biozone") + theme_minimal()+
#   theme(text=element_text(size=20,family="Franklin Gothic Book"))

cairo_pdf("figure_parts/insectivore_size_distribution.pdf",width=3.5,height=3.5,family="Franklin Gothic Book")
ggplot(data=sizes2, aes(x=bin4,bin=id.new,fill=highlight))+
  geom_boxplot(aes(ymin=lower.sd,lower=lower.sd,middle=area,upper=upper.sd,ymax=upper.sd,
                   width=3),lwd=0.05,
               stat="identity",position=position_dodge(width=0),alpha=0.75) + 
  coord_flip()+ ylim(-0.3,1.2) +
  scale_fill_manual(name="Taxon",values=c("black",erin.col)) +
  ylab("ln(L x W)") + xlab("Biozone") + theme_minimal()+
  theme(text=element_text(size=10,family="Franklin Gothic Book"),
        legend.position=c(0.85,0.25))
dev.off()
embedFonts("figure_parts/insectivore_size_distribution.pdf")

#find specific critters in specific bins
filter(sizes2, bin4=="early Wa-0") %>% arrange(.,area)

#quick save, not for publication

print.data.frame(sizes[which(sizes$bin2=="Wa-2"),])
print.data.frame(sizes[which(sizes$bin2=="Wa-0 1"),])
print.data.frame(sizes)
print.data.frame(sizes[which(sizes$id.new=="Plagioctenoides"),])


