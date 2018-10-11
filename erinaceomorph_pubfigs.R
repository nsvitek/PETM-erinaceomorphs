#after having loaded, cleaned erinaceomorph data (up through "clean metadata")
# function for plotting for publication purposes. Here to be tweaked. -----

plot_linear<-function(dataset,variable,x_label,colors=bin.col,o18curve=FALSE){
  if(o18curve==TRUE){
    background<-readJPEG(paste(datadir,"../O18curve.jpg",sep="/"))
    ggplot(data=dataset, aes_string(y="meter_level",x=variable,group="meter_level"))+
      annotation_custom(rasterGrob(background,
                                   width = unit(1,"npc"), height = unit(1,"npc")), -Inf, Inf, -Inf, Inf)+
      geom_hline(yintercept=14.45, linetype=2)+
      geom_hline(yintercept=46, linetype=2)+
      geom_boxplot(alpha=1) +
      geom_point(aes(fill=bin2,shape=bin2),alpha=0.95,size=2) +
      scale_fill_manual(guide=FALSE, 
                        labels=levels(bins)[levels(bins) %in% unique(dataset$bin2)],
                        values=colors[levels(bins) %in% unique(dataset$bin2)]) +
      xlab(x_label) +
      scale_shape_manual(guide=FALSE,
                         labels=levels(bins)[levels(bins) %in% unique(dataset$bin2)],
                         values=c(21,22,23,23,23,24,25)[levels(bins) %in% unique(dataset$bin2)])+
      theme(text=element_text(size=10,family="ArialMT"),axis.title.y=element_blank()) + 
      coord_cartesian(ylim = c(0,75))
  }
  else {
    ggplot(data=dataset, aes_string(y="meter_level",x=variable,group="meter_level"))+
      geom_hline(yintercept=14.45, linetype=2)+
      geom_hline(yintercept=46, linetype=2)+
      geom_boxplot(alpha=1) +
      geom_point(aes(fill=bin2,shape=bin2),alpha=0.95,size=2) +
      scale_fill_manual(guide=FALSE, 
               labels=levels(bins)[levels(bins) %in% unique(dataset$bin2)],
               values=colors[levels(bins) %in% unique(dataset$bin2)]) +
      xlab(x_label) + theme_classic() +
      scale_shape_manual(guide=FALSE,
               labels=levels(bins)[levels(bins) %in% unique(dataset$bin2)],
               values=c(21,22,23,23,23,24,25)[levels(bins) %in% unique(dataset$bin2)])+
      theme(text=element_text(size=10,family="ArialMT"),
            axis.title.y=element_blank()) + 
      coord_cartesian(ylim = c(0,75))
    
  }
}

# make plots: crown area ------
#take care of M/1 crown area first
position<-"M/1"
if(position=="M/1"){pos="M-1"}else if (position=="M/2"){pos="M-2"}
figure_out<-paste(datadir,"/figure_parts/",taxon,"_",pos,"_",sep="")

cairo_pdf(paste(figure_out,"lnlxw.pdf",sep=""),width=6.25/5,height=7/4,family="ArialMT")
plot_linear(dataset=m1.measures[which(m1.measures$meter_level<=75),],
            variable="lnlxw",
            x_label="ln(L x W)",colors=bin.col,
            o18curve=FALSE)
dev.off()
embedFonts(paste(figure_out,"lnlxw.pdf",sep=""))

# dataset$bin2
# dataset<-m1.measures[which(m1.measures$meter_level<=75),]
# # m1.mean<-group_by(dataset,bin2) %>% summarise(.,lnlxw=mean(lnlxw,na.rm=TRUE))
# # m1.mean$meter_level<-binmean[which(bins %in% m1.mean$bin2)]
# # m1.mean<-group_by(dataset,meter_level) %>% summarise(.,lnlxw=mean(lnlxw,na.rm=TRUE))
# ggplot(data=dataset,aes_string(y="meter_level",x="lnlxw"))+
#   geom_hline(yintercept=14.45, linetype=2)+
#   geom_hline(yintercept=46, linetype=2)+
#   # geom_path(data=m1.mean)+ #for now, the lines don't add anything, but this is how to do them. 
#   geom_boxplot(alpha=1,aes(group=meter_level)) +
#   geom_point(aes(fill=bin2,shape=bin2),alpha=0.95,size=2) +
#   scale_fill_manual(guide=FALSE,
#                     labels=levels(bins)[levels(bins) %in% unique(dataset$bin2)],
#                     values=bin.col[levels(bins) %in% unique(dataset$bin2)]) +
#   xlab("ln(L x W)") + theme_classic() +
#   scale_shape_manual(guide=FALSE,
#                      labels=levels(bins)[levels(bins) %in% unique(dataset$bin2)],
#                      values=c(21,22,23,23,23,24,25)[levels(bins) %in% unique(dataset$bin2)])+
#   theme(text=element_text(size=10,family="ArialMT"),
#         axis.title.y=element_blank()) + 
#   coord_cartesian(ylim = c(0,75))

# make plots: dental metrics -----
#now dental metric 
position<-"M/2"
if(position=="M/1"){pos="M-1"}else if (position=="M/2"){pos="M-2"}
figure_out<-paste(datadir,"/figure_parts/",taxon,"_",pos,"_",sep="")

md<-critters[which(critters$posthoc_pos==position),]

cairo_pdf(paste(figure_out,"RFI.pdf",sep=""),width=2.25,height=7/4,family="ArialMT")
plot_linear(dataset=md,variable="RFI2",
            x_label="RFI",colors=bin.col,
            o18curve=FALSE)
dev.off()
embedFonts(paste(figure_out,"RFI.pdf",sep=""))

usnm<-md$specimen_number %>% grep("USNM.*",.,perl = TRUE)
if(length(usnm)==0){md2<-md} else {md2<-md[-usnm,]}

cairo_pdf(paste(figure_out,"DNE.pdf",sep=""),width=2.25,height=7/4,family="ArialMT")
plot_linear(dataset=md2,variable="DNE2",
            x_label="DNE",colors=bin.col,
            o18curve=FALSE)
dev.off()
embedFonts(paste(figure_out,"DNE.pdf",sep=""))

#now linear shape
if (taxon == "Colpocherus"){
  variable<-"met.l.l"
  x_label<-"Relative Metaconid Length" #relative metaconid length
}
if (taxon == "Macrocranion"){
  variable<-"cant"
  x_label<-"Canting Angle"
}
if (taxon == "Talpavoides"){
  variable<-"ic.hh.l"
  x_label<-"RHHID" #relative hypoconid-hypoconulid intercusp distance
}

cairo_pdf(paste(figure_out,"2D.pdf",sep=""),width=2.25,height=7/4,family="ArialMT")
plot_linear(dataset=c.data,variable=variable,
            x_label=x_label,colors=bin.col,
            o18curve=FALSE)
dev.off()
embedFonts(paste(figure_out,"2D.pdf",sep=""))

# legend ------
#make legend
if (taxon == "Talpavoides"){
  cairo_pdf(paste(figure_out,"legend_shape.pdf",sep=""),family="ArialMT")
  ggplot(data=c.data, aes_string(y="meter_level",x=variable,group="meter_level"))+
    geom_point(aes(fill=bin2,shape=bin2),alpha=1,size=4) +
    scale_fill_manual(name="Bin", 
                      labels=levels(bins)[levels(bins) %in% unique(c.data$bin2)],
                      values=bin.col[levels(bins) %in% unique(c.data$bin2)]) +
    scale_shape_manual(name="Bin",
                       labels=levels(bins)[levels(bins) %in% unique(c.data$bin2)],
                       values=c(21,22,23,23,23,24,25)[levels(bins) %in% unique(c.data$bin2)])+
    theme(text=element_text(size=20,family="ArialMT"),axis.title.y=element_blank()) + 
    coord_cartesian(ylim = c(0,75)) 
  dev.off()
  embedFonts(paste(figure_out,"legend_shape.pdf",sep=""))
}

# PCA ------
#make plot of PCA 
cairo_pdf(paste(figure_out,"PCA.pdf",sep=""),width=2.25,height=7/3,family="ArialMT")
PCA.data<-cbind(PCAp$x[,PCs],md)
ggplot(data=PCA.data, aes(x=PC1,y=PC2))+
  geom_point(aes(fill=bin2,shape=bin2),alpha=0.9,size=4) +
  xlab(paste("PC 1 (",an$percent[1],"%)",sep="")) +
  ylab(paste("PC 2 (",an$percent[2],"%)",sep="")) +
  scale_fill_manual(guide=FALSE, name="Bin",
                    labels=levels(bins)[levels(bins) %in% unique(PCA.data$bin2)],
                    values=bin.col[levels(bins) %in% unique(PCA.data$bin2)]) +
  scale_shape_manual(guide=FALSE, name="Bin",
                     labels=levels(bins)[levels(bins) %in% unique(PCA.data$bin2)],
                     values=c(21,22,23,23,23,24,25)[levels(bins) %in% unique(PCA.data$bin2)])+
  theme_classic() +
  theme(text=element_text(size=10,family="ArialMT"))
dev.off()
embedFonts(paste(figure_out,"PCA.pdf",sep=""))

# from Secord et al 2012 data ----
cairo_pdf(paste(datadir,"/figure_parts/o18_curve.pdf",sep=""),
          width=7/5,height=7/4,family="ArialMT")
ggplot(data=Coryphodon2, aes(x=Level,y=O18))+
  geom_vline(xintercept=14.45, linetype=2)+
  geom_vline(xintercept=46, linetype=2)+ ylab(" ") +
  geom_ribbon(data=Coryphodon2,x=Coryphodon2$Level,
              (aes(ymin=Coryphodon2$CorX-sd(Coryphodon2$CorX,na.rm=T),
                   ymax=Coryphodon2$CorX+sd(Coryphodon2$CorX,na.rm=T))),fill="grey50",
              alpha=0.5) +
  geom_line(data=Coryphodon2,aes(x=CorY,y=CorX),alpha=1,
            color="grey50",size=2)+
  geom_point(color="grey50",alpha=1) +
  theme_classic() + coord_flip(xlim = c(0,75)) +
  theme(text=element_text(size=10,family="ArialMT"),
        axis.title.y=element_blank())
dev.off()
embedFonts(paste(datadir,"/figure_parts/o18_curve.pdf",sep=""))

cairo_pdf(paste(datadir,"/figure_parts/horse_curve.pdf",sep=""),
          width=7/5,height=7/4,family="ArialMT")
ggplot(data=Horses2, aes(x=Level,y=LogArea))+
  geom_vline(xintercept=14.45, linetype=2)+
  geom_vline(xintercept=46, linetype=2)+ ylab("ln(L x W)") +
  geom_ribbon(data=Horses2,x=Horses2$Level,
              (aes(ymin=Horses2$HorX-sd(Horses2$HorX,na.rm=T),
                   ymax=Horses2$HorX+sd(Horses2$HorX,na.rm=T))),fill="grey50",
              alpha=0.5) +
  geom_line(data=Horses2,aes(x=HorY,y=HorX),alpha=1,
            color="grey50",size=2)+
  geom_point(color="grey50",alpha=1) +
  theme_classic() + coord_flip(xlim = c(0,75)) +
  theme(text=element_text(size=10,family="ArialMT"),
        axis.title.y=element_blank())
dev.off()
embedFonts(paste(datadir,"/figure_parts/horse_curve.pdf",sep=""))


