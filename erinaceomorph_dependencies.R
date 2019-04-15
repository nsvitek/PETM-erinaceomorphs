# load packages
# install.packages("readxl")
library(tidyverse)
library(readxl)
library(ggplot2)
library(jpeg) #for background curve
library(grid) #for background curve
library(wesanderson) #for colors
library(geomorph) #reading, formatting, analyzing morphometric data
library(Rvcg) #importing/exporting 3d files
library(molaR) #not just dental calcs, but also cSize
library(scales) #for rescale for colors, not cutting anymore

# source local functions
setwd(scriptsdir)
source("observer-free-morphotype-characterization/rearrange3ddat.R")
source("observer-free-morphotype-characterization/find_repeatablePCs.R")
source("observer-free-morphotype-characterization/figpiece3d.R")
source("observer-free-morphotype-characterization/anderson.R")
source("PETM-erinaceomorphs/binsBHB.R")
# source("plyPlaneClip.R") #rough copy of unknown origin, replaced by plyClip_Update
source("scripts/plyClip_Update.R") #temporary update from P.E.Morse, should be able to use molaR soon
source("scripts/calculate_error.R")
setwd("scripts")
source("Temperature_and_herbivore_curve_data/PETMData.R")
setwd("..")
source("scripts/function_bootstrap.R")
source("scripts/rotate3dShape.R")

# colors ------
#set colors. Checked for colorblind compatability
bin.col<-c("#88CCEE","white","#DDCC77","#CC6677","#882255","#999933","#117733")
bins<-c("Cf-3", "Wa-M", "Wa-0 1", "Wa-0 2","Wa-0 3","Wa-1","Wa-2") %>% factor(.,levels=.)
bin.colMS<-c("#88CCEE","#DDCC77","#CC6677","#882255","#117733")
binsMS<-c("pre-PETM","early PETM","mid-PETM","late PETM","post-PETM") %>% factor(.,levels=.)

ramp<-colorRampPalette(c("blue","green","yellow","red")) #set gradient color scheme
if(exists("ncut")==FALSE){ncut<-256}
raintable<-ramp(ncut)

# fonts ------
#settings to get non-Helvetica fonts to work with ggplot 
library(extrafont)
# Adjust the path to match your installation of Ghostscript
Sys.setenv(R_GSCMD = "C:/Program Files (x86)/gs/gs9.26/bin/gswin32c.exe")

# ms-specific bins -------
bins.forMS<-function(dataframe){
  dataframe$binMS<-NA
  dataframe$binMS[which(dataframe$meter_level<5)]<-"pre-PETM"
  dataframe$binMS[which(dataframe$meter_level>5&dataframe$meter_level<25)]<-"early PETM"
  dataframe$binMS[which(dataframe$meter_level>25&dataframe$meter_level<41)]<-"mid-PETM"
  dataframe$binMS[which(dataframe$meter_level>41&dataframe$meter_level<46)]<-"late PETM"
  dataframe$binMS[which(dataframe$meter_level>46)]<-"post-PETM"
  dataframe$binMS<-factor(dataframe$binMS,levels=binsMS)
  return(dataframe)
}

binmeanMS<-c(mean(c(0.1,5)),mean(c(14.45,25)),
           mean(c(25,41)),mean(c(41,46)),mean(c(46,75)))

binsMin<-c("pre-PETM","PETM","post-PETM")
binmeanMin<-c(mean(c(0.1,5)),mean(c(14.45,46)),mean(c(46,75)))
binsMinimal<-function(dataframe){
  dataframe$binMin<-NA
  dataframe$binMin[which(dataframe$meter_level<5)]<-"pre-PETM"
  dataframe$binMin[which(dataframe$meter_level>5&dataframe$meter_level<46)]<-"PETM"
  dataframe$binMin[which(dataframe$meter_level>46)]<-"post-PETM"
  dataframe$binMin<-factor(dataframe$binMin,levels=binsMin)
  return(dataframe)
}

#plot functions --------
#define function for linear plot aesthetics
plot_linear<-function(dataset,variable,x_label,colors=bin.colMS,o18curve=TRUE){
  if(o18curve==TRUE){
    background<-readJPEG(paste(datadir,"../O18curve.jpg",sep="/"))
    ggplot(data=dataset, aes_string(y="meter_level",x=variable,group="meter_level"))+
      annotation_custom(rasterGrob(background,
                                   width = unit(1,"npc"), height = unit(1,"npc")), -Inf, Inf, -Inf, Inf)+
      geom_hline(yintercept=14.45, linetype=2)+
      geom_hline(yintercept=46, linetype=2)+
      # geom_boxplot(alpha=1) +
      geom_point(aes(fill=binMS,shape=binMS),alpha=0.75,size=4) +
      scale_fill_manual(name="Bin", 
                        labels=levels(binsMS)[levels(binsMS) %in% unique(dataset$binMS)],
                        values=colors[levels(binsMS) %in% unique(dataset$binMS)]) +
      ggtitle(taxon) + xlab(x_label) + ylab("Meter Level") +
      scale_shape_manual(name="Bin",
                         labels=levels(binsMS)[levels(binsMS) %in% unique(dataset$binMS)],
                         values=c(23,21,21,21,22)[levels(binsMS) %in% unique(dataset$binMS)])+
      theme(text=element_text(size=20,family="Franklin Gothic Book")) + 
      coord_cartesian(ylim = c(0,75))
  }
  else {
    ggplot(data=dataset, aes_string(y="meter_level",x=variable,group="meter_level"))+
      geom_hline(yintercept=14.45, linetype=2)+
      geom_hline(yintercept=46, linetype=2)+
      # geom_boxplot(alpha=1) +
      geom_point(aes(fill=binMS,shape=binMS),alpha=0.75,size=4) +
      scale_fill_manual(name="Bin", 
                        labels=levels(binsMS)[levels(binsMS) %in% unique(dataset$binMS)],
                        values=colors[levels(binsMS) %in% unique(dataset$binMS)]) +
      ggtitle(taxon) + xlab(x_label) + ylab("Meter Level") +
      scale_shape_manual(name="Bin",
                         labels=levels(binsMS)[levels(binsMS) %in% unique(dataset$binMS)],
                         values=c(23,21,21,21,22)[levels(binsMS) %in% unique(dataset$binMS)])+
      theme(text=element_text(size=20,family="Franklin Gothic Book")) + 
      coord_cartesian(ylim = c(0,75))
    }
}

#change: scaled is the $m2d input, not the whole package. No choice

PCheat2<-function(data,scaled,pc=1,palette,alter="none",outlier=FALSE,...){
  meanshape<-mshp(scaled)
  pc.differences<-pcdif(data,meanshape,pcs=pc)
  heatcolor.index<-shpdif(pc.differences[[1]]$min,pc.differences[[1]]$max,palette,alter=alter,outlier=outlier)
  pieces2plot<-list(pc.differences,heatcolor.index)
  return(pieces2plot)
}

# a function to predict shape differences from PC scores
#in function, mean shape is a kxm vector (colSums of 2d shape matrix)
PC2shape<-function(PC_analysis,sample_subset,PCs,mean_shape,...){
  pctrans<-matrix(nrow=length(PCs),ncol=length(mean_shape)) #make a blank matrix
  for (i in PCs){
    pcval<-mean(PC_analysis$x[sample_subset,i])
    pctrans[i,]<-pcval*PC_analysis$rotation[,i]
  }
  pcpredict<-colSums(pctrans)+mean_shape
  return(pcpredict)
}

