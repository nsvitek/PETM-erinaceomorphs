# !diagnostics off
#above added to try to stop getting warnings message about unknown factors

# #Spine of code to analyze species of Wa-0 erinaceomorphs of Bighorn Basin
#Choose your own adventure. 
# taxon<-"Macrocranion"
# taxon<-"Talpavoides"
taxon<-"Colpocherus"

# source dependencies -----
#set file locations
scriptsdir <- "C://scripts"
# scriptsdir <- "C://cygwin/home/N.S/scripts/scripts"
datadir <- "D:/Dropbox/Documents/Dissertation/sys_eulipotyphla/lineage_data"
# datadir <- "C:/Users/N.S/Dropbox/Documents/Dissertation/sys_eulipotyphla/lineage_data"

source(paste(scriptsdir,"/PETM-erinaceomorphs/erinaceomorph_dependencies.R",sep=""))

# read shape data -----
if (taxon=="Macrocranion"){
  shapedir<-"macrocranion_p_10k_18-02-09_2048"
  shapedir2<-"macrocranion_pc_18-02-10_2048"
}
if (taxon=="Talpavoides"){shapedir<-"talpavoides_p_10k_18-04-02_2048"}
if (taxon=="Colpocherus"){shapedir<-"colpocherus_p_10k_18-03-05_2048"}

#read in pseudolandmark data
molars<-read.morphologika(paste(datadir,shapedir,
                                "morphologika_unscaled_high.txt",sep="/")) %>%
  preprocess(.)

CS<-molars$cs

# # crop rotate and calculate -----
# #this code only needs to be run once at the start of project analysis.
# freeze<-ls() #take a snapshop of objects in environment
# #create cropped surfaces for downstream analysis
# source(paste(scriptsdir,"/PETM-erinaceomorphs/erinaceomorph_recrop_ply.R",sep=""))
# rm(list = setdiff(ls(),freeze)) #clean up environment, also removes freeze
# #calculate dental metrics for downstream analysis
# source(paste(scriptsdir,"/PETM-erinaceomorphs/erinaceomorph_dental_calcs.R",sep=""))

# read data ----
#set repeat values. Number of specimens used, r, is in [taxon]_data.R
r_p<-3 #number of copies of each re-crop
r_c<-3 #number of re-crops
tooth<-c("M/2","M/2*")

if (taxon=="Macrocranion"){
  source(paste(scriptsdir,"/PETM-erinaceomorphs/macrocranion_data.R",sep=""))
}
if (taxon=="Talpavoides"){
  source(paste(scriptsdir,"/PETM-erinaceomorphs/talpavoides_data.R",sep=""))
}
if (taxon=="Colpocherus"){
  source(paste(scriptsdir,"/PETM-erinaceomorphs/colpocherus_data.R",sep=""))
}

#make datasets of unique specimen shapes
critter_id<-NULL
for (i in 1:r){critter_id<-c(critter_id,(r_p*r_c*i)-(r_p*r_c-1))}
critter_id<-c(critter_id,(r_p*r_c*r+1):nrow(metadata))

critters<-metadata[critter_id,]
shapes<-molars$m2d[critter_id,]

# # error -----
# freeze<-ls() #take a snapshop of objects in environment
# setwd(paste(datadir,"/output/results_error",sep="")) #move to folder for keeping error-related results
# source(paste(scriptsdir,"/PETM-erinaceomorphs/erinaceomorph_error.R",sep=""))
# #a high amount of error due to cropping. Address.
# rm(list = setdiff(ls(),freeze)) #clean up environment, also removes freeze
# setwd("../")

# # position PCA -----
# PCA<-prcomp(shapes,scale.=FALSE)
# 
# #explore distribution of tooth positions
# plot(PCA$x[,1:2],bg=wes_palette("Darjeeling2")[factor(critters$posthoc_pos)],
#      pch=21,cex=1.5)
# search_term<-which(critters$specimen_number=="CAB15-1513")
# text(PCA$x[search_term,1],PCA$x[search_term,2],critters$specimen_number[search_term])

# plot(PCA$x[,1:2],bg=wes_palette("Darjeeling2")[critters$posthoc_pos],
#      pch=21,cex=((critters$cs^2)*5)) #,xlim=c(0.045,0.09),ylim=c(-0.07,0.07)
# text(PCA$x[,1],PCA$x[,2],critters$specimen_number[])
# plot(PCA$x[,1],critters$cs,bg=wes_palette("Darjeeling2")[critters$posthoc_pos],
#      pch=21,cex=((critters$cs^2)*5)) #,xlim=c(0.045,0.09),ylim=c(-0.07,0.07)

# # with size ----
#
# molars.us<-read.morphologika(paste(datadir,shapedir,
#                                 "morphologika_unscaled_high.txt",sep="/"))[,,critter_id] %>%
#   two.d.array(.)
#
# PCA<-prcomp(molars.us,scale.=FALSE)
#
# #explore distribution of tooth positions
# plot(PCA$x[,1:2],bg=wes_palette("Darjeeling2")[critters$posthoc_pos],
#      pch=21,cex=1.5)
#
# plot(PCA$x[,1:2],bg=wes_palette("Darjeeling2")[critters$posthoc_pos],
#      pch=21,cex=((critters$cs^2)*5)) #,xlim=c(0.045,0.09),ylim=c(-0.07,0.07)
# text(PCA$x[,1],PCA$x[,2],critters$specimen_number[])

# # figure differences in tooth positions -------
# m1<-which(critters$posthoc_pos=="M/1")
# m2<-which(critters$posthoc_pos=="M/2")
#
# m1_PC<-PC2shape(PCA,m1,PCs,colMeans(shapes)) %>%
#   matrix(.,ncol=molars$m,nrow=molars$k,byrow=TRUE)
#
# m2_PC<-PC2shape(PCA,m2,PCs,colMeans(shapes)) %>%
#   matrix(.,ncol=molars$m,nrow=molars$k,byrow=TRUE)
#
# position_color<-shpdif(m1_PC,m2_PC,ramp,alter="none",outlier=FALSE)
#
# plot3d(m1_PC,col=position_color,size=10,axes=F,xlab="",ylab="",zlab="")
# open3d()
# plot3d(m2_PC,col=position_color,size=10,axes=F,xlab="",ylab="",zlab="")

# clean metadata -----
#based on error analysis, set number of repeatable PCs
if (taxon=="Macrocranion"){PCs<-c(1:4)}
if (taxon=="Talpavoides"){PCs<-c(1:3)}
if (taxon=="Colpocherus"){PCs<-c(1:3)}

#set bins, with both single and multiple PETM bin(s)
critters<-binsBHB(critters, critters$meter_level)
critters<-bins.forMS(critters)
critters<-binsMinimal(critters)

#simplify tooth position information
critters$posthoc_pos[which(critters$posthoc_pos=="M/1*")]<-"M/1"
critters$posthoc_pos[which(critters$posthoc_pos=="M/2*")]<-"M/2"

# subset data -------
# #make position datasets
# position<-"M/1"
position<-"M/2"
if(position=="M/1"){pos="M-1"}else if (position=="M/2"){pos="M-2"}

#create relevant data objects
md<-critters[which(critters$posthoc_pos==position),]
sh<-shapes[which(critters$posthoc_pos==position),]
PCAp<-prcomp(sh,scale.=FALSE)
an<-anderson(PCAp$sdev)

#set general framework for result file names
figure_out<-paste(datadir,"/output/",taxon,"_",pos,"_",sep="")

# plot 3D metrics ----------
#Centroid Size
plot_linear(dataset=md,variable="lncs",
            x_label="ln(Centroid Size)",colors=bin.colMS,
            o18curve=TRUE)
#one way to save for publication
# cairo_pdf(paste(figure_out,"lncs.pdf",sep=""),width=10,height=10,family="Franklin Gothic Book")
# dev.off()
# embedFonts(paste(figure_out,"lncs.pdf",sep=""))
#quick save, not for publication
ggsave(paste(figure_out,"lncs.jpg",sep=""))

#RFI
plot_linear(dataset=md,variable="RFI2",
            x_label="RFI",colors=bin.col,
            o18curve=TRUE)
ggsave(paste(figure_out,"RFI.jpg",sep=""))

#DNE
usnm<-md$specimen_number %>% grep("USNM.*",.,perl = TRUE)
if(length(usnm)==0){md2<-md} else {md2<-md[-usnm,]}

plot_linear(dataset=md2,variable="DNE",
            x_label="DNE",colors=bin.col,
            o18curve=TRUE)
ggsave(paste(figure_out,"DNE.jpg",sep=""))

# plot linear metrics -------
#Crown area
if(position == "M/1"){
  plot_linear(dataset=m1.measures,variable="lnlxw",
            x_label="ln(L x W)",colors=bin.col,
            o18curve=TRUE)
  ggsave(paste(figure_out,"lnlxw.jpg",sep=""))
}

if(position == "M/2"){
  plot_linear(dataset=c.data,variable="lnlxw",
              x_label="ln(L x W)",colors=bin.col,
              o18curve=TRUE)
  ggsave(paste(figure_out,"lnlxw.jpg",sep=""))
  
  for (i in 1:length(metrics)){
    p1<-plot_linear(dataset=c.data,variable=metrics[i],
                x_label=metrics[i],colors=bin.col,
                o18curve=TRUE)
    ggsave(paste(figure_out,metric_names[i],".jpg",sep=""),plot=p1)
  }
  warnings()
}

# # correlate area to centroid size ----
# if(position=="M/1"){
#   for(row in 1:nrow(m1.measures)){
#     if(is.na(m1.measures$Catalog.Number[row])){
#       m1.measures$Catalog.Number[row]<-m1.measures$Field.Number[row]
#     }
#   }
#   
#   md$specimen_number %in% m1.measures$Catalog.Number #check
#   md$lnlxw<-NA
#   for (row in 1:nrow(md)){
#     linkup<-which(m1.measures$Catalog.Number==md$specimen_number[row])
#     md$lnlxw[row]<-m1.measures$lnlxw[linkup]
#   }
#   
#   ggplot(data=md, aes(x=lnlxw,y=lncs))+
#     geom_smooth(method='lm',formula=(y~x),color="black")+
#     geom_point(aes(fill=bin2,shape=bin2),alpha=0.9,size=4) +
#     xlab("ln(Length x Width)") +
#     ylab("ln(Centroid Size)") +
#     ggtitle(taxon) +
#     scale_fill_manual(name="Bin",
#                       labels=levels(bins)[levels(bins) %in% unique(PCA.data$bin2)],
#                       values=bin.col[levels(bins) %in% unique(PCA.data$bin2)]) +
#     scale_shape_manual(name="Bin",
#                        labels=levels(bins)[levels(bins) %in% unique(PCA.data$bin2)],
#                        values=c(21,22,23,23,23,24,25)[levels(bins) %in% unique(PCA.data$bin2)])+
#     theme_classic() +
#     theme(text=element_text(size=20,family="Franklin Gothic Book"))
#   # cairo_pdf(paste(figure_out,"corr_CS-lnlxw.pdf",sep=""),width=10,height=10,family="Franklin Gothic Book")
#   # dev.off()
#   # embedFonts(paste(figure_out,"corr_CS-lnlxw.pdf",sep=""))
#   #quick save, not for publication
#   ggsave(paste(figure_out,"corr_CS-lnlxw.jpg",sep=""))
# }

# stats settings -----
replicates<-1000
minN<-1 #minimum number of samples necessary to compare a bin
#make combinations of bins
biostrat2<-levels(c.data$bin2) %>% combn(.,2)
biostrat3<-levels(c.data$binMS) %>% combn(.,2)
biostrat4<-levels(c.data$binMin) %>% combn(.,2)

if (taxon=="Macrocranion"){
  #variables to test
  factors1<-c("cant","lnlxw","heights","tri.w.l")
  factors2<-c("lncs","RFI2","DNE2")
  #remove unsuitable specimens: USNM538323.2 for DNE
  md.stat<-md
  md.stat$DNE[which(md$specimen_number=="USNM538323.2")]<-NA

}
if (taxon=="Talpavoides"){
  factors1<-c("lnlxw","tal.w.l","tri.h.l","ic.hh.l",
              "met.w.tri","ento.l.l","cant")
  factors2<-c("lncs","RFI2","DNE2")
  md.stat<-md
} 
if (taxon=="Colpocherus"){
  factors1<-c("lnlxw","ic.me.l","met.l.l","met.w.l")
  factors2<-c("lncs","RFI2","DNE2")
  #any outliers?
  #CAB14-0668, UF284224 in RFI, CAB14-1021 weirdly low DNE
  md.stat<-md
  # md.stat$RFI2[which(md$specimen_number=="CAB14-0668")]<-NA
  # md.stat$RFI2[which(md$specimen_number=="UF284224")]<-NA
  # md.stat$DNE[which(md$specimen_number=="CAB14-1021")]<-NA
}

# stats: fine ------
#test for pairwise differences in crown area by bin
test2_1<-matrix(NA,nrow=length(factors1),ncol=ncol(biostrat2))
rownames(test2_1)<-factors1
colnames(test2_1)<-paste(biostrat2[1,],"vs",biostrat2[2,])

for (rows in 1:nrow(test2_1)){
  for (zone in 1:ncol(test2_1)){
    if(with(c.data,eval(as.name(factors1[rows]))[which(bin2==biostrat2[1,zone])] %>% length) < minN|
       with(c.data,eval(as.name(factors1[rows]))[which(bin2==biostrat2[2,zone])] %>% length) < minN){
      test2_1[rows,zone]<-NA #if sample sizes are too small, skip
    } else {
      test2_1[rows,zone]<-with(c.data,bootstrap(eval(as.name(factors1[rows]))[which(bin2==biostrat2[1,zone])],
                                             eval(as.name(factors1[rows]))[which(bin2==biostrat2[2,zone])],
                                             metric="meandiff",replicates=replicates)$probability)
      #the "eval(as.name("string")) comes from stackoverflow. Don't completely understand.
    }
  }
}

test2_2<-matrix(NA,nrow=length(factors2),ncol=ncol(biostrat2))
rownames(test2_2)<-factors2
colnames(test2_2)<-paste(biostrat2[1,],"vs",biostrat2[2,])

for (rows in 1:nrow(test2_2)){
  for (zone in 1:ncol(test2_2)){
    if(with(md.stat,eval(as.name(factors2[rows]))[which(bin2==biostrat2[1,zone])] %>% length) < minN|
       with(md.stat,eval(as.name(factors2[rows]))[which(bin2==biostrat2[2,zone])] %>% length) < minN){
      test2_2[rows,zone]<-NA #if sample sizes are too small, skip
    } else {
      test2_2[rows,zone]<-with(md.stat,bootstrap(eval(as.name(factors2[rows]))[which(bin2==biostrat2[1,zone])],
                                               eval(as.name(factors2[rows]))[which(bin2==biostrat2[2,zone])],
                                               metric="meandiff",replicates=replicates)$probability)
      #the "eval(as.name("string")) comes from stackoverflow. Don't completely understand.
    }
  }
}

test2_3<-matrix(NA,nrow=1,ncol=ncol(biostrat2))
rownames(test2_3)<-"m1.lnlxw"
colnames(test2_3)<-paste(biostrat2[1,],"vs",biostrat2[2,])
for (zone in 1:ncol(test2_3)){
  if(with(m1.measures,eval(as.name("lnlxw"))[which(bin2==biostrat2[1,zone])] %>% length) < minN|
     with(m1.measures,eval(as.name("lnlxw"))[which(bin2==biostrat2[2,zone])] %>% length) < minN){
    test2_3[1,zone]<-NA #if sample sizes are too small, skip
  } else {
    test2_3[1,zone]<-with(m1.measures,bootstrap(eval(as.name("lnlxw"))[which(bin2==biostrat2[1,zone])],
                            eval(as.name("lnlxw"))[which(bin2==biostrat2[2,zone])],
                            metric="meandiff",replicates=replicates)$probability)
  }
}

test2m2<-rbind(test2_1,test2_2)
test2m1<-rbind(test2_2,test2_3)
# stats: medium ------
#test for pairwise differences in crown area by bin
test3_1<-matrix(NA,nrow=length(factors1),ncol=ncol(biostrat3))
rownames(test3_1)<-factors1
colnames(test3_1)<-paste(biostrat3[1,],"vs",biostrat3[2,])

for (rows in 1:nrow(test3_1)){
  for (zone in 1:ncol(test3_1)){
    if(with(c.data,eval(as.name(factors1[rows]))[which(binMS==biostrat3[1,zone])] %>% length) < minN|
       with(c.data,eval(as.name(factors1[rows]))[which(binMS==biostrat3[2,zone])] %>% length) < minN){
      test3_1[rows,zone]<-NA #if sample sizes are too small, skip
    } else {
      test3_1[rows,zone]<-with(c.data,bootstrap(eval(as.name(factors1[rows]))[which(binMS==biostrat3[1,zone])],
                                                eval(as.name(factors1[rows]))[which(binMS==biostrat3[2,zone])],
                                                metric="meandiff",replicates=replicates)$probability)
      #the "eval(as.name("string")) comes from stackoverflow. Don't completely understand.
    }
  }
}

test3_2<-matrix(NA,nrow=length(factors2),ncol=ncol(biostrat3))
rownames(test3_2)<-factors2
colnames(test3_2)<-paste(biostrat3[1,],"vs",biostrat3[2,])

for (rows in 1:nrow(test3_2)){
  for (zone in 1:ncol(test3_2)){
    if(with(md.stat,eval(as.name(factors2[rows]))[which(binMS==biostrat3[1,zone])] %>% length) < minN|
       with(md.stat,eval(as.name(factors2[rows]))[which(binMS==biostrat3[2,zone])] %>% length) < minN){
      test3_2[rows,zone]<-NA #if sample sizes are too small, skip
    } else {
      test3_2[rows,zone]<-with(md.stat,bootstrap(eval(as.name(factors2[rows]))[which(binMS==biostrat3[1,zone])],
                                            eval(as.name(factors2[rows]))[which(binMS==biostrat3[2,zone])],
                                            metric="meandiff",replicates=replicates)$probability)
      #the "eval(as.name("string")) comes from stackoverflow. Don't completely understand.
    }
  }
}

test3_3<-matrix(NA,nrow=1,ncol=ncol(biostrat3))
rownames(test3_3)<-"m1.lnlxw"
colnames(test3_3)<-paste(biostrat3[1,],"vs",biostrat3[2,])
for (zone in 1:ncol(test3_3)){
  if(with(m1.measures,eval(as.name("lnlxw"))[which(binMS==biostrat3[1,zone])] %>% length) < minN|
     with(m1.measures,eval(as.name("lnlxw"))[which(binMS==biostrat3[2,zone])] %>% length) < minN){
    test3_3[1,zone]<-NA #if sample sizes are too small, skip
  } else {
    test3_3[1,zone]<-with(m1.measures,bootstrap(eval(as.name("lnlxw"))[which(binMS==biostrat3[1,zone])],
                                                eval(as.name("lnlxw"))[which(binMS==biostrat3[2,zone])],
                                                metric="meandiff",replicates=replicates)$probability)
  }
}

test3m2<-rbind(test3_1,test3_2)
test3m1<-rbind(test3_2,test3_3)

# stats: coarse ------
#test for pairwise differences in crown area by bin
test4_1<-matrix(NA,nrow=length(factors1),ncol=ncol(biostrat4))
rownames(test4_1)<-factors1
colnames(test4_1)<-paste(biostrat4[1,],"vs",biostrat4[2,])

for (rows in 1:nrow(test4_1)){
  for (zone in 1:ncol(test4_1)){
    if(with(c.data,eval(as.name(factors1[rows]))[which(binMin==biostrat4[1,zone])] %>% length) < minN|
       with(c.data,eval(as.name(factors1[rows]))[which(binMin==biostrat4[2,zone])] %>% length) < minN){
      test4_1[rows,zone]<-NA #if sample sizes are too small, skip
    } else {
      test4_1[rows,zone]<-with(c.data,bootstrap(eval(as.name(factors1[rows]))[which(binMin==biostrat4[1,zone])],
                                                eval(as.name(factors1[rows]))[which(binMin==biostrat4[2,zone])],
                                                metric="meandiff",replicates=replicates)$probability)
      #the "eval(as.name("string")) comes from stackoverflow. Don't completely understand.
    }
  }
}

test4_2<-matrix(NA,nrow=length(factors2),ncol=ncol(biostrat4))
rownames(test4_2)<-factors2
colnames(test4_2)<-paste(biostrat4[1,],"vs",biostrat4[2,])

for (rows in 1:nrow(test4_2)){
  for (zone in 1:ncol(test4_2)){
    if(with(md.stat,eval(as.name(factors2[rows]))[which(binMin==biostrat4[1,zone])] %>% length) < minN|
       with(md.stat,eval(as.name(factors2[rows]))[which(binMin==biostrat4[2,zone])] %>% length) < minN){
      test4_2[rows,zone]<-NA #if sample sizes are too small, skip
    } else {
      test4_2[rows,zone]<-with(md.stat,bootstrap(eval(as.name(factors2[rows]))[which(binMin==biostrat4[1,zone])],
                                            eval(as.name(factors2[rows]))[which(binMin==biostrat4[2,zone])],
                                            metric="meandiff",replicates=replicates)$probability)
      #the "eval(as.name("string")) comes from stackoverflow. Don't completely understand.
    }
  }
}


test4_3<-matrix(NA,nrow=1,ncol=ncol(biostrat4))
rownames(test4_3)<-"m1.lnlxw"
colnames(test4_3)<-paste(biostrat4[1,],"vs",biostrat4[2,])
for (zone in 1:ncol(test4_3)){
  if(with(m1.measures,eval(as.name("lnlxw"))[which(binMin==biostrat4[1,zone])] %>% length) < minN|
     with(m1.measures,eval(as.name("lnlxw"))[which(binMin==biostrat4[2,zone])] %>% length) < minN){
    test4_3[1,zone]<-NA #if sample sizes are too small, skip
  } else {
    test4_3[1,zone]<-with(m1.measures,bootstrap(eval(as.name("lnlxw"))[which(binMin==biostrat4[1,zone])],
                                                eval(as.name("lnlxw"))[which(binMin==biostrat4[2,zone])],
                                                metric="meandiff",replicates=replicates)$probability)
  }
}

test4m2<-rbind(test4_1,test4_2)
test4m1<-rbind(test4_2,test4_3)

# write ----
#write test2,test3,test4 to csv files
if(position == "M/1"){
  write.csv(test4m1,paste(figure_out,"stats_coarse.csv",sep=""))
  write.csv(test3m1,paste(figure_out,"stats_medium.csv",sep=""))
  write.csv(test2m1,paste(figure_out,"stats_fine.csv",sep=""))
} else {
  write.csv(test4m2,paste(figure_out,"stats_coarse.csv",sep=""))
  write.csv(test3m2,paste(figure_out,"stats_medium.csv",sep=""))
  write.csv(test2m2,paste(figure_out,"stats_fine.csv",sep=""))
}

# correct within-PETM results ------
m_resultsNA<-matrix(NA,5,5) #empty matrix
rownames(m_resultsNA)<-colnames(m_resultsNA)<-c("pre-PETM","early PETM","mid-PETM","late PETM","post-PETM")

#No need for Colpocherus, only Wa-0 data
#Macrocranion: m1.lnlxw and m2.canting angle
#Talpavoides: m1.lnlxw, m2.canting angle, trigonid height, talonid with, ic.hh,
if(position == "M/1"){
  m1.lnlxw.m<-m_resultsNA #copy empty matrix
  m1.lnlxw.m[lower.tri(m1.lnlxw.m,diag=FALSE)]<-test3m1[which(rownames(test3m1)=="m1.lnlxw"),]
  m1.lnlxw.m[upper.tri(m1.lnlxw.m)]<-t(m1.lnlxw.m)[upper.tri(m1.lnlxw.m)]
  for (i in 1:ncol(m1.lnlxw.m)){
    m1.lnlxw.m[,i]<-p.adjust(m1.lnlxw.m[,i],method="BH")
  }
  write.csv(m1.lnlxw.m,paste(figure_out,"lnlxw_corrected.csv",sep=""))
}
if(position == "M/2"){ #both Macrocranion and Talpavoides need cant
  cant.m<-m_resultsNA #copy empty matrix
  cant.m[lower.tri(cant.m,diag=FALSE)]<-test3m2[which(rownames(test3m2)=="cant"),]
  cant.m[upper.tri(cant.m)]<-t(cant.m)[upper.tri(cant.m)]
  for (i in 1:ncol(cant.m)){
    cant.m[,i]<-p.adjust(cant.m[,i],method="BH")
  }
  write.csv(cant.m,paste(figure_out,"cant_corrected.csv",sep=""))
  #also need RFI, DNE
  RFI.m<-m_resultsNA #copy empty matrix
  RFI.m[lower.tri(RFI.m,diag=FALSE)]<-test3m2[which(rownames(test3m2)=="RFI2"),]
  RFI.m[upper.tri(RFI.m)]<-t(RFI.m)[upper.tri(RFI.m)]
  for (i in 1:ncol(RFI.m)){
    RFI.m[,i]<-p.adjust(RFI.m[,i],method="BH")
  }
  write.csv(RFI.m,paste(figure_out,"RFI_corrected.csv",sep=""))
  DNE.m<-m_resultsNA #copy empty matrix
  DNE.m[lower.tri(DNE.m,diag=FALSE)]<-test3m2[which(rownames(test3m2)=="DNE2"),]
  DNE.m[upper.tri(DNE.m)]<-t(DNE.m)[upper.tri(DNE.m)]
  for (i in 1:ncol(DNE.m)){
    DNE.m[,i]<-p.adjust(DNE.m[,i],method="BH")
  }
  write.csv(DNE.m,paste(figure_out,"DNE_corrected.csv",sep=""))
  if (taxon=="Talpavoides"){
    #interconid distance between hypoconulid and hypoconid
    ic.hh.l.m<-m_resultsNA #copy empty matrix
    ic.hh.l.m[lower.tri(ic.hh.l.m,diag=FALSE)]<-test3m2[which(rownames(test3m2)=="ic.hh.l"),]
    ic.hh.l.m[upper.tri(ic.hh.l.m)]<-t(ic.hh.l.m)[upper.tri(ic.hh.l.m)]
    for (i in 1:ncol(ic.hh.l.m)){
      ic.hh.l.m[,i]<-p.adjust(ic.hh.l.m[,i],method="BH")
    }
    write.csv(ic.hh.l.m,paste(figure_out,"ic-hh-l_corrected.csv",sep=""))
    #trigonid height
    tri.h.l.m<-m_resultsNA #copy empty matrix
    tri.h.l.m[lower.tri(tri.h.l.m,diag=FALSE)]<-test3m2[which(rownames(test3m2)=="tri.h.l"),]
    tri.h.l.m[upper.tri(tri.h.l.m)]<-t(tri.h.l.m)[upper.tri(tri.h.l.m)]
    for (i in 1:ncol(tri.h.l.m)){
      tri.h.l.m[,i]<-p.adjust(tri.h.l.m[,i],method="BH")
    }
    write.csv(tri.h.l.m,paste(figure_out,"tri-h-l_corrected.csv",sep=""))
    #talonid width
    tal.w.l.m<-m_resultsNA #copy empty matrix
    tal.w.l.m[lower.tri(tal.w.l.m,diag=FALSE)]<-test3m2[which(rownames(test3m2)=="tal.w.l"),]
    tal.w.l.m[upper.tri(tal.w.l.m)]<-t(tal.w.l.m)[upper.tri(tal.w.l.m)]
    for (i in 1:ncol(tal.w.l.m)){
      tal.w.l.m[,i]<-p.adjust(tal.w.l.m[,i],method="BH")
    }
    write.csv(tal.w.l.m,paste(figure_out,"tal-w-l_corrected.csv",sep=""))
  }
}
# correct minimum binning results -------
m_resultsNA<-matrix(NA,3,3) #empty matrix
rownames(m_resultsNA)<-colnames(m_resultsNA)<-c("pre-PETM","PETM","post-PETM")

#No need for Colpocherus, only Wa-0 data
#Macrocranion: m1.lnlxw and m2.canting angle
#Talpavoides: m1.lnlxw, m2.canting angle, trigonid height, talonid with, ic.hh,
if(position == "M/1"){
  m1.lnlxw.m<-m_resultsNA #copy empty matrix
  m1.lnlxw.m[lower.tri(m1.lnlxw.m,diag=FALSE)]<-test4m1[which(rownames(test4m1)=="m1.lnlxw"),]
  m1.lnlxw.m[upper.tri(m1.lnlxw.m)]<-t(m1.lnlxw.m)[upper.tri(m1.lnlxw.m)]
  for (i in 1:ncol(m1.lnlxw.m)){
    m1.lnlxw.m[,i]<-p.adjust(m1.lnlxw.m[,i],method="BH")
  }
  write.csv(m1.lnlxw.m,paste(figure_out,"lnlxw_corrected2.csv",sep=""))
}
if(position == "M/2"){ #both Macrocranion and Talpavoides need cant
  cant.m<-m_resultsNA #copy empty matrix
  cant.m[lower.tri(cant.m,diag=FALSE)]<-test4m2[which(rownames(test4m2)=="cant"),]
  cant.m[upper.tri(cant.m)]<-t(cant.m)[upper.tri(cant.m)]
  for (i in 1:ncol(cant.m)){
    cant.m[,i]<-p.adjust(cant.m[,i],method="BH")
  }
  write.csv(cant.m,paste(figure_out,"cant_corrected2.csv",sep=""))
  #also need RFI, DNE
  RFI.m<-m_resultsNA #copy empty matrix
  RFI.m[lower.tri(RFI.m,diag=FALSE)]<-test4m2[which(rownames(test4m2)=="RFI2"),]
  RFI.m[upper.tri(RFI.m)]<-t(RFI.m)[upper.tri(RFI.m)]
  for (i in 1:ncol(RFI.m)){
    RFI.m[,i]<-p.adjust(RFI.m[,i],method="BH")
  }
  write.csv(RFI.m,paste(figure_out,"RFI_corrected2.csv",sep=""))
  DNE.m<-m_resultsNA #copy empty matrix
  DNE.m[lower.tri(DNE.m,diag=FALSE)]<-test4m2[which(rownames(test4m2)=="DNE2"),]
  DNE.m[upper.tri(DNE.m)]<-t(DNE.m)[upper.tri(DNE.m)]
  for (i in 1:ncol(DNE.m)){
    DNE.m[,i]<-p.adjust(DNE.m[,i],method="BH")
  }
  write.csv(DNE.m,paste(figure_out,"DNE_corrected2.csv",sep=""))
  if (taxon=="Talpavoides"){
    #interconid distance between hypoconulid and hypoconid
    ic.hh.l.m<-m_resultsNA #copy empty matrix
    ic.hh.l.m[lower.tri(ic.hh.l.m,diag=FALSE)]<-test4m2[which(rownames(test4m2)=="ic.hh.l"),]
    ic.hh.l.m[upper.tri(ic.hh.l.m)]<-t(ic.hh.l.m)[upper.tri(ic.hh.l.m)]
    for (i in 1:ncol(ic.hh.l.m)){
      ic.hh.l.m[,i]<-p.adjust(ic.hh.l.m[,i],method="BH")
    }
    write.csv(ic.hh.l.m,paste(figure_out,"ic-hh-l_corrected2.csv",sep=""))
    #trigonid height
    tri.h.l.m<-m_resultsNA #copy empty matrix
    tri.h.l.m[lower.tri(tri.h.l.m,diag=FALSE)]<-test4m2[which(rownames(test4m2)=="tri.h.l"),]
    tri.h.l.m[upper.tri(tri.h.l.m)]<-t(tri.h.l.m)[upper.tri(tri.h.l.m)]
    for (i in 1:ncol(tri.h.l.m)){
      tri.h.l.m[,i]<-p.adjust(tri.h.l.m[,i],method="BH")
    }
    write.csv(tri.h.l.m,paste(figure_out,"tri-h-l_corrected2.csv",sep=""))
    #talonid width
    tal.w.l.m<-m_resultsNA #copy empty matrix
    tal.w.l.m[lower.tri(tal.w.l.m,diag=FALSE)]<-test4m2[which(rownames(test4m2)=="tal.w.l"),]
    tal.w.l.m[upper.tri(tal.w.l.m)]<-t(tal.w.l.m)[upper.tri(tal.w.l.m)]
    for (i in 1:ncol(tal.w.l.m)){
      tal.w.l.m[,i]<-p.adjust(tal.w.l.m[,i],method="BH")
    }
    write.csv(tal.w.l.m,paste(figure_out,"tal-w-l_corrected2.csv",sep=""))
  }
}

# PCA -----
PCA.data<-cbind(PCAp$x[,PCs],md)
ggplot(data=PCA.data, aes(x=PC1,y=PC2))+
  geom_point(aes(fill=bin2,shape=bin2),alpha=0.9,size=4) +
  xlab(paste("PC 1 (",an$percent[1],"%)",sep="")) +
  ylab(paste("PC 2 (",an$percent[2],"%)",sep="")) +
  ggtitle(taxon) +
  scale_fill_manual(name="Bin",
                    labels=levels(bins)[levels(bins) %in% unique(PCA.data$bin2)],
                    values=bin.col[levels(bins) %in% unique(PCA.data$bin2)]) +
  scale_shape_manual(name="Bin",
                     labels=levels(bins)[levels(bins) %in% unique(PCA.data$bin2)],
                     values=c(21,22,23,23,23,24,25)[levels(bins) %in% unique(PCA.data$bin2)])+
  theme_classic() +
  theme(text=element_text(size=20,family="Franklin Gothic Book"))

# plot(PCAp$x[,1:2],bg=bin.col[md$bin2],
#      pch=21,cex=1.5) #,xlim=c(0.045,0.09),ylim=c(-0.07,0.07)
# text(PCAp$x[,1],PCAp$x[,2],md$specimen_number[])

# cairo_pdf(paste(figure_out,"PCA.pdf",sep=""),width=10,height=10,family="Franklin Gothic Book")
# dev.off()
# embedFonts(paste(figure_out,"PCA.pdf",sep=""))
#quick save, not for publication
ggsave(paste(figure_out,"PCA.jpg",sep=""))

# describe PC shapes -------
#plot differences for repeatable PCs
rainbows1<-PCheat2(PCAp,sh,pc=1,palette=ramp,alter="square",outlier=TRUE)
open3d() #plot PC max
plot3d(rainbows1[[1]]$pc1$max,axes=F,col=rainbows1[[2]],size=10,xlab="",ylab="",zlab="")
writePLY(paste(taxon,"_",pos,"_PC1_max.ply",sep=""),format="ascii",pointRadius=0.005)
rgl.close()
  
open3d() #plot PC min
plot3d(rainbows1[[1]]$pc1$min,axes=F,col=rainbows1[[2]],size=10,xlab="",ylab="",zlab="")
writePLY(paste(taxon,"_",pos,"_PC1_min.ply",sep=""),format="ascii",pointRadius=0.005)
rgl.close()

# describe mean shapes -------
#use repeatable PCs only to describe differences between groups
# CIE1<-which(md$binMin=="CIE")
# CIE2<-which(md$binMin=="post-CIE")
if (taxon=="Talpavoides"){ #the only one in Paleocene
  cf3<-which(md$binMS=="Cf-3") #find relevant subset of specimens
  #predict shape from mean PC vals
  cf3_PC<-PC2shape(PCAp,cf3,c(PCs),colMeans(shapes)) %>% 
    matrix(.,ncol=molars$m,nrow=molars$k,byrow=TRUE) 
  wa1<-which(md$binMS=="Wa-1")
  wa1_PC<-PC2shape(PCAp,wa1,c(PCs),colMeans(shapes)) %>% 
    matrix(.,ncol=molars$m,nrow=molars$k,byrow=TRUE)
  
  #calculate heat map to paint shape
  position_colorc1<-shpdif(cf3_PC,wa1_PC,ramp,alter="square",outlier=TRUE)
  
  # #plot Cf-3 vs Wa-1 comparison
  # open3d()
  # plot3d(wa1_PC,col=position_colorc1,size=10,axes=F,xlab="",ylab="",zlab="")
  # writePLY(paste(taxon,"_",pos,"_wa1_32colors.ply",sep=""),format="ascii",pointRadius=0.005)
  # rgl.close()
  # 
  # open3d()
  # plot3d(cf3_PC,col=position_colorc1,size=10,axes=F,xlab="",ylab="",zlab="")
  # writePLY(paste(taxon,"_",pos,"_cf3_32colors.ply",sep=""),format="ascii",pointRadius=0.005)
  # rgl.close()
}

if (taxon!="Colpocherus"){
  #do same as for Cf-3 above, but for combinations of the first 3 Eocene biozones
  wa0<-which(md$binMS=="Wa-0")
  wa1<-which(md$binMS=="Wa-1")
  wa2<-which(md$binMS=="Wa-2")
  wa0_PC<-PC2shape(PCAp,wa0,c(PCs),colMeans(shapes)) %>% 
    matrix(.,ncol=molars$m,nrow=molars$k,byrow=TRUE)
  wa1_PC<-PC2shape(PCAp,wa1,c(PCs),colMeans(shapes)) %>% 
    matrix(.,ncol=molars$m,nrow=molars$k,byrow=TRUE)
  wa2_PC<-PC2shape(PCAp,wa2,c(PCs),colMeans(shapes)) %>% 
    matrix(.,ncol=molars$m,nrow=molars$k,byrow=TRUE)
  #tried with outlier=FALSE, in Macrocranion outliers associated with weird spikes
  position_color01<-shpdif(wa0_PC,wa1_PC,ramp,alter="square",outlier=TRUE)
  position_color02<-shpdif(wa0_PC,wa2_PC,ramp,alter="square",outlier=TRUE)
  position_color12<-shpdif(wa1_PC,wa2_PC,ramp,alter="square",outlier=TRUE)

  # #Wa-0 vs Wa-1 comparison
  # open3d() #plot 
  # plot3d(wa0_PC,col=position_color01,size=10,axes=F,xlab="",ylab="",zlab="")
  # writePLY(paste(figure_out,"wa0_01colors.ply",sep=""),format="ascii",pointRadius=0.005)
  # rgl.close()
  # 
  # open3d() #plot 
  # plot3d(wa1_PC,col=position_color01,size=10,axes=F,xlab="",ylab="",zlab="")
  # writePLY(paste(figure_out,"wa1_01colors.ply",sep=""),format="ascii",pointRadius=0.005)
  # rgl.close()
  # 
  # #Wa-0 vs Wa-2 comparison
  # open3d
  # plot3d(wa0_PC,col=position_color02,size=10,axes=F,xlab="",ylab="",zlab="")
  # writePLY(paste(figure_out,"wa0_02colors.ply",sep=""),format="ascii",pointRadius=0.005)
  # rgl.close()
  # 
  # open3d
  # plot3d(wa2_PC,col=position_color02,size=10,axes=F,xlab="",ylab="",zlab="")
  # writePLY(paste(figure_out,"wa2_02colors.ply",sep=""),format="ascii",pointRadius=0.005)
  # rgl.close()
  # 
  # #Wa-2 vs Wa-1 comparison
  # open3d()
  # plot3d(wa1_PC,col=position_color12,size=10,axes=F,xlab="",ylab="",zlab="")
  # writePLY(paste(figure_out,"wa1_12colors.ply",sep=""),format="ascii",pointRadius=0.005)
  # rgl.close()
  # 
  # open3d()
  # plot3d(wa2_PC,col=position_color12,size=10,axes=F,xlab="",ylab="",zlab="")
  # writePLY(paste(figure_out,"wa2_12colors.ply",sep=""),format="ascii",pointRadius=0.005)
  # rgl.close()

}

if (taxon=="Talpavoides"){ #the only one in Paleocene
  position_color03<-shpdif(wa0_PC,cf3_PC,ramp,alter="square",outlier=TRUE)

  open3d() #plot
  plot3d(wa0_PC,col=position_color03,size=10,axes=F,xlab="",ylab="",zlab="")
  writePLY(paste(figure_out,"wa0_03colors.ply",sep=""),format="ascii",pointRadius=0.005)
  rgl.close()
  
  open3d() #plot
  plot3d(cf3_PC,col=position_color03,size=10,axes=F,xlab="",ylab="",zlab="")
  writePLY(paste(figure_out,"cf3_03colors.ply",sep=""),format="ascii",pointRadius=0.005)
  rgl.close()
}

#Shape differences within PETM
#Do with Macrocranion and Colpocherus, but only M/1 of Talpavoides
wa0e<-which(md$bin2=="Wa-0 1")
wa0m<-which(md$bin2=="Wa-0 2")
  
wa0e_PC<-PC2shape(PCAp,wa0e,c(PCs),colMeans(shapes)) %>% 
  matrix(.,ncol=molars$m,nrow=molars$k,byrow=TRUE)
wa0m_PC<-PC2shape(PCAp,wa0m,c(PCs),colMeans(shapes)) %>% 
  matrix(.,ncol=molars$m,nrow=molars$k,byrow=TRUE)
  
position_colorem<-shpdif(wa0e_PC,wa0m_PC,ramp,alter="square",outlier=TRUE)

open3d() #plot 
plot3d(wa0e_PC,col=position_colorem,size=10,axes=F,xlab="",ylab="",zlab="")
writePLY(paste(taxon,"_",pos,"_wa0e_emcolors.ply",sep=""),format="ascii",pointRadius=0.005)
rgl.close()
  
open3d() #plot 
plot3d(wa0m_PC,col=position_colorem,size=10,axes=F,xlab="",ylab="",zlab="")
writePLY(paste(taxon,"_",pos,"_wa0m_emcolors.ply",sep=""),format="ascii",pointRadius=0.005)
  rgl.close()

# stats for PLM shape -----
testgdf<-geomorph.data.frame(coords=PCAp$x[,PCs],bin2=md$bin2,binMS=md$binMS,binMin=md$binMin)
if (taxon!="Colpocherus"){
  testbinMin<-advanced.procD.lm(coords~1,~binMin,data=testgdf,group=~binMin,iter=999,RRPP=TRUE)
  testbinMS<-advanced.procD.lm(coords~1,~binMS,data=testgdf,group=~binMS,iter=999,RRPP=TRUE)

  write.csv(testbinMS$P.means.dist,paste(figure_out,"_ProcANOVA_biozone.csv",sep=""))
  write.csv(testbinMin$P.means.dist,paste(figure_out,"_ProcANOVA_CIE.csv",sep=""))
}

testbin2<-advanced.procD.lm(coords~1,~bin2,data=testgdf,group=~bin2,iter=999,RRPP=TRUE)
write.csv(testbin2$P.means.dist,paste(figure_out,"_ProcANOVA_inPETM.csv",sep=""))