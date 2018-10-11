# #Evaluate two kinds of error: cropping & pseudolandmark placement (PLMP)
# general approach: subset the data to measure evaluate only replicated specimens
# calculate values from those subset

# create labels ---------
reps<-c(1:(r_p*r_c*r))
label_error<-dimnames(molars$scaled)[[3]][reps] %>% strsplit(.,split="[_-]")
#create variables based on file names
specimen<-crop<-replicate<-NULL
for (i in 1:length(reps)){
  specimen[i]<-paste(label_error[[i]][1],label_error[[i]][2],sep="-")
  if (taxon=="Macrocranion"|taxon=="Colpocherus"){
    crop[i]<-label_error[[i]][5]
    replicate[i]<-label_error[[i]][6]
  }
  if (taxon=="Talpavoides"){
    crop[i]<-label_error[[i]][4]
    replicate[i]<-label_error[[i]][5]
  }
}

#replicate specimens groups in context of total dataset
label_repeats<-NULL #complicated way to avoid hard coding the factors. Something shorter?
for(i in 1:r){rep(i,r_p*r_c) %>% c(label_repeats,.) -> label_repeats} 
label_repeats<-c(label_repeats,rep(r+1,(nrow(metadata)-(r_p*r_c*r))))

#all specimen labels
label_all<-dimnames(molars$scaled)[[3]][] %>% substr(.,1,17) %>% factor

# shape error ----------
PCA_er_c<-prcomp(molars$m2d[reps,],scale.=FALSE)
testgdf<-geomorph.data.frame(coords=PCA_er_c$x[,],
           specimen=factor(specimen),crop=factor(crop),replicate=factor(replicate))
errorANOVA<-procD.lm(coords~specimen/crop/replicate,data=testgdf,iter=999,RRPP=TRUE) %>% .$aov.table
err_cr<-error3d(errorANOVA,r_p,f1=1,f2=2) #crop+replicate
err_r<-error3d(errorANOVA,r_p,f1=1,f2=3) #replicate

#replicates in context of total dataset
PCA_er<-prcomp(molars$m2d[,],scale.=FALSE) #PCA of all surfaces
testgdf2<-geomorph.data.frame(coords=PCA_er$x[,],specimen=label_all)
errorANOVA2<-procD.lm(coords~specimen,data=testgdf2,iter=999,RRPP=TRUE) %>% .$aov.table
mean_rep<-c(rep(1,length(which(label_repeats==(r+1)))),rep(r_p*r_c,r)) %>% mean
err_cr2<-error3d(errorANOVA2,mean_rep,f1=1,f2=2) #crop+replicate


# plot shape error -------
jpeg(filename=paste("error_crop_",taxon,"_pca_reps.jpg",sep=""))
plot(PCA_er_c$x[,1:2],bg=wes_palette("Darjeeling2")[factor(specimen)],pch=21,cex=1.5)
title(paste("Cropping Repeatability =", round(err_cr$repeatability,3),
            "\nPLM Repeatability =", round(err_r$repeatability,3)))
dev.off()

jpeg(filename=paste("error_crop_",taxon,"_pca_all.jpg",sep=""))
plot(PCA_er$x[,1:2],bg=wes_palette("Darjeeling2")[factor(label_repeats)],pch=21,cex=1.5)
title(paste("Cropping Repeatability =", round(err_cr2$repeatability,3)))
dev.off()

jpeg(filename=paste("error_crop_",taxon,"_by_pc.jpg",sep=""))
repeatPCs_result<-find_repeatablePCs(PCA_er$x,label_all,rep=mean_rep) #due to cropping error
title(paste("# repeatable PCs =", (min(which(round(repeatPCs_result,2)<0.9))-1)))
dev.off()

jpeg(filename=paste("error_PLM_",taxon,"_by_pc.jpg",sep=""))
repeatPCs_result<-find_repeatablePCs(PCA_er_c$x,paste(specimen,crop,sep="_"),rep=r_p) #due to PLM error
title(paste("# repeatable PCs =", max(which(repeatPCs_result>=0.9))))
dev.off()

# example shape error --------

#example shapes
newshapes1<-rotateMorphologika(molars$scaled[,,c(1,4,7)],degreesX=0,degreesY=0,degreesZ=0) #compare recrops
# newshapes<-rotateMorphologika(molars$scaled[,,c(10,13,16)],degreesX=0,degreesY=0,degreesZ=0) #compare recrops
newshapes2<-rotateMorphologika(molars$scaled[,,c(1:3)],degreesX=0,degreesY=0,degreesZ=0)
newshapes3<-rotateMorphologika(molars$scaled[,,c(4:6)],degreesX=0,degreesY=0,degreesZ=0)
# newshapes<-rotateMorphologika(molars$scaled[,,c(34:36)],degreesX=0,degreesY=0,degreesZ=0)

#plot first
meanshape<-apply(newshapes1,1:2,mean) #calculate mean shape
variances<-apply(newshapes1,1:2,var) %>% apply(.,1,mean) #find mean variance for all points, unrotated
colbydiff<-rescale(variances,to=c(1,ncut)) %>% round
result<-raintable[colbydiff]

open3d()
plot3d(meanshape,axes=F,col=result,size=10,xlab="",ylab="",zlab="") #some bright colors near cropping margin, but also at local minima
writePLY(paste("error_example_crop",taxon,".ply",sep=""),format="ascii",pointRadius=0.005)
rgl.close()

#plot second
meanshape<-apply(newshapes2,1:2,mean) #calculate mean shape
variances<-apply(newshapes2,1:2,var) %>% apply(.,1,mean) #find mean variance for all points, unrotated
colbydiff<-rescale(variances,to=c(1,ncut)) %>% round
result<-raintable[colbydiff]

open3d()
plot3d(meanshape,axes=F,col=result,size=10,xlab="",ylab="",zlab="") #some bright colors near cropping margin, but also at local minima
writePLY(paste("error_example_PLM1",taxon,".ply",sep=""),format="ascii",pointRadius=0.005)
rgl.close()

#plot third
meanshape<-apply(newshapes3,1:2,mean) #calculate mean shape
variances<-apply(newshapes3,1:2,var) %>% apply(.,1,mean) #find mean variance for all points, unrotated
colbydiff<-rescale(variances,to=c(1,ncut)) %>% round
result<-raintable[colbydiff]

open3d()
plot3d(meanshape,axes=F,col=result,size=10,xlab="",ylab="",zlab="") #some bright colors near cropping margin, but also at local minima
writePLY(paste("error_example_PLM2",taxon,".ply",sep=""),format="ascii",pointRadius=0.005)
rgl.close()

# centroid size error: cropping ----
aov.CS<-aov(cs~specimen,data=metadata[reps,]) %>% summary
err<-error2d(aov.CS,r)

jpeg(filename=paste("error_crop_",taxon,"_CS.jpg",sep=""))
boxplot(metadata$cs~label_repeats)
title(paste("Repeatability =", round(err$repeatability,3),
            "\nMeasurement Error =", round(err$PME,3),"%"))
dev.off()

# dental metric error: cropping ----
#given the significant amount of shape error due to cropping, check dental metrics
aov.RFI<-aov(RFI~specimen,data=metadata[reps,]) %>% summary
err<-error2d(aov.RFI,r)

#visualize
jpeg(filename=paste("error_crop_",taxon,"_RFI.jpg",sep=""))
boxplot(metadata$RFI~label_repeats)
title(paste("Repeatability =", round(err$repeatability,3),
            "\nMeasurement Error =", round(err$PME,3),"%"))
dev.off()

#repeat with DNE
aov.DNE<-aov(DNE~specimen,data=metadata[reps,]) %>% summary
boxplot(metadata$DNE~label_repeats) #check for outliers that need manual fixes
outlier<-which(metadata$DNE>=300) #one outlier. Weird. Look into this.
#USNM540402 is a problem

err<-error2d(aov.DNE,r)

jpeg(filename=paste("error_crop_",taxon,"_DNE.jpg",sep=""))
boxplot(metadata$DNE~label_repeats) #there are weird DNE outliers created for no clear reason
title(paste("Repeatability =", round(err$repeatability,3),
            "\nMeasurement Error =", round(err$PME,3),"%"))
dev.off()

#repeat with Slope
aov.slope<-aov(Slope~specimen,data=metadata[reps,]) %>% summary
err<-error2d(aov.slope,r)

jpeg(filename=paste("error_crop_",taxon,"_slope.jpg",sep=""))
boxplot(metadata$Slope~label_repeats)
title(paste("Repeatability =", round(err$repeatability,3),
            "\nMeasurement Error =", round(err$PME,3),"%"))
dev.off()


aov.slope<-aov(Slope2~specimen,data=metadata[reps,]) %>% summary
err<-error2d(aov.slope,r)

jpeg(filename=paste("error_crop_",taxon,"_slope2.jpg",sep=""))
boxplot(metadata$Slope2~label_repeats)
title(paste("Repeatability =", round(err$repeatability,3),
            "\nMeasurement Error =", round(err$PME,3),"%"))
dev.off()


#repeat with OPCr
aov.OPCR<-aov(OPCR~specimen,data=metadata[reps,]) %>% summary
err<-error2d(aov.OPCR,r)

jpeg(filename=paste("error_crop_",taxon,"_OPCR.jpg",sep=""))
boxplot(metadata$OPCR~label_repeats)
title(paste("Repeatability =", round(err$repeatability,3),
            "\nMeasurement Error =", round(err$PME,3),"%"))
dev.off()

metadata$specimen_number[which(metadata$OPCR==max(metadata$OPCR))] #USNM scans are weird

# dental metric error: cropping 2 -----
aov.RFI<-aov(RFI2~specimen,data=metadata[reps,]) %>% summary
err<-error2d(aov.RFI,r)

#visualize
jpeg(filename=paste("error_crop_",taxon,"_RFI2.jpg",sep=""))
boxplot(metadata$RFI2~label_repeats)
title(paste("Repeatability =", round(err$repeatability,3),
            "\nMeasurement Error =", round(err$PME,3),"%"))
dev.off()

# shape error: cropping 2 -----
if (taxon=="Macrocranion"){
  #PCA of total dataset
  molars<-read.morphologika(paste(datadir,shapedir2,
                                  "morphologika_unscaled_low.txt",sep="/")) %>% 
    preprocess(.)
  
  PCA_er<-prcomp(molars$m2d[,],scale.=FALSE)
  
  # calculate for a dataset of only replicated specimens
  PCA_er_c<-prcomp(molars$m2d[reps,],scale.=FALSE)

    #calculate total measurement error of dataset
  testgdf<-geomorph.data.frame(coords=PCA_er_c$x[,],
                               specimen=factor(specimen),crop=factor(crop),replicate=factor(replicate))
  errorANOVA<-procD.lm(coords~specimen/crop/replicate,data=testgdf,iter=999,RRPP=TRUE) %>% .$aov.table
  err_cr<-error3d(errorANOVA,r_p,f1=1,f2=2) #crop+replicate
  err_r<-error3d(errorANOVA,r_p,f1=1,f2=3) #replicate

  #replicates in context of total dataset
  PCA_er<-prcomp(molars$m2d[,],scale.=FALSE) #PCA of all surfaces
  
  jpeg(filename=paste("error_crop2_",taxon,"_pca_reps.jpg",sep=""))
  plot(PCA_er_c$x[,1:2],bg=wes_palette("Darjeeling2")[factor(specimen)],pch=21,cex=1.5)
  title(paste("Cropping Repeatability =", round(err_cr$repeatability,3),
              "\nPLM Repeatability =", round(err_r$repeatability,3)))
  dev.off()
  
  jpeg(filename=paste("error_crop2_",taxon,"_pca_all.jpg",sep=""))
  plot(PCA_er$x[,1:2],bg=wes_palette("Darjeeling2")[factor(label_repeats)],pch=21,cex=1.5)
  title(paste("Cropping Repeatability =", round(err_cr$repeatability,3),
              "\nPLM Repeatability =", round(err_r$repeatability,3)))
  dev.off()
  
  jpeg(filename=paste("error_crop2_",taxon,"_by_pc.jpg",sep=""))
  repeatPCs_result<-find_repeatablePCs(PCA_er_c$x,specimen,rep=r_p*r_c) #due to cropping error
  title(paste("# repeatable PCs =", max(which(repeatPCs_result>=0.9))))
  dev.off()
  
  jpeg(filename=paste("error_PLM2_",taxon,"_by_pc.jpg",sep=""))
  repeatPCs_result<-find_repeatablePCs(PCA_er_c$x,paste(specimen,crop,sep="_"),rep=r_p) #due to PLM error
  title(paste("# repeatable PCs =", max(which(repeatPCs_result>=0.9))))
  dev.off()
  
}
