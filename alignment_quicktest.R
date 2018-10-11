#Use simulated dataset to identify what is going on with different sources of error

taxon<-"Macrocranion"

# source dependencies -----
scriptsdir <- "C://cygwin/home/N.S/scripts"
source(paste(scriptsdir,"/BHB-erinaceomorph/erinaceomorph_dependencies.R",sep=""))

# settings -----
r<-6 #number of replicates per specimen
shapedir1<-"TwoBumpSphere_1024" #replicates are identical file copies
shapedir2<-"TwoBumpSphere_rot_1024" #replicates rotated 60 degrees from previous replicate
shapedir3<-"TwoBumpSphere_holes_1024" #replicates have 2 triangles removed each

molars<-read.morphologika(paste(datadir,shapedir2,
         "morphologika_unscaled_high.txt",sep="/")) %>% 
  preprocess(.)

# statistics ----
#PCA of total dataset
PCA_er<-prcomp(molars$m2d[,],scale.=FALSE)
# dimnames(molars$scaled)[[3]]
error_labels<-c(rep(1,6),2,3,4,5,6,7,rep(8,6)) %>% factor #for noholes dataset

testgdf<-geomorph.data.frame(coords=PCA_er$x[,],specimen=error_labels)
errorANOVA<-procD.lm(coords~error_labels,data=testgdf,iter=999,RRPP=TRUE) %>% 
  .$aov.table
err<-error3d(errorANOVA,r)

plot(PCA_er$x[,1:2],bg=c("black","purple","blue","steelblue","green",
                         "yellow","orange","red")[error_labels],pch=21,cex=2)
title(paste("Repeatability =", round(err$repeatability,3),
            "\nMeasurement Error =", round(err$PME,3),"%"))

#look at number of repeatable PCs
repeatPCs_result<-find_repeatablePCs(PCA_er$x,error_labels,rep=r)

# map variance ------
#map variance of repeated shapes
ramp<-colorRampPalette(c("blue","green","yellow","red")) #set gradient color scheme
if(exists("ncut")==FALSE){ncut<-256}
raintable<-ramp(ncut)

newshapes<-molars$scaled[,,c(13:18)]
plot3d(newshapes[,,1],axes=F) #check orientation
plot3d(newshapes[,,2],axes=F) #check orientation
plot3d(newshapes[,,3],axes=F) #check orientation

meanshape<-apply(newshapes,1:2,mean) #calculate mean shape
variances<-apply(newshapes,1:2,var) %>% apply(.,1,mean) #find mean variance for all points, unrotated
colbydiff<-rescale(variances,to=c(1,ncut)) %>% round #rescale to fit variance to a color value in gradient
result<-raintable[colbydiff] #match variance to a color
plot3d(meanshape,axes=F,col=result,size=10) #plot variance map
