# check cropping rotation -----
#figure out rotation desired for cropping along Z plane

if (taxon=="Macrocranion"){
  newshapes<-rotateMorphologika(molars$scaled,degreesX=-70,degreesY=115,degreesZ=0)
  zval<--0.004
}
if (taxon=="Talpavoides"){
  newshapes<-rotateMorphologika(molars$scaled,degreesX=125,degreesY=-40,degreesZ=30)
  zval2<--0.0046
  zval1<--0.0063
}
if (taxon=="Colpocherus"){
  newshapes<-rotateMorphologika(molars$scaled,degreesX=-120,degreesY=15,degreesZ=0)
  zval2<--0.0063
  zval1<--0.0075
}

#test rotation angles
rotateMorphologika(molars$scaled,degreesX=-120,degreesY=15,degreesZ=0)[,,1] %>% 
  plot3d()
plot3d(newshapes[,,1]) #check orientation
max(newshapes[,3,]) #check to make sure maximum value matches expectations
#max doesn't work because of difference in orientations even in aligned shapes in Talpavoides

# Specimen <- vcgPlyRead(lsdr[28])
# Specimen <- molaR_Clean(Specimen)
# 
# points3d(t(Specimen$vb))
# PointNames <- c(as.character(1:ncol(Specimen$vb)))
# text3d(t(Specimen$vb), text=PointNames) #3538


# cut ply files --------
folder<-paste(datadir,shapedir,"aligned_4crop",sep="/") # make a path to folder of aligned surfaces
setwd(folder) #move to that folder
lsdr<-list.files(path = ".", full.names = TRUE, recursive = FALSE) #make a list of the files in the folder

#load first surface to serve as a reference
surface.reference<-vcgPlyRead(lsdr[1]) #sample M/2, talp
# surface.reference<-vcgPlyRead(lsdr[28]) #sample M/1, talp
surface.reference.max<-surface.reference$vb[3,] %>% max #vb are coordinates for vertices, so 3rd row is Z coord?


#plot first surface and test clip to find desired plane
# plot3d(surface.reference)
if(taxon=="Talpavoides"){zval<-zval2} #try out both z-vals
zrange<-c(zval-0.0001,zval+0.0001)
pick.vertex<- which(surface.reference$vb[3,]>=zrange[1]&surface.reference$vb[3,]<=zrange[2])[1]
plyClip_Update(surface.reference,axis="Z",vertIndex = pick.vertex, 
        edgeRefine=FALSE) #determine vertex index from looking

# plyClip_Update(vcgPlyRead(lsdr[3]),axis="Z",vertIndex = pick.vertex,
#                edgeRefine=FALSE) #determine vertex index from looking

# translate and cut each specimen
for (i in 1:length(lsdr)){
  surface.old<-vcgPlyRead(lsdr[i])
  surface.old.max<-surface.old$vb[3,] %>% max
  surface.difference<-surface.reference.max-surface.old.max
  surface.move<-surface.old
  surface.move$vb[3,]<-surface.old$vb[3,] +surface.difference
  if(taxon=="Talpavoides"|taxon=="Colpocherus"){
    if (metadata$posthoc_pos[i]=="M/1"|metadata$posthoc_pos[i]=="M/1*"){
      zval<-zval1
    } #choose position-specific z-value
    if (metadata$posthoc_pos[i]=="M/2"|metadata$posthoc_pos[i]=="M/2*"){
      zval<-zval2
    } #choose position-specific z-value
  }
  zrange<-c(zval-0.0001,zval+0.0001)
  pick.vertex<- which(surface.move$vb[3,]>=zrange[1]&surface.move$vb[3,]<=zrange[2])[1]
    surface.clip<-plyClip_Update(surface.move,axis="Z",vertIndex = pick.vertex, 
                 edgeRefine=FALSE) #determine vertex index from looking
  vcgPlyWrite(surface.clip, format="ascii",
              filename=paste(datadir,"/",shapedir,"/aligned_cropped/",
                substr(lsdr[i],start=1,stop=(nchar(lsdr[i])-4)),sep="")) #save clipped surface
  print(paste("Clipped ",i,"of",length(lsdr)))
}
