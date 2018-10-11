# initial dental metrics ------
#these are the specimens rotated from "aligned" folder into life position using MeshLab
folder<-paste(datadir,shapedir,"aligned_life",sep="/") # make a path to folder of aligned surfaces
setwd(folder) #move to that folder
molaR_Batch(filename="../molaR_Batch_RFI_DNE_OPC.csv",RFI_alpha=0.10,OPCr=FALSE,DNE=TRUE,RFI=TRUE,Slope=FALSE)

# recalculate RFI -----
#these are the cropped specimens rotated from "aligned_cropped" folder into life position using MeshLab
folder<-paste(datadir,shapedir,"aligned_life2_cropped",sep="/") # make a path to folder of aligned surfaces
setwd(folder) #move to that folder
# lsdr<-list.files(path = ".", full.names = TRUE, recursive = FALSE) #make a list of the files in the folder
# vcgPlyRead(lsdr[1]) %>% plot3d() #check
molaR_Batch(filename="../molaR_Batch2_RFI_DNE_OPC.csv",RFI_alpha=0.10,OPCr=FALSE,DNE=TRUE,RFI=TRUE,Slope=FALSE)
setwd(datadir) #move back up

# individual checks ------
# #manual fixes, checks on individual specimens
#UF284221 is 64
lsdr<-list.files(path = ".", full.names = TRUE, recursive = FALSE) #make a list of the files in the folder
one<-vcgPlyRead(lsdr[44]) #%>% plot3d() #check
two<-RFI(one,alpha=0.1)
RFI3d(two)
# Check2D(two)

one<-vcgPlyRead(lsdr[37]) #%>% plot3d() #check
two<-Slope(one)
Slope3d(two)
# 
# ?DNE
one<-vcgPlyRead(lsdr[41]) #%>% plot3d() #check
two<-DNE(one,outlier=0.1)
DNE3d(two)

#Colpocherus CAB16-1273 is behaving strangely for DNE on uncropped specimen. 
fortytwo<-vcgPlyRead(lsdr[42]) #%>% plot3d() #check
fortytwo.dne<-DNE(fortytwo,outlier=0.4)
DNE3d(fortytwo.dne) #,setRange=c(0,0.49)
# fortytwo.dne$Surface_DNE

# #28 vs 38 vs 39 vs 35 in terms of differences in M/1 DNE for Talpavoides
# (wa1 vs wa2 vs wa0 vs cf3, unworn)
twentyeight<-vcgPlyRead(lsdr[28]) #%>% plot3d() #check
twentyeight.dne<-DNE(twentyeight,outlier=0.1)
DNE3d(twentyeight.dne,setRange=c(0,0.49))
writePLY(paste(substr(lsdr[28],3,12),"_DNE.ply",sep=""),format="ascii",pointRadius=0.005)

thirtyeight<-vcgPlyRead(lsdr[38]) #%>% plot3d() #check
thirtyeight.dne<-DNE(thirtyeight,outlier=0.1)
DNE3d(thirtyeight.dne,setRange=c(0,0.49))
writePLY(paste(substr(lsdr[38],3,12),"_DNE.ply",sep=""),format="ascii",pointRadius=0.005)

thirtynine<-vcgPlyRead(lsdr[39]) #%>% plot3d() #check
thirtynine.dne<-DNE(thirtynine,outlier=0.1)
DNE3d(thirtynine.dne,setRange=c(0,0.49))
writePLY(paste(substr(lsdr[39],3,12),"_DNE.ply",sep=""),format="ascii",pointRadius=0.005)

thirtyfive<-vcgPlyRead(lsdr[35]) #%>% plot3d() #checks
thirtyfive.dne<-DNE(thirtyfive,outlier=0.1)
DNE3d(thirtyfive.dne,setRange=c(0,0.49))
writePLY(paste(substr(lsdr[35],3,12),"_DNE.ply",sep=""),format="ascii",pointRadius=0.005)
getwd()

#manual fixes for Talpavoides: RFI on 44; 29; 10; 11; 12;
#round 2: 13,14,15,16,17,18,30,32,40,42,44,
