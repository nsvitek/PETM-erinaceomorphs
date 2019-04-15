# read linear data ------
source(paste(scriptsdir,"/PETM-erinaceomorphs/erinaceomorph_load_linear.R",sep=""))
warnings()

# #subset linear measurements relevant to taxon
metrics<-c("tri.w/length","tal.w/length","met.w.l","met.w/tri.w",
           "ic.me.l","met.l.l","cant")
metric_names<-c("tal-w-length","tal-w-length","met-w-length",
                "met-w-tri","ic-metaento-length","met-l-length","cant")


# read shape data -----
r<-3 #number of replicated specimens for each error type

shapemetadata<-read.csv(paste(datadir,
                              "colpocherus_manuscript.csv",sep="/"))
dentalmetrics<-read.csv(paste(datadir,shapedir,
                              "molaR_Batch_RFI_DNE_OPC.csv",sep="/"))
dentalmetrics2<-read.csv(paste(datadir,shapedir,
                               "molaR_Batch2_RFI_DNE_OPC.csv",sep="/"))

n<-nrow(shapemetadata) #number of specimens
replicate_id<-NULL
for (i in 1:r){replicate_id<-c(replicate_id,rep(i,r_p*r_c))}
replicate_id<-c(replicate_id,(r+1):n)

metadata<-shapemetadata[replicate_id,] %>%
  cbind(.,dentalmetrics[,])

#add centroid size to metadata
metadata$cs<-CS
metadata$lncs<-log(CS)

#add in second round of dental metrics
metadata$RFI2<-dentalmetrics2$RFI
metadata$DNE2<-dentalmetrics2$DNE
metadata$OPCR2<-dentalmetrics2$OPCR
metadata$Slope2<-dentalmetrics2$Slope

assign("last.warning", NULL, envir = baseenv())
