#predict mean shape of a subgroup in the first Q PCs


#in function, mean shape is a kxm vector (colSums of 2d shape matrix)
PC2shape<-function(PC_analysis,sample_subset,PCs,mean_shape){
  pctrans<-matrix(nrow=length(PCs),ncol=length(mean_shape)) #make a blank matrix
  for (i in PCs){
    pcval<-mean(PC_analysis$x[sample_subset,i])
    pctrans[i,]<-pcval*PC_analysis$rotation[,i]
  }
  pcpredict<-colSums(pctrans)+mean_shape
  return(pcpredict)
}
