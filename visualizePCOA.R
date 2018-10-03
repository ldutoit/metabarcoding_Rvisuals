#!/usr/bin/r

#load PCA
nsample=33
data<-read.csv("bray_curtis_pcoa_ordination.txt",sep="\t",skip=9,header=F,)
data<-data[1:nsample,]
colnames(data)<-c("sample",paste("PC",1:(ncol(data)-1),sep=""))


#Load metadata
metadata<-read.csv("sample-metadata.tsv",h=F,sep="\t",skip=4)
metadata<-metadata[order(match(metadata[,1],data[,1])),]# sort the metadata according to the data ind names
head(metadata)
head(data)

#Plot it
cols=c("#d7191c","#fdae61","#abd9e9","#2c7bb6")# pick colors, check http://colorbrewer2.org/
plot(data$PC1,data$PC2,pch=19,xlab="PC 1",ylab="PC 2",col=cols[metadata[,4]],cex=1.2,xlim=c(-0.6,0.4),ylim=c(-0.6,0.4),frame=F)
legend("bottomleft",levels(as.factor(metadata[,4])),col=cols,pch=19,bty="n")

