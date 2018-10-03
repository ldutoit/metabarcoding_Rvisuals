#!/usr/bin/#

library(RColorBrewer)

#######Read and prepare the data
data<-read.table("barplot_export_family_level_resolution.csv",h=T,sep=",")
colnames(data)# see what coilumns we have

# get only gammaproteobacteria with bacteria as line, samples as columns
gamma<-t(data[,grep("Gammaproteobacteria",colnames(data))])


rownames(gamma)<-gsub("[a-z,.]*_","",rownames(gamma),perl=T,ignore.case=T)#change row names to family names
colnames(gamma)<-data$index #colnames become samples

#Transform this data in % of total gammaproteobacteria in sampole
gamma_percentage=apply((gamma), 2, function(x){x*100/sum(x,na.rm=T)})
head(gamma_percentage)


############## Make a stacked barplot--> it will be in %!
coul = brewer.pal(nrow(gamma), "Paired") 
par(mfrow=c(1,2))
barplot(gamma_percentage[,data$BodySite=="left palm"], col=coul , border="white",ylab="Proportion(%)",las=2,cex.names=0.8)
barplot(gamma_percentage[,data$BodySite=="right palm"], col=coul , border="white",ylab="Proportion(%)",las=2,cex.names=0.8)


############### average pies
# group data per body type
aggregated_data<-aggregate(t(gamma_percentage), list(data$BodySite), mean) #transpose and  obtain mean per bodysite
aggregated_data<-apply(aggregated_data,c(1,2),as.numeric) #make numeric
aggregated_data<-aggregated_data[,-1]# remove group
aggregated_data

# let's visualize left palm
par(mfrow=c(1,1))
lbls <- paste(substr(colnames(aggregated_data),1,6),". ", round(aggregated_data[2,],2),"%",sep="") # add percents to labels to create labels 
pie(aggregated_data[2,],radius=0.4,sub="left palm",labels=lbls,cex=0.5) 

##Proportion of everything small in an  "others" categories
threshold=5 # group evertything below 5% as Others
belowthresh<-which(aggregated_data[2,]<threshold)
others<-sum(aggregated_data[2,belowthresh])
newpie<-c(aggregated_data[2,-belowthresh],others)
names(newpie)[length(newpie)]<-"Others" #last label
lbls <- paste(names(newpie)," ", round(newpie,2),"%",sep="") # add percents to labels to create labels 
pie(newpie,radius=0.4,sub="left palm",labels=lbls,cex=0.5) 
