#!/data/software/install/miniconda3/envs/R.3.6.0/bin/Rscript
#Date:2015/10/14
## modified by jianming.zhang@oebiotech.com 2017.08.07
#args<-commandArgs(T)
file <- 'D:/desk/XMSH_202305_3817/OTUs_even.xls'
otu<-read.table(file,header=T,sep="\t")
#####################

otu_ICI_NE <- otu[,grepl("ICI_NE|OTU_ID",colnames(otu))]
otu_ICI_E <- otu[,grepl("ICI_E|OTU_ID",colnames(otu))]

otu_NAC_NE <- otu[,grepl("NAC_NE|OTU_ID",colnames(otu))]
otu_NAC_E <- otu[,grepl("NAC_E|OTU_ID",colnames(otu))]


otu_GC <- otu[,grepl("GC|OTU_ID",colnames(otu))]



#######################
otu <- otu_NAC_E
#########################
rank<-apply(otu[,2:ncol(otu)],2,function(x) rev(sort(x/sum(x)*100)))
otunum<-apply(otu[,2:ncol(otu)],2,function(x) sum(x>0))
max_x<-max(otunum)
rank[rank==0]=NA
write.table(rank[1:max_x,],"otu.rank.xls",row.names=TRUE,sep="\t",quote=FALSE,col.names=NA,na="")

#if(ncol(otu)>10) {
#    rank<-rank[,1:10]
#    max_x<-max(otunum[1:10])
#}
cols<-c("#4A7EBB","#BE4B48","#0D7339","#3D90D9","#D98236","#DF514F","#8CCDA3","#3F5D7D","#279B61","#008AB8","#993333","#A3E496","#95CAE4","#CC3333","#FFCC33","#FFFF7A","#CC6699")
if (ncol(rank)>length(cols)) {
    library(RColorBrewer)
    cols<-c(cols,colorRampPalette(brewer.pal(8,"Dark2"))(ncol(rank)-length(cols)))
}
if((ncol(otu)-1)>100){
    pdf("RankAbundance.pdf",height=5,width=10,onefile=FALSE)
    layout(matrix(c(1,2),1,2))
}else{
    pdf("RankAbundance.pdf",height=5,width=7,onefile=FALSE)
    layout(matrix(c(1,1,2),1,3))
}
par(mar=c(5,4,2,0))
options(scipen=9999999)
plot(c(0,max_x),c(0.001,100),type="n",xlab="Species Rank",ylab="Relative Abundence",log="y",font.lab=2,yaxt="n",font.axis=2,cex.axis=.7)
axis(2,at=c(0.001,0.01,0.1,1,10,100),labels=c(0.001,0.01,0.1,1,10,100),las=2,font.axis=2,cex.axis=.7)
for (i in 1:ncol(rank)) {
    lines(rank[,i],col=cols[i],lwd=2)
}
par(mar=c(4,1,2,0))
plot.new()
ncol<-ifelse((ncol(otu)-1)>100,ceiling((ncol(otu)-1)/45),ceiling((ncol(otu)-1)/38))
cex<-ifelse((ncol(otu)-1)>100,0.5,0.8)
legend("topleft",lty=1,merge=T,legend=colnames(rank),col=cols,ncol=ncol,lwd=1,cex=cex,bty="n",text.font=2)
dev.off()
