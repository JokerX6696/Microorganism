rm(list=ls())
setwd('D:/desk/XMSH_202310_7100')
library(reshape2)
library(ggalluvial)

# modified by tiansheng.xu; tiansheng.xu@oebiotech.com
library(ggplot2)

cols_new <- c("#4D4D4D","#134C45","#188078","#4E948B","#165B9A","#1E92C8","#73C1E0","#652025","#B22124","#DDA085",
              "#C0591D","#D99407","#C03120","#717495","#CA9AC4","#5F2462","#536AA9","#272789","#66B1E3","#7B5C3A",
              "#F6CA7E","#82C68C","#F19388","#83CCCA","#8F90A9","#EFA665","#7897CE","#EDDE87","#9787BE","#2380C5",
              "#71BE6F")
for(file in dir(pattern = "*xls")){
  otu1 <-read.table(file,header=T,check.names=FALSE,sep="\t",row.names = 1)
  # 新增对所有样本求平均
  df_case <- otu1[,1:19]
  df_control <- otu1[,20:38]
  case <- apply(df_case, 1, mean)
  control <- apply(df_control, 1, mean)
  otu1 <- data.frame(Taxonomy=rownames(otu1),case=case,control=control)
  counts <- apply(otu1[,2:ncol(otu1)],1,sum)
  otu <- otu1[which(counts!=0),]
  n<-ncol(otu)
  #m<-log(n/2,2.85)
  if(n<10){m=0.75
  }else if(n>=10 & n<40){m<-log(n/2,9)
  }else if(n>=40 & n<80){m<-log(n/2,7)
  }else if(n>=80 & n<120){m<-log(n/2,3.5)
  }else {m<-log(n/2,2.85)
  }
  if (nrow(otu)==1){q()}
  if (nrow(otu)>length(cols_new)) {
    library(RColorBrewer)
    cols<-cols_new
    # cols<-c(cols,colorRampPalette(brewer.pal(8,"Dark2"))(nrow(otu)-length(cols)))
  }
  if (ncol(otu)>length(cols_new)) {
    library(RColorBrewer)
    cols<-c(cols,colorRampPalette(brewer.pal(8,"Dark2"))(ncol(otu)-length(cols)))
  }
  
  rownames(otu) <- otu[,1]
  otu <-otu[,-1]
  al <- which(rownames(otu) %in% c("All"))
  if(length(al)) otu <-otu[-al,]
  rowsum <-sapply(1:nrow(otu),function(x) sum(otu[x,]))
  otu<-otu[order(rowsum,decreasing=TRUE),]
  dat <-sapply(1:ncol(otu),function(x) otu[,x]/sum(otu[,x])) 
  colnames(dat) <-colnames(otu)
  rownames(dat) <-rownames(otu)
  lab <-rownames(dat)
  
  df <- data.frame(dat)
  df$type <- rownames(df) 
  df <- melt(df,variable.name = 'type')
  colnames(df) <- c('type','sample','value')
  
  df$type <- factor(x=df$type,levels = rev(rownames(dat)))
  
  col_num <- 31 - length(unique(df$type)) +1
  cols_new2 <- cols_new[col_num:31]
  p <- ggplot(data = df, mapping = aes(x=sample,y=value,fill=type, stratum=type, alluvium=type )) +
    geom_bar(stat = 'identity',position = 'fill',width = 0.6) + 
    scale_fill_manual("",values = cols_new2) + 
    labs(x="",y='Relative abundance(%)') + 
    geom_alluvium(aes(fill=type),width = 0.6) +
    scale_y_continuous(breaks=c(0.00,0.25,0.50,0.75,1.00), labels = c(0,25,50,75,100)) +
    theme_bw()
  out <- sub(pattern = "^.+\\.o","o",file)
  out <- sub(pattern = "\\.xls","",out)
  #        dev.off()              
  ggsave(filename = paste(out,'png',sep = "."),plot = p,device = 'png',width = 9,height = 6)
  ggsave(filename = paste(out,'pdf',sep = "."),plot = p,device = 'pdf',width = 9,height = 6)
}


