rm(list=ls())
require("vegan")
library('ggplot2')
data <- read.table('OTUs_even.xls',header=T,sep="\t",row.names=1,check.names=FALSE)
data <- t(data)
data_1 <- data[grepl("S_GC",rownames(data)),]
data_2 <- data[grepl("SF_GN",rownames(data)),]
data_3 <- data[grepl("SN_GN",rownames(data)),]
sp1 <- specaccum(data_1)
sp2 <- specaccum(data_2)
sp3 <- specaccum(data_3)
# pdf(file="specaccum_All.pdf")
# plot(sp1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue",xlab="Number of Samples Sequenced",ylab="Species Detected")
# plot(sp2, ci.type="poly", col="red", lwd=2, ci.lty=0, ci.col="lightblue",xlab="Number of Samples Sequenced",ylab="Species Detected")
#                      dev.off()

df <- data.frame(sp1_x=sp1$sites[1:20],sp1_y=sp1$richness[1:20],sp2_x=sp2$sites[1:20],sp2_y=sp2$richness[1:20],sp3_x=sp3$sites[1:20],sp3_y=sp3$richness[1:20])
p <- ggplot(data = df) + 
  geom_line(aes(x=sp1_x,y=sp1_y,color='#00468BFF')) + 
  geom_line(aes(x=sp2_x,y=sp2_y,color='#ED0000FF')) +
  geom_line(aes(x=sp3_x,y=sp3_y,color='#42B540FF')) +
  labs(x="Number of Samples Sequenced",y='Species Detected',color = 'Group' ) + 
  scale_color_manual(breaks=c(c('#00468BFF','#ED0000FF','#42B540FF')),values = c('#00468BFF','#ED0000FF','#42B540FF'),labels = c('S_GC','SF_GN','SN_GN')) +
  theme_bw() + 
  theme(panel.grid = element_blank())

ggsave(filename = 'S_GC_SF_GN_SN_GN_specaccum_All.pdf',plot = p,device = 'pdf',width = 9,height = 6)
ggsave(filename = 'S_GC_SF_GN_SN_GN_specaccum_All.png',plot = p,device = 'png',width = 9,height = 6)