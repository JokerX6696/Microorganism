rm(list=ls())
setwd('D:/desk/XMSH_202311_7584/2.alpha_beta/')
library('ggplot2')
library('ggdist')
zb <- read.table('nmds_euclidean_otu_table_even.txt',sep = '\t',quote = "",header = T,row.names = 1)
zb <- zb[1:12,1:2]
#xfcdf <- zb[1:12,1:2]
zb$Group <- factor(gsub('_.+','',rownames(zb)),levels = unique(gsub('_.+','',rownames(zb))))

df <- data.frame(NMDS1 = zb[, 1], nmds2 = zb[, 2], Cluster = zb$Group,Group=zb$Group)



p <- ggplot() + 
  geom_point(data = zb,mapping = aes(x=NMDS1,y=NMDS2,color=Group),size=3) +
  theme_classic() +
  scale_color_manual(values = c('#00468BFF','#ED0000FF','#42B540FF','#0099B4FF')) 
  #stat_ellipse(aes(color = Group),level = 0.5)

ggsave(plot = p,filename = 'NMDS_euclidean.pdf',device = 'pdf',width = 9,height = 6)
