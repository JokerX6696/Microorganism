rm(list=ls())

library(ggplot2)
library(reshape2)

file <- 'all_KEGG_L2.xls'
df <- read.table(file,sep="\t",header = TRUE,row.names = 1)
df$mean <- apply(df,1,mean)
df$name <- rownames(df)
############################### 饼图 #######################################
p1 <- ggplot(data = df,mapping = aes(x="",y=mean,fill=name)) + 
  geom_bar(stat = "identity") + 
  coord_polar(theta = "y") + 
  guides(fill = guide_legend(title = NULL,ncol=2)) + 
  labs(x="",y="")+
  theme_bw() + 
  theme(axis.text = element_blank(),## 删去所有刻度线
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        )   
ggsave(filename = 'kegg_Pie.png',plot = p1,width = 12,height = 6,device = 'png')
############################ 柱状堆积图 ####################################
df_2 <- df[,-8]
df_2 <- melt(df_2,id.vars = 'name')
names(df_2) <- c("Pathway", "Sample", "Abundance")
p2 <- ggplot(data = df_2,mapping = aes(x=Sample,y=Abundance,fill=Pathway)) + 
  geom_bar(stat = "identity",width=0.6,position = "fill") + 
  theme_bw() + 
  labs(x="",y="")+
  guides(fill = guide_legend(title = NULL,ncol=2))# + 
  #theme(axis.text = element_blank(),## 删去所有刻度线
        #axis.ticks.y = element_blank(),
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank()
  #)   
ggsave(filename = 'kegg_stark.png',plot = p2,width = 12,height = 6,device = 'png')
