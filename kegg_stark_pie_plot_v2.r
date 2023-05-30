rm(list=ls())
setwd('D:\\desk\\XMSH_202305_4060')
library('ggplot2')
library(reshape2)
file <- 'all_KEGG_L2.xls'
df <- read.table(file,sep = '\t',header = TRUE,row.names = 1)
S <- '#6BADD5,#640010,#072F6B,#C51517,#1B6AA5,#BDBDBD,#AD017D,#F768A0,#2D248A,#48006A,#8B5109,#A42A2A,#FBC723,#79A2A6,#003B2F,#35968F,#F8B79E,#F79594'
colors_s <- strsplit(S,',')[[1]]

colors <- c(colors_s,colors_s,colors_s)
colors <- colors[1:44]





#################### 画饼图
df_pie <- df
df_pie$mean <- apply(df_pie,1,mean)
df_pie$name <- rownames(df_pie)
p <- ggplot(df_pie,aes(x="",y = mean,fill=name)) + 
  geom_bar(width = 1,stat = "identity",color = "white") + 
  guides(fill = guide_legend(ncol = 2,title = "")) + 
  xlab("") + ylab("") +
  scale_fill_manual(breaks = df_pie$name, values = colors) +
  coord_polar(theta="y") + 
  theme_bw() + 
  theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
        panel.grid = element_blank()
        )
ggsave('Kegg_Pie.pdf',plot = p,device = 'pdf',width = 16,height = 6)
ggsave('Kegg_Pie.png',plot = p,device = 'png',width = 16,height = 6)
################### 柱状堆积
df_stack <- df
df_stack$name <- rownames(df)
df_stack <- melt(df_stack)
colnames(df_stack) <- c("name","sample","value")
p <- ggplot(df_stack,aes(x=sample,y = value,fill=name)) + 
  geom_bar(width = 0.6,stat = "identity",position = "fill") + 
  guides(fill = guide_legend(ncol = 2,title = "")) + 
  xlab("") + ylab("") +
  scale_fill_manual(breaks = df_pie$name, values = colors) +
  theme_bw() + 
  theme(axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        panel.grid = element_blank()
  )
ggsave('Kegg_Stack.pdf',plot = p,device = 'pdf',width = 16,height = 6)
ggsave('Kegg_Stack.png',plot = p,device = 'png',width = 16,height = 6)
##        dev.off()





