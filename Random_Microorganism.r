rm(list=ls())
setwd('D:/desk/何胜夫售后/20230224_DOE202213632-b2_何胜夫-孙雅婷-73个人2bRAD-M项目_结题报告/PA/item7')
library(ggplot2)
library(reshape2)

ab <- read.table(file = 'L7.others_Top30_species_group.xls',sep = '\t',header = TRUE)

file <- 'indicators_indicator_value.xls'
df <- read.table(file = file,sep = '\t',header = TRUE)
df <- df[,1:3]
df <- melt(df,variable.name = 'OTUId')
colnames(df) <- c('type','group','indval')
p <- ggplot() + 
  geom_point(data = df,mapping = aes(x = factor(group,levels = c('S_Control','S_GC')),y=type,color=group,size=indval)) + 
  labs(x="",y="") + 
  scale_color_manual(breaks=c('S_GC','S_Control'),values = c('#E7AC48','#6EC5EA')) +
  ggtitle('indicator analysis') +
  theme_bw() + 
  theme(
    text=element_text(size=16,  family="serif"),
    plot.title = element_text(hjust = 0.5),
    line=element_line(linetype = 2)
    )
ggsave(filename = 'indicator.png',plot = p,device = 'png',width = 9,height = 6)
ggsave(filename = 'indicator.pdf',plot = p,device = 'pdf',width = 9,height = 6)
#          dev.off()

#########################################################################
file <- 'importance.xls'
df <- read.table(file = file,sep = '\t',header = TRUE)
df$type <- rownames(df)
colnames(df)[1] <- 'Abundance' # 不代表丰度 随机取了个名

df <- merge(df,ab,by.x = 'type',by.y = 'Taxonomy',all = TRUE)

df <- df[rev(order(df$Abundance)),]
df$type <- factor(df$type,levels = rev(df$type))

p <- ggplot() + 
  geom_point(data = df,mapping = aes(x = Abundance,y=type,size=S_GC,color = S_Control)) + 
  labs(x="",y="",color="Abundance",size="Abundance") + 
  scale_color_gradient(low = "#00468BFF",high = "#AD002AFF") +
  ggtitle('Random forest-Accuracy') +
  theme_bw() + 
  #scale_color_discrete(name="Experimental") +
  theme(
    text=element_text(size=16,  family="serif"),
    plot.title = element_text(hjust = 0.5),
    line=element_line(linetype = 2)
  )
ggsave(filename = 'Random_forest_Accuracy.png',plot = p,device = 'png',width = 9,height = 6)
ggsave(filename = 'Random_forest_Accuracy.pdf',plot = p,device = 'pdf',width = 9,height = 6)
#########################################################################
file <- 'importance_Gini.xls'
df <- read.table(file = file,sep = '\t',header = TRUE)
df$type <- rownames(df)
colnames(df)[1] <- 'Abundance'
df <- merge(df,ab,by.x = 'type',by.y = 'Taxonomy',all = TRUE)

df <- df[rev(order(df$Abundance)),]
df$type <- factor(df$type,levels = rev(df$type))
p <- ggplot() + 
  geom_point(data = df,mapping = aes(x = Abundance,y=type,size=T_GC,color = T_Control)) + 
  labs(x="",y="",color="Abundance",size="Abundance") + 
  scale_color_gradient(low = "#00468BFF",high = "#AD002AFF") +
  ggtitle('Random forest-Gini') +
  theme_bw() + 
  theme(
    text=element_text(size=16,  family="serif"),
    plot.title = element_text(hjust = 0.5),
    line=element_line(linetype = 2)
  )
ggsave(filename = 'Random_forest_Gini.png',plot = p,device = 'png',width = 9,height = 6)
ggsave(filename = 'Random_forest_Gini.pdf',plot = p,device = 'pdf',width = 9,height = 6)




