rm(list=ls())
setwd('D:\\desk\\XMSH_202305_3817\\PA\\9')
library('ggplot2')
library(ggsignif)
library(reshape2)
file <- 'KEGG_L2.Kruskal_Wallis.diff.top10.boxplot.txt'
df <- read.table(file, sep = '\t',header = TRUE)
df$group_cl <- sub("_.*$","",df$Sample)
df$group_xf <- sub("_..?$","",df$Sample)
color_xf <- c("#2E75CE","#9AAF8C","#F4B083","#538035","#C00000")
color_cl <- c("#2E75CE","#9AAF8C","#C00000")
td <- combn(c( "GC","NAC_E","NAC_NE","ICI_E","ICI_NE"),2)
ltd <- list(c(td[,1]),c(td[,2]),c(td[,3]),c(td[,4]),c(td[,5]),c(td[,6]),c(td[,7]),c(td[,8]),c(td[,9]),c(td[,10]))
max <- max(df$value)
ytd <- c(0.03,0.05,0.07,0.09,0.11,0.13,0.15,0.17,0.19)
tip <- c(c(0.05,0.05),c(0.05,0.05),c(0.05,0.05),c(0.05,0.05),c(0.05,0.05),c(0.05,0.05),c(0.05,0.05),c(0.05,0.05),c(0.05,0.05),c(0.05,0.05))
cls <- "Signaling molecules and interaction"#"Signaling molecules and interaction" "Cellular community - eukaryotes" 
df <- df[df$id == cls,]




df$group_xf <- factor(df$group_xf,levels = c( "GC","NAC_E","NAC_NE","ICI_E","ICI_NE"))
p <- ggplot(data = df,mapping = aes(x=group_xf,y=Abundance,fill=group_xf)) +
  stat_boxplot(geom = "errorbar", width=0.1,size=0.8) +
  geom_boxplot(outlier.colour="white",size=0.8) +
  xlab("Group") + ylab("Abundance") +
  scale_fill_manual(breaks = c("GC","NAC_E","NAC_NE","ICI_E","ICI_NE"),values = color_xf) +
  theme_bw() +
  theme(panel.background =element_blank(), #背景
        axis.line=element_line(),#坐标轴的线设为显示
        legend.position="none",
        plot.title = element_text(size=12,hjust=0.5))+#图例位置
  #geom_jitter(width = 0.2)+#添加抖动点
  ggtitle(cls) + #标题
  geom_signif(comparisons = ltd,#设置需要比较的组
              test = 'wilcox.test', ##计算方法
              y_position = ytd ,#图中横线位置设置
              tip_length = tip,#横线下方的竖线设置
              size=0.8,color="black")
ggsave(filename = paste('xf_',cls,'.pdf',sep = ''),plot = p,device = 'pdf',width = 9,height = 6)
ggsave(filename = paste('xf_',cls,'.png',sep = ''),plot = p,device = 'png',width = 9,height = 6)
##################
df$group_cl <- factor(df$group_cl,levels = c("GC","NAC","ICI"))
max = floor(max(df$Shannon)) + 0.5
###############################################################
p <- ggplot(data = df,mapping = aes(x=group_cl,y=Abundance,fill=group_cl)) +
  stat_boxplot(geom = "errorbar", width=0.1,size=0.8) +
  geom_boxplot(outlier.colour="white",size=0.8) +
  xlab("Group") + ylab("Abundance") +
  scale_fill_manual(breaks = c("GC","NAC","ICI"),values = color_cl) +
  theme_bw() +
  theme(panel.background =element_blank(), #背景
        axis.line=element_line(),#坐标轴的线设为显示
        legend.position="none",
        plot.title = element_text(size=12,hjust=0.5))+#图例位置
  #geom_jitter(width = 0.2)+#添加抖动点
  ggtitle(cls) + #标题
  geom_signif(comparisons = list(c("ICI","NAC"),c("NAC","GC"),c("ICI","GC")),#设置需要比较的组
              test = 'wilcox.test', ##计算方法
              y_position = c(0.125,0.135,0.145),#图中横线位置设置
              tip_length = c(c(0.05,0.05),c(0.05,0.05),c(0.05,0.05)),#横线下方的竖线设置
              size=0.8,color="black"
  ) 
ggsave(filename = paste('cl_',cls,'.pdf',sep = ''),plot = p,device = 'pdf',width = 9,height = 6)
ggsave(filename = paste('cl_',cls,'.png',sep = ''),plot = p,device = 'png',width = 9,height = 6)

