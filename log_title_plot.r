#  用于绘制 对数坐标轴！
rm(list=ls())
setwd('D:/desk/XMSH_202310_7100')

library('dplyr')
library('ggplot2')
library(gridExtra)
library('ggbreak')

abd <- 'Abundance_Stat.all.xls'
df <- read.table(abd,sep = '\t',comment.char = "",header = T)
names(df)[1] <- 'Kingdom'

df <- df[,-c(2:7)]
df <- df[,grepl('case|control|Kingdom',names(df))]
df <- df[,c(1,order(names(df)[-1])+1)]

result <- df %>%
  group_by(Kingdom) %>%
  summarise(across(everything(), sum))
result <- as.data.frame(result)
rownames(result)<- result$Kingdom

result <- result[,-1]


hm <- result['human',]
result <- result[1:3,]
# 做数据归一化，删除宿主
for (vue in 1:38) {
  sy <- 1-hm[1,vue]
  result[,vue] <- result[,vue]/sy
}
# 整理绘图矩阵
df_plot <- as.data.frame(t(result))
df_plot$group <- c(
  rep('case',19),
  rep('control',19)
)

df_plot <- melt(df_plot,'group')
names(df_plot) <- c('group','type','value')
df_plot$type <- factor(df_plot$type,levels = c( 'Bacteria','Fungi','Archaea'))
df_plot$value <- df_plot$value * 100
df_plot_case <- df_plot[df_plot$group == 'case',]
df_plot_control <- df_plot[df_plot$group == 'control',]
# plot


p1 <- ggplot(df_plot_case, aes(x = type, y = value,shape=type,color=type)) +
  geom_point(position = position_jitter(width = 0.2), size = 2,show.legend = F) + 
  theme_bw() + 
  labs(x='case',y='Abundance (%)') + 
  scale_color_manual(breaks = c('Archaea', 'Bacteria','Fungi'),values=c('#00468BFF','#ED0000FF','#42B540FF')) + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 18)) + 
  scale_y_log10()


p2 <- ggplot(df_plot_control, aes(x = type, y = value,shape=type,color=type)) +
  geom_point(position = position_jitter(width = 0.2), size = 2,show.legend = F) + 
  theme_bw() + 
  labs(x='control') + 
  scale_color_manual(breaks = c('Archaea', 'Bacteria','Fungi'),values=c('#00468BFF','#ED0000FF','#42B540FF')) + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 18),
        axis.ticks = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank())+
  scale_y_log10()
pdf(file = 'Abundance_point.pdf',width = 9,height = 6)
grid.arrange(p1, p2, ncol = 2,widths=c(40,37))
dev.off()

