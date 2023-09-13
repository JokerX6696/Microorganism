rm(list=ls())
library('ggplot2')

df <- read.csv('plot.txt',sep = '\t')


plot_df <- df[df$group1 == df$group1[1],]
plot_df$sample <- factor(plot_df$sample,levels = plot_df$sample)
ggplot(data = plot_df,mapping = aes(x=sample,y=abd,fill='black')) + 
  geom_bar(stat = "identity",show.legend = F) + 
  labs(x = '', y= 'Relative abundance') + 
  theme_bw() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ) + 
  scale_fill_manual(values = '#AD002AFF') + 
  geom_text(aes(label = abd), vjust = -0.5) # 添加数值标签，vjust 控制标签的垂直位置
  
  
