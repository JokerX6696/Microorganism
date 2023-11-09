rm(list=ls())
setwd('D:/desk/XMSH_202311_7630/3.bar')
library('ggplot2')
library('dplyr')
library(reshape2)
df <- read.table('Abundance.filtered.xls',header = T,quote = "",comment.char = "",sep = '\t')
df <- df[,7:59]
rownames(df) <- df$Species
df <- df[,-1]
sel <- c('Fusobacterium_nucleatum', 'Porphyromonas_gingivalis', 'Bulleidia_moorei')
sx <- c('Day_0_2','Day_0_4','Day_0_5','Day_0_8','Day_0_11','Day_0_12','Day_0_13','Day_0_14','Day_0_15','Day_0_16','Day_0_17','Day_0_21','Day_0_22','Day_0_24','Day_0_26','Day_0_27','Day_0_28','Day_0_29','Day_28_4','Day_28_7','Day_28_9','Day_28_10','Day_28_13','Day_28_15','Day_28_17','Day_28_18','Day_28_19','Day_28_21','Day_28_22','Day_28_23','Day_28_27','Day_28_34','Day_28_36','Day_28_37','Day_28_41','Day_28_43','LX','LQ','ZFX','HJ','LYN','JSZ','MXM','ZHL','ZML','CL','ZF','XLJ','SXY','CJY','LX_LS','LY')
gp <- c('Day_0','Day_0','Day_0','Day_0','Day_0','Day_0','Day_0','Day_0','Day_0','Day_0','Day_0','Day_0','Day_0','Day_0','Day_0','Day_0','Day_0','Day_0','Day_28','Day_28','Day_28','Day_28','Day_28','Day_28','Day_28','Day_28','Day_28','Day_28','Day_28','Day_28','Day_28','Day_28','Day_28','Day_28','Day_28','Day_28','Control','Control','Control','Control','Control','Control','Control','Control','Control','Control','Control','Control','Control','Control','Control','Control')
df <- as.data.frame(t(df[sel,]))
df <- df[sx,]
df$group <- gp
df$sample <- rownames(df)

info <- c()

for (s in sel) {
  for (g in unique(gp)) {
    tmp <- df[df$group==g,s]
    avg <- mean(tmp)
    ret <- sd(tmp) / sqrt(length(tmp))
    info <- c(info,g,s,avg,ret)
  }
}

df_plot <- as.data.frame(matrix(data = info,ncol = 4,byrow = T))
names(df_plot) <- c('Group','Species','Abundance','erbar')

df_plot$Group <- factor(df_plot$Group,levels = unique(gp))
df_plot$Species <- gsub(sel[3],'Solobacterium_moorei',df_plot$Species)
df_plot$Abundance <- as.numeric(format(as.numeric(df_plot$Abundance), scientific = TRUE))
df_plot$erbar <- as.numeric(format(as.numeric(df_plot$erbar), scientific = TRUE))
###############################
p <- ggplot(data = df_plot,mapping = aes(x=Species,y=Abundance,fill = Group)) +
  geom_bar(stat = 'identity',position = position_dodge(width = 0.9)) +
  theme_bw() + 
  geom_errorbar(aes(ymin = (Abundance - erbar),ymax=(Abundance + erbar)),width = 0.2,position = position_dodge(width = 0.9)) + 
  scale_fill_manual(breaks = unique(gp),values = c('#00468BFF','#ED0000FF','#42B540FF'))# + 
  # coord_cartesian(ylim = c(max(df_plot$Abundance), min(df_plot$Abundance))) + 
  # scale_y_log10()

ggsave(plot = p,filename = 'Group_bar_plot.pdf',device = 'pdf',width = 9,height = 6)



















# df_plot <- melt(df)
# names(df_plot) <- c('Group','Sample','Species','Abundance')
# df_plot$Sample <- factor(df_plot$Sample,levels = sx)
# df_plot$Group <- factor(df_plot$Group,levels = unique(gp))
# ################################
# df_plot <- df_plot[df_plot$Species == sel[1],]
# df_plot <- df_plot[df_plot$Abundance != 0,]
# ###############################
# ggplot(data = df_plot,mapping = aes(x=Group,y=Abundance,fill = Sample)) + 
#   geom_bar(stat = 'identity',position = position_dodge()) + 
#   theme_bw()
# 
# 
# 




