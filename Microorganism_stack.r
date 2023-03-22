rm(list=ls())
library(ggplot2)
library(reshape2)
setwd('D:/desk/R_temp')
df <- read.table('Abundance_Stat.all.xls', sep = '\t', header = TRUE, comment.char='')
df <- df[,-c(2:7)]
colnames(df)[1] <- 'Type'
df <- df[ df$Type != 'human',]
# 去除宿主后 对剩余样本做归一化总丰度 得到新矩阵
for (variable in 2:length(df)) {
  s = sum(df[,variable])
  df[,variable] <- df[,variable]/s
}
# 读取新分组表格
group <- readxl::read_xlsx('metadata-archaea20230322.xlsx', sheet = 'metadata(1)')
group <- group[-1,c(2,5)]
colnames(group) <- c('sample', 'group')
for(i in group$sample) {
  if (! i %in% colnames(df)) {
    group <- group[-(which(i==group$sample)),]
  }
}

BC <- group[group$group=='BC',]$sample
GC <- group[group$group=='GC',]$sample
BP <- group[group$group=='BP',]$sample
GP <- group[group$group=='GP',]$sample
group_list <- list(BC, GC, BP, GP)
names(group_list) <- c('BC', 'GC', 'BP', 'GP')
# 根据列表提取 df 样本
# BC
temp_df <- df[,c(1,which(colnames(df) %in% BC))]
temp_df <- melt(temp_df,variable.name = 'sample')
p <- ggplot(data=temp_df,mapping = aes(x = sample, y = value, fill = Type, color = NULL)) + 
  geom_bar(stat = 'identity',width=0.6,position = "fill") + 
  xlab('BC') + ylab('Proportion') + 
  theme(panel.background = element_blank(),
        panel.grid = element_blank(), 
        axis.line = element_line(colour = "#000000",size = 1),
        axis.text = element_text(colour = "#000000" ,size = 10),
        axis.text.x = element_text(angle = 45,vjust = 0.85,hjust = 0.75)) ##就是这里)
ggsave(filename = './group_BC_Type.pdf', device = 'pdf', plot = p,width = 9, height = 6)
# GC
temp_df <- df[,c(1,which(colnames(df) %in% GC))]
temp_df <- melt(temp_df,variable.name = 'sample')
p <- ggplot(data=temp_df,mapping = aes(x = sample, y = value, fill = Type, color = NULL)) + 
  geom_bar(stat = 'identity',width=0.6,position = "fill") + 
  xlab('GC') + ylab('Proportion') + 
  theme(panel.background = element_blank(),
        panel.grid = element_blank(), 
        axis.line = element_line(colour = "#000000",size = 1),
        axis.text = element_text(colour = "#000000" ,size = 10),
        axis.text.x = element_text(angle = 45,vjust = 0.85,hjust = 0.75)) ##就是这里)
ggsave(filename = './group_GC_Type.pdf', device = 'pdf', plot = p,width = 9, height = 6)
# BP
temp_df <- df[,c(1,which(colnames(df) %in% BP))]
temp_df <- melt(temp_df,variable.name = 'sample')
p <- ggplot(data=temp_df,mapping = aes(x = sample, y = value, fill = Type, color = NULL)) + 
  geom_bar(stat = 'identity',width=0.6,position = "fill") + 
  xlab('BP') + ylab('Proportion') + 
  theme(panel.background = element_blank(),
        panel.grid = element_blank(), 
        axis.line = element_line(colour = "#000000",size = 1),
        axis.text = element_text(colour = "#000000" ,size = 10),
        axis.text.x = element_text(angle = 45,vjust = 0.85,hjust = 0.75)) ##就是这里)
ggsave(filename = './group_BP_Type.pdf', device = 'pdf', plot = p,width = 9, height = 6)
# GP
temp_df <- df[,c(1,which(colnames(df) %in% GP))]
temp_df <- melt(temp_df,variable.name = 'sample')
p <- ggplot(data=temp_df,mapping = aes(x = sample, y = value, fill = Type, color = NULL)) + 
  geom_bar(stat = 'identity',width=0.6,position = "fill") + 
  xlab('GP') + ylab('Proportion') + 
  theme(panel.background = element_blank(),
        panel.grid = element_blank(), 
        axis.line = element_line(colour = "#000000",size = 1),
        axis.text = element_text(colour = "#000000" ,size = 10),
        axis.text.x = element_text(angle = 45,vjust = 0.85,hjust = 0.75)) ##就是这里)
ggsave(filename = './group_GP_Type.pdf', device = 'pdf', plot = p,width = 9, height = 6)
