rm(list = ls())
setwd('D:/desk/R_wkdir')
#### 本脚本 通过标签矩阵计算 sequence per sample 与 Shannon 的 line 图
#  这里设置 seed 防止每次抽样结果不同!
set.seed(1311)
#  加载需要用到的包
library('ggplot2')

########### para
tag_file <- 'mic_tag_stat.xlsx'  # 输入的标签矩阵文件
rm_host <- 'human'  # 宿主种类




#############
#Shannon <- function()

############# 生成标准格式的 标签矩阵
df_tag <- data.frame(readxl::read_xlsx(tag_file, col_names = TRUE))
rownames(df_tag) <- df_tag[,1]
df_tag <- df_tag[,-1]
df_tag <- df_tag[which(rownames(df_tag) != rm_host),]











