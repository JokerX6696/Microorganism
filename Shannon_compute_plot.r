rm(list = ls())
setwd('D:/desk/R_wkdir')
#### 本脚本 通过标签矩阵计算 sequence per sample 与 Shannon 的 line 图
#  这里设置 seed 防止每次抽样结果不同!
set.seed(1311)
#  加载需要用到的包
library('ggplot2')
library('reshape2')
########### para
tag_file <- 'mic_tag_stat.xlsx'  # 输入的标签矩阵文件
rm_host <- 'human'  # 宿主种类
samples_size <- c(5,10,50,100,1000,2000,5000,8000,10000,20000)



############# 计算信息熵
Shannon <- function(sample_vecoor){
  prop <- table(sample_vecoor) / length(sample_vecoor) # 计算每个值的概率
  entropy <- -sum(prop * log2(prop), na.rm = TRUE)  # 计算香农熵
  return(entropy)
}
############# 获取信息熵向量
get_shannon <- function(df){
  shannon_value <- c()
  for (j in 1:ncol(df)) {  # 计算每一列的信息熵
    sample_all <- c()
    for (i in 1:nrow(df)){
      Mic_name <- rownames(df)[i]
      Mic_num <- df[i,1]
      sample_all <- c(sample_all,rep(Mic_name,Mic_num))
    }
    
    
    for (sample_size in samples_size) {
      stat_counts <- sample(sample_all,sample_size)
      shannon_value <- c(shannon_value,Shannon(stat_counts))
    }
  }
  
  return(shannon_value)
}
############# 生成标准格式的 标签矩阵
df_tag <- data.frame(readxl::read_xlsx(tag_file, col_names = TRUE))
rownames(df_tag) <- df_tag[,1]
df_tag <- df_tag[,-1]
df_tag <- df_tag[which(rownames(df_tag) != rm_host),]



# 随机取样后 获取不同取样次数的香侬熵,返回一个向量
shannon_vector <- get_shannon(df_tag)
# 处理 香农熵矩阵 用于后续画图
df_shannon <- data.frame(matrix(shannon_vector,nrow = length(samples_size))) # 向量根据不同的取样次数 变为矩阵
names(df_shannon) <- names(df_tag)
df_shannon$x_pos <- samples_size

df_shannon_plot <- melt(df_shannon,id.vars = 'x_pos')
names(df_shannon_plot)[2:3] <- c('sample','Shanno')


levels(df_shannon_plot$sample) <- c(sort(as.vector(unique(df_shannon_plot$sample))))  # 注意这里可以手动控制一下顺序
################### 开始绘图
p <- ggplot(data = df_shannon_plot,mapping = aes(x=x_pos,y=Shanno,color=sample)) + 
  geom_line() + 
  theme_bw() + 
  xlab('Tag_per_sample')

ggsave(filename = 'Shannon.png',plot = p,device = 'png',width = 9,height = 6)

df_save <- data.frame(Tag_num = samples_size,df_shannon)
df_save <- df_save[,-length(names(df_save))]
write.table(x = df_save,file = 'Shannon_stat.xls',sep = '\t',quote = F,row.names = F,col.names = T)




