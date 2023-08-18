## 设置环境
package_names <- c('ggplot2','reshape2','optparse')
for (pkg in package_names) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}
####  传参

option_list <- list(
  make_option(c("-i","--input"),type="character",help="统计信息熵所使用到的文件,例如丰度矩阵 species.xls 或其他类似格式的文件!"),
  make_option(c("-o","--output"),type="character",help="输出文件名,可使用相对路径或绝对路径.",default="Shannon"),
  make_option(c("-s","--seed"),type="character",help="取样过程中seed 使用到的 seed, 使用 ',' 分割,例如 '1,2,3,4,5,6',推荐使用默认,数字的数量代表取样的数量. 数字越多,运行速度越慢 boxplot 点越多.每次输入seed 不一致,会导致结果略微不同!",default="79,69,1,3,1,1")
)
opt <- parse_args(OptionParser(option_list=option_list))
opt_parser <- OptionParser(option_list = option_list)
####  para
if (is.null(opt$i)) {
  print('请输入必要参数!')
  q()
}
seed_set <- strsplit(opt$s,',')[[1]]
abd_file <- opt$i
output_name <- opt$o
## 定义香侬计算函数  给定一个丰度向量,返回一个香侬值
Shannon <- function(prop){
  entropy <- -sum(prop * log2(prop), na.rm = TRUE)  # 计算香农熵
  return(entropy)
}

############ 
abd_df <- read.table(abd_file, sep = '\t', header = TRUE, row.names = 1)
samples_all <- names(abd_df)
############
plot_df <- data.frame(select_num=1:length(samples_all))

for (sd in seed_set) {
  set.seed(sd)
  all_shannon <- c()
  for(num in 1:length(samples_all)) {
    select_sample <- sample(samples_all,size = num,replace = FALSE) # 获取随机的样本
    abd_df_temp <- data.frame(abd_df[,select_sample])  # 根据指定样本获得子矩阵
    avg_abd <- apply(abd_df_temp,1,mean)
    all_shannon <- c(all_shannon,Shannon(avg_abd))
  }
  plot_df <- cbind(plot_df,all_shannon)
}
names(plot_df) <- c('select_num',seq(1:length(seed_set)))

plot_df <- melt(plot_df,id.vars = 'select_num')
plot_df <- plot_df[,c(1,3)]
names(plot_df) <- c('num','Shannon')
x_lim <- ceiling(length(unique(plot_df$num))/10 ) * 10
# 标准差
sd_vector <- c()
avg_vector <- c()
for (i in unique(plot_df$num)) {
  sd_vector <- c(sd_vector,sd(plot_df[plot_df$num==i,'Shannon']))
  avg_vector <- c(avg_vector, mean(plot_df[plot_df$num==i,'Shannon']))
}

area <- data.frame(num=unique(plot_df$num),y_top = avg_vector + sd_vector/2,y_bot=avg_vector - sd_vector/2)
# 原始图像绘制
p <- ggplot(data = plot_df,mapping = aes(x=factor(num),y=Shannon,fill='yellow')) + 
  stat_boxplot(aes(ymin=..lower..,ymax=..upper..),size = 0.5)  + # 绘制箱子整体 使用实线主题覆盖虚线
  stat_boxplot(geom = "errorbar",aes(ymin=..ymax..),color="black") +  # 添加上方最大值横线
  stat_boxplot(geom = "errorbar",aes(ymax=..ymin..),color="black") +  # 添加下方最小值横线
  geom_boxplot() + 
  scale_fill_manual(values = '#FFFF00') + 
  guides(fill=FALSE) +  # 去除图例
  xlab('Number of Samples Sequenced') + 
  theme_bw() +  # 主题
  theme(panel.grid =element_blank()) +    # 删掉网格线
  scale_x_discrete(breaks = seq(0, x_lim, by = 10)) 


ggsave(filename = paste0(output_name,'.pdf'),plot = p,device = 'pdf',width = 9,height = 8)
ggsave(filename = paste0(output_name,'.png'),plot = p,device = 'png',width = 9,height = 8)
 
# p_ribbon <- p + geom_ribbon(data=area,mapping = aes(x=num,ymin=y_bot,ymax=y_top),fill = "blue", alpha = 0.5)
# grid.arrange(p, p_ribbon, ncol = 2)
# 
# 
# ggplot() + 
#   geom_ribbon(data=area,mapping = aes(x=num,ymin=y_bot,ymax=y_top),fill = "blue", alpha = 0.5)

# p2 <- ggplot() + 
#   stat_boxplot(aes(ymin=..lower..,ymax=..upper..),size = 0.5)  + # 绘制箱子整体 使用实线主题覆盖虚线
#   stat_boxplot(geom = "errorbar",aes(ymin=..ymax..),color="black") +  # 添加上方最大值横线
#   stat_boxplot(geom = "errorbar",aes(ymax=..ymin..),color="black") +  # 添加下方最小值横线
#   geom_boxplot(data = plot_df,mapping = aes(x=factor(num),y=Shannon,fill='yellow')) + 
#   scale_fill_manual(values = '#FFFF00') + 
#   guides(fill=FALSE) +  # 去除图例
#   xlab('Number of Samples Sequenced') + 
#   theme_bw() +  # 主题
#   theme(panel.grid =element_blank()) +    # 删掉网格线
#   scale_x_discrete(breaks = seq(0, x_lim, by = 10)) +
#   geom_ribbon(data=area,mapping = aes(x=num,ymin=y_bot,ymax=y_top),fill = "blue", alpha = 0.5)
