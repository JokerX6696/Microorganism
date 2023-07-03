rm(list=ls())
setwd('D:/desk/XMSH_202306_4587/1.heatmap')
library('pheatmap')
library('dplyr')
abd_file <- 'D:/desk/XMSH_202306_4587/Abundance_Stat.filter.anno.xls'
g <- 'D:/desk/XMSH_202306_4587/group.list'
# 绘图函数
plot_big <- function(df, out_name){
  pdf(out_name, width = 36, height = 3,)
  pheatmap(df,
           border=FALSE,
           cluster_cols = FALSE,
           cluster_rows = FALSE,
           scale = "row",
           color = colorRampPalette(c("navy", "white", "firebrick3"))(50),
           angle_col = 90,
           cellheight = 8,
           fontsize_row = 8,
           fontsize_col = 8
           
  )
  dev.off()
}
plot_small <- function(df, out_name){
  annotation_col = data.frame(Group = Group$group)
  rownames(annotation_col) <- Group$sample
  if(dim(df)[1]==30){h=5}else{h=3}
  pdf(out_name, width = 11, height = h,)
  pheatmap(df,
           border=FALSE,
           cluster_cols = FALSE,
           cluster_rows = FALSE,
           scale = "row",
           color = colorRampPalette(c("navy", "white", "firebrick3"))(50),
           angle_col = 90,
           cellheight = 10,
           cellwidth = 2.8,
           fontsize_row = 8,
           fontsize_col = 8,
           show_colnames = FALSE,
           annotation_col = annotation_col
           
  )
  dev.off()
}
p <- function(df_tmp,lev,arr){
  stR <- paste0("正在绘制 ",lev," 水平图像")
  print(stR)
  col <- which(lev == arr)
  df_tmp <- df[,c(col,8:length(df))]
  # 使用 dplyr 包 按列求和
  sum_df <- df_tmp %>% group_by_at(lev) %>% summarize(across(everything(), sum))
  
  sum_df <- as.data.frame(sum_df)
  rownames(sum_df) <- sum_df[,1]
  sum_df <- sum_df[,-1]
  sum_df$sum <- apply(sum_df, 1, sum)
  sum_df <- sum_df[rev(order(sum_df$sum)),]
  sum_df <- sum_df[,-length(names(sum_df))]
  # 画 top 30
  if(dim(sum_df)[1] > 30){
    p_df <- sum_df[1:30,]
  }else{
    p_df <- sum_df
  }
  top30_name <- paste0('Top30_',lev,"_heatmap.pdf") # Top30_species_with_cluster.pdf
  plot_big(p_df, top30_name)
  top30_name <- paste0('Top30_',lev,"_heatmap_nocolnames.pdf")
  plot_small(p_df, top30_name)
  # 画 top 15
  if(dim(sum_df)[1] > 15){
    p_df <- sum_df[1:15,]
  }else{
    p_df <- sum_df
  }
  top15_name <- paste0('Top15_',lev,"_heatmap.pdf") # Top30_species_with_cluster.pdf
  plot_big(p_df, top15_name)
  top15_name <- paste0('Top15_',lev,"_heatmap_nocolnames.pdf")
  plot_small(p_df, top15_name)
}




# group 处理
Group <- read.table(g, sep = '\t', comment.char = "", header = FALSE, quote = "")
names(Group) <- c("sample","group")
# 读取丰度总表
df <- read.table(abd_file, sep = '\t', comment.char = "", header = TRUE, quote = "")
# 处理丰度表
df <- df[,-c(8,9)]
names(df)[1] <- "Kingdom"
df <- data.frame(df[,1:7], df[,Group$sample])
# 循环画图
names(df)[2:7]
arr <- names(df)[1:7]

for(lev in arr[2:7]){
  p(df,lev,arr)
}







