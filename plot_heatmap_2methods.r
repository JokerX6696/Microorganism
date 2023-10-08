rm(list=ls())
setwd('D:/desk/XMSH_202309_6655/heatmap/')
library('pheatmap')
lev <- c('class','family','genus','order','species')
meth <- c('.Wilcoxon','.T_test')
for (lev_temp in lev) {
  for (m in meth) {
    
  }
  
  f <- paste0(lev_temp,m,'.Group.xls')
  # 筛选差异物种
  df <- read.table(f,sep = '\t',header = T)
  df <- df[df$P <= 0.05,]
  
  sel <- df[,1]
  # 根据差异物种 选择丰度表
  abd_f <- paste0(lev_temp,'.xls')
  abd <- read.table(abd_f,sep = '\t',header = T,row.names = 1)
  abd <- abd[sel,]
  # 处理画图数据
  plot_df <- data.frame(abd[20:45],abd[1:19])
  
  # plot 不聚类
  
  w = ncol(plot_df)/3
  h <- nrow(plot_df)/2
  if(ncol(plot_df) < 2){
    s <- paste(lev_temp,m,'rare')
    print(s)
    next
  }
  pdf(file = paste0(lev_temp,m,'.diff.heatmap.pdf'),width = w,height = h)
  pheatmap(
    mat = plot_df,
    color = colorRampPalette(c("navy", "white", "firebrick3"))(50),
    scale = "row",
    border=FALSE,
    cluster_rows = TRUE, 
    cluster_cols = FALSE,
    angle_col = 45
  )
  dev.off()
  
  pdf(file = paste0(lev_temp,m,'.diff.heatmapNocluster.pdf'),width = w,height = h)
  pheatmap(
    mat = plot_df,
    color = colorRampPalette(c("navy", "white", "firebrick3"))(50),
    scale = "row",
    border=FALSE,
    cluster_rows = TRUE, 
    cluster_cols = TRUE,
    angle_col = 45
  )
  dev.off()
}


