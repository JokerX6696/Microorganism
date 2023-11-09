rm(list=ls())
setwd('D:/desk/XMSH_202311_7630')
library('pheatmap')
f <- read.table('binary_jaccard_feature_even.txt',sep = '\t',header = T,row.names = 1)

an <- factor(c(rep('Day_0',18),rep('Day_28',18),rep('Control',16)),levels = c('Day_0','Day_28','Control'))

# 构建行注释信息
annotation_row = data.frame(
  Group = factor(an)
)
rownames(annotation_row) <- rownames(f)
#
ann_colors = list(
  Group =c('Day_0' = "#7570B3",'Day_28' = '#E7298A','Control' = '#66A61E')
)
#######################
pdf(file = 'Distance_heatmap.pdf',width = 9,height = 8)
pheatmap(
  mat = f,
  border=FALSE,
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  color = colorRampPalette(c("navy", "white", "firebrick3"))(50),
  annotation_row = annotation_row,
  annotation_col = annotation_row,
  annotation_colors = ann_colors,
  fontsize_row = 6,
  fontsize_col = 6
  )
dev.off()



ann_colors2 = list(
  Time = c("white", "firebrick"),
  CellType = c(CT1 = "#1B9E77", CT2 = "#D95F02"),
  GeneClass = c(Path1 = "#7570B3", Path2 = "#E7298A", Path3 = "#66A61E")
)




