rm(list=ls())
setwd('D:/desk/XMSH_202311_7630/2.heatmap')
library('pheatmap')
############## para
ff <- 'species.kruskal_wallis.diff.heatmap.xls'
out <-  strsplit(ff,split = '\\.')[[1]][1]
####################
f <- read.table(ff,sep = '\t',header = T,row.names = 1)

an <- factor(c(rep('Day_0',18),rep('Day_28',18),rep('Control',16)),levels = c('Day_0','Day_28','Control'))

# 构建行注释信息
annotation_row = data.frame(
  Group = factor(an)
)
rownames(annotation_row) <- colnames(f)
#
ann_colors = list(
  Group =c('Day_0' = "#7570B3",'Day_28' = '#E7298A','Control' = '#66A61E')
)
#######################
pdf(file = paste0(out,'_kruskal_wallis_heatmap.pdf'),width = 9,height = 6)
pheatmap(
  mat = f,
  border=FALSE,
  cluster_rows = T,
  treeheight_row = 0,
  cluster_cols = FALSE,
  color = colorRampPalette(c("navy", "white", "firebrick3"))(50),
  annotation_row = annotation_row,
  annotation_col = annotation_row,
  annotation_colors = ann_colors,
  fontsize_row = 6,
  fontsize_col = 6,
  scale = "row"
  )
dev.off()




