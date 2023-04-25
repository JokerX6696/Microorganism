rm(list=ls())
setwd('D:/desk/DZQD2023040574-b1/20221118_QD2021-19862_韦超老师的12个小鼠角膜组织2bRAD-M微生物菌群检测项目_第2次个性化_结题报告/report/fig3')
library(reshape2)
library(pheatmap)
file <- 'genus_species_correlation.xls'
df <- read.table(file,header = TRUE, sep = '\t')

df_c <- df[,-4]
df_c <- dcast(data = df, formula =  Category1 ~ Category2 )

rownames(df_c) <- df_c$Category1

df_c <- df_c[,-1]

p <- pheatmap(df_c)
dev.off()
df_p <- df_c[p$tree_row$order,p$tree_col$order,]


s = c()
for(i in as.matrix(df_p)){
  print(i)
  if(i >= 0.9){
    s <- c(s,"***")
  }else if(i >= 0.8){
    s <- c(s,"**")
  }else if(i >= 0.6){
    s <- c(s,"*")
  }else{
    s <- c(s," ")
  }
}
s <- matrix(s,nrow = 30)

prefix <- sub("_correlation.xls", "",file)

pdf(file=paste(prefix,'_corheatmap.pdf',sep = ""),width = 9,height = 6)
pheatmap(df_p, cluster_rows = FALSE, cluster_cols = FALSE,angle_col = 90,display_numbers = s)
dev.off()

png(file=paste(prefix,'_corheatmap.png',sep = ""),width = 900,height = 600)
pheatmap(df_p, cluster_rows = FALSE, cluster_cols = FALSE,angle_col = 90,display_numbers = s)
dev.off()
