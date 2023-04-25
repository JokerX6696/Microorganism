rm(list=ls())
setwd('D:\\desk\\DZQD2023040574-b1\\PA\\fig3')
library(reshape2)
library(pheatmap)
file <- 'genus_species_correlation.xls'
df <- read.table(file,header = TRUE, sep = '\t')

df_c <- df[,-3]
df_pvalue <- df[,-4]
df_c <- dcast(data = df_c, formula =  Category1 ~ Category2 )
df_pvalue <- dcast(data = df_pvalue, formula =  Category1 ~ Category2 )
rownames(df_c) <- df_c$Category1
rownames(df_pvalue) <- df_c$Category1
df_c <- df_c[,-1]
df_pvalue <- df_pvalue[,-1]
p <- pheatmap(df_c)
dev.off()
df_p <- df_c[p$tree_row$order,p$tree_col$order,]
df_pvalue <- df_pvalue[p$tree_row$order,p$tree_col$order,]

s = c()
for(w in as.matrix(df_pvalue)){
  #print(i)
  i <- as.numeric(w)
  if(i <= 0.001){
    s <- c(s,"***")
  }else if(i <= 0.01){
    s <- c(s,"**")
  }else if(i <= 0.05){
    s <- c(s,"*")
  }else{
    s <- c(s," ")
  }
}
s <- matrix(s,nrow = 30)

prefix <- sub("_correlation.xls", "",file)

pdf(file=paste(prefix,'_corheatmap.pdf',sep = ""),width = 9,height = 6)
pheatmap(df_p, cluster_rows = FALSE, cluster_cols = FALSE,angle_col = 90,display_numbers = s, main = "Correlation Heatmap")
dev.off()

png(file=paste(prefix,'_corheatmap.png',sep = ""),width = 900,height = 600)
pheatmap(df_p, cluster_rows = FALSE, cluster_cols = FALSE,angle_col = 90,display_numbers = s, main = "Correlation Heatmap")
dev.off()
