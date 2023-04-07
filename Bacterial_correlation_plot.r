rm(list = ls())
setwd('D:/desk/R_temp/item7')
library(pheatmap)
file <- 'Abundance_Stat.filter.anno.txt'

df <- read.table(file,header = TRUE,sep = '\t',check.names = F,comment.char = "",quote = "")

df <- df[,-c(1:6,8,9)]

df <- t(df)
colnames(df) <- df[1,]
df <- as.data.frame(df[-1,])
# 转化为数值矩阵
df <- as.data.frame(lapply(df,as.numeric))
# 选取细菌 top30 种
species <- read.table('L7.Top30_species.xls',header = T)
species <- species$Taxonomy

df_a <- df[,which(colnames(df) %in% species)]

cor_matirx <- cor(df_a)
s = c()
for(i in as.matrix(cor_matirx)){
  if(i >= 1){
    s <- c(s,"***")
  }else if(i >= 0.9){
    s <- c(s,"**")
  }else if(i >= 0.8){
    s <- c(s,"*")
  }else{
    s <- c(s," ")
  }
}
s <- matrix(s,nrow = 30)

png(file = 'bacteria_top30.png',width = 900,height = 600)
pheatmap(cor_matirx,display_numbers = s,angle_col = 45,color = colorRampPalette(c("navy", "white", "firebrick3"))(50))
dev.off()

pdf(file = 'bacteria_top30.pdf',width = 9,height = 6)
pheatmap(cor_matirx,display_numbers = s,angle_col = 45,color = colorRampPalette(c("navy", "white", "firebrick3"))(50))
dev.off()
# 选取真菌 top10 种
species <- read.table('L7.Top15_species.xls',header = T)
species <- species$Taxonomy[1:10]

df_a <- df[,which(colnames(df) %in% species)]

cor_matirx <- cor(df_a)
s = c()
for(i in as.matrix(cor_matirx)){
  if(i >= 1){
    s <- c(s,"***")
  }else if(i >= 0.9){
    s <- c(s,"**")
  }else if(i >= 0.8){
    s <- c(s,"*")
  }else{
    s <- c(s," ")
  }
}
s <- matrix(s,nrow = 10)

png(file = 'fungus_top10.png',width = 900,height = 600)
pheatmap(cor_matirx,display_numbers = s,angle_col = 45,color = colorRampPalette(c("navy", "white", "firebrick3"))(50))
dev.off()

pdf(file = 'fungus_top10.pdf',width = 9,height = 6)
pheatmap(cor_matirx,display_numbers = s,angle_col = 45,color = colorRampPalette(c("navy", "white", "firebrick3"))(50))
dev.off()
