rm(list=ls())
library(igraph)
library(psych)
library(dplyr)
library(pheatmap)
library(reshape2)
################################
DX <- readxl::read_xlsx('差异代谢物.xlsx',sheet = 'ABX_SPF_GF')
DX[DX$`P-value` < 0.05,]

namedx <- DX$Metabolites
DX = data.frame(DX[,grepl("^T.*",names(DX))])
row.names(DX) <- namedx

DX <- t(DX)
DX_top_100 <- DX[, 1:100]
################################
diff_ab <- read.table('species.ANOVA.Group.xls', sep='\t', header = T)
diff_ab <- diff_ab[diff_ab$P<0.05,]

diff_list <- diff_ab$species

ABU <- read.table('Abundance_Stat.filter.anno.xls', sep='\t', header = T,quote = "",encoding = 'UTF-8')
names(ABU) <- sub("M","",names(ABU))

ABU <- ABU[ABU$Specie %in% diff_list,]
rownames(ABU) <- ABU$Specie;ABU <- ABU[,-1]
ABU <- t(ABU)
#############################################################
# top 100

corr <- corr.test(x = ABU, y = DX_top_100, method = "spearman")

corr.r <- corr$r
corr.r.write <- cbind(rownames(corr.r), corr.r)
write.table(corr.r.write, file = "Top_100_correlate_value.xls", sep = "\t",
            row.names = FALSE)
matr <- melt(corr.r)

corr.p <- corr$p
corr.p.write <- t(cbind(rownames(corr.p), corr.p))
write.table(corr.p.write, file = "Top_100_p_value.xls", sep = "\t",
            col.names = FALSE)
matp <- melt(corr.p)
########################################################################
cor_ret <- data.frame(matr,matp$value)
colnames(cor_ret) <- c("category1","category2","Correlation","Pvalue")
cor_ret$Type <- rep("Group",length(cor_ret$Pvalue))
s <- c()
for(i in cor_ret$Pvalue){
  if(i <= 0.05){
    s <- c(s,"*")
  }else if(i <= 0.01){
    s <- c(s,"**")
  }else if(i <= 0.001){
    s <- c(s,"*")
  }else{
    s <- c(s," ")
  }
}
cor_ret$Significance <- s
padj <- melt(corr$p.adj)
cor_ret$AdjPvalue <- padj$value
cor_ret <- data.frame(category1=cor_ret$category2,category2=cor_ret$category1,cor_ret[3:7])
write.table(cor_ret,file = "Top_100-Group-species-Correlation.xls",sep = '\t',quote = F,row.names = F)
############################################################################
s = c()
for(i in as.matrix(corr.p)){
  if(i <= 0.05){
    s <- c(s,"*")
  }else if(i <= 0.01){
    s <- c(s,"**")
  }else if(i <= 0.001){
    s <- c(s,"*")
  }else{
    s <- c(s," ")
  }
}
s <- t(matrix(s,nrow = 5))
display_numbers = s
png(file = 'Top_100_corr.png',width = 900,height = 600)
pheatmap(t(corr.r),
         cluster_rows = T,
         display_numbers = s,
         cellwidth = 15,
         cluster_cols = T,
         angle_col = 45,
         color = colorRampPalette(c("navy", "white", "firebrick3"))(50),
         fontsize_row = 4,
         fontsize_col = 5,
         border=FALSE
         )
dev.off()

pdf(file = 'Top_100_corr.pdf',width = 9,height = 6)
pheatmap(t(corr.r),
         cluster_rows = T,
         display_numbers = s,
         cellwidth = 15,
         cluster_cols = T,
         angle_col = 45,
         color = colorRampPalette(c("navy", "white", "firebrick3"))(50),
         fontsize_row = 4,
         fontsize_col = 5,
         border=FALSE
         )
dev.off()
#############################################################
# all
corr <- corr.test(x = ABU, y = DX, method = "spearman")

corr.r <- corr$r
corr.r.write <- cbind(rownames(corr.r), corr.r)
write.table(corr.r.write, file = "All_correlate_value.xls", sep = "\t",
            row.names = FALSE)
matr <- melt(corr.r)

corr.p <- corr$p
corr.p.write <- t(cbind(rownames(corr.p), corr.p))
write.table(corr.p.write, file = "All_p_value.xls", sep = "\t",
            col.names = FALSE)
matp <- melt(corr.p)
########################################################################
cor_ret <- data.frame(matr,matp$value)
colnames(cor_ret) <- c("category1","category2","Correlation","Pvalue")
cor_ret$Type <- rep("Group",length(cor_ret$Pvalue))
s <- c()
for(i in cor_ret$Pvalue){
  if(i <= 0.05){
    s <- c(s,"*")
  }else if(i <= 0.01){
    s <- c(s,"**")
  }else if(i <= 0.001){
    s <- c(s,"*")
  }else{
    s <- c(s," ")
  }
}
cor_ret$Significance <- s
padj <- melt(corr$p.adj)
cor_ret$AdjPvalue <- padj$value
cor_ret <- data.frame(category1=cor_ret$category2,category2=cor_ret$category1,cor_ret[3:7])
write.table(cor_ret,file = "All-Group-species-Correlation.xls",sep = '\t',quote = F,row.names = F)
############################################################################
display_numbers = s
png(file = 'All_corr.png',width = 900,height = 600)
pheatmap(t(corr.r),
         show_rownames = F,
         cluster_rows = T,
         #display_numbers = s,
         cellwidth = 15,
         cluster_cols = T,
         angle_col = 45,
         color = colorRampPalette(c("navy", "white", "firebrick3"))(50),
         fontsize_row = 1,
         fontsize_col = 5,
         border=FALSE
)
dev.off()

pdf(file = 'All_corr.pdf',width = 9,height = 6)
pheatmap(t(corr.r),
         show_rownames = F,
         cluster_rows = T,
         #display_numbers = s,
         cellwidth = 15,
         cluster_cols = T,
         angle_col = 45,
         color = colorRampPalette(c("navy", "white", "firebrick3"))(50),
         fontsize_row = 1,
         fontsize_col = 5,
         border=FALSE
)
dev.off()
