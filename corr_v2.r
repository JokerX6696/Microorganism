rm(list=ls())
setwd('D:/desk/XMSH_202307_5383')
library(psych)
library(pheatmap)
library(reshape2)
##para
l <- 'ANOVA' # kruskal_wallis             ANOVA 
# 处理蛋白矩阵
pro_file <- './proteomics0724OE1.xlsx'
pro_df <- readxl::read_xlsx(pro_file)
names(pro_df) <- gsub('-','_',names(pro_df))
pro_df <- data.frame(pro_df)
rownames(pro_df) <- gsub('-','_',pro_df$Gene)
pro_df <- t(pro_df[,-1])
# 差异物种
diff_file <- paste0('./species.',l,'.Group.xls')
diff_df <- read.table(diff_file,sep = '\t',header = TRUE)
diff_df <- diff_df[diff_df$P<0.05,]
diff_species <- gsub('-','_',diff_df$species)
# 处理微生物矩阵
mic_file <- './species.xls'
mic_df <- read.table(mic_file,sep = '\t',header = TRUE)
s <- rownames(pro_df)
mic_df <- data.frame(Taxonomy=mic_df$Taxonomy, mic_df[,s])
rownames(mic_df) <- gsub('-','_',mic_df$Taxonomy)
mic_df <- mic_df[diff_species,]
mic_df <- t(mic_df[,-1])

# 计算相关性
corr_cls <- corr.test(x=mic_df,y = pro_df,method = 'spearman',use = "pairwise",adjust = "fdr", alpha = .05)
# 删除包含NA的行
df_corr <- na.omit(as.data.frame(corr_cls$r))
df_p <- na.omit(as.data.frame(corr_cls$p))
# 可视化                       dev.off()
pdf(file = paste0(l,'_heatmap.pdf'),width = 12,height = 8)
pheatmap(
  mat = df_corr,
  color = colorRampPalette(c("navy", "white", "firebrick3"))(50),
  border = FALSE,
  fontsize_row = 2,
  fontsize_col = 3,
  angle_col = 45
         )
dev.off()

# 定义输出文件
df_corr <- data.frame(rownames(df_corr),df_corr)
names(df_corr)[1] = ""
write.table(x=df_corr,file = paste0(l,'_corr.xls'),sep = '\t',quote = F,col.names = TRUE,row.names = F)
  
df_p <- data.frame(rownames(df_p),df_p)
names(df_p)[1] = ""
write.table(x=df_p,file = paste0(l,'_Pvalue.xls'),sep = '\t',quote = F,col.names = TRUE,row.names = F)
  



AdjPvalue <- melt(corr_cls$p.adj)
Correlation <- melt(corr_cls$r)
Pvalue <- melt(corr_cls$p)

fin_df <- merge(x=Correlation,y=Pvalue,by = names(Correlation)[1:2],all.x = TRUE)
names(fin_df) <- c('Microorganism','Protein','Correlation','Pvalue')
fin_df <- merge(x=fin_df,y=AdjPvalue,by.x = names(fin_df)[1:2],by.y = names(AdjPvalue)[1:2],all.x = TRUE)
names(fin_df) <- 'AdjPvalue'
fin_df <- na.omit(fin_df)
write.table(x=fin_df,file = paste0(l,'_Pvalue.xls'),sep = '\t',quote = F,col.names = TRUE,row.names = F)
