rm(list=ls())
setwd('D:/desk/XMSH_202306_4587/2.alpha')
abd_file <- 'D:/desk/XMSH_202306_4587/Abundance_Stat.filter.anno.xls'
library(iNEXT)
G <- 'D:/desk/XMSH_202306_4587/group.txt'
Group <- read.table(G, sep = '\t', comment.char = "", header = TRUE, quote = "")
df <- read.table(abd_file, sep = '\t', quote = "", comment.char = "",header = TRUE)
df <- df[,-c(8,9)]
names(df)[1] <- "Kingdom"
df <- data.frame(df[,1:7], df[,Group$sample])

# 计算每个样本的 Observed 指数
observed_list <- list()
for(s in Group$sample){
  num <- length(which(0 != df[[s]]))
  observed_list[[s]] = num
}

observed_df <- t(as.data.frame(observed_list))
colnames(observed_df)[1] <- 'Observed'

# 计算每个样本的 ACE 指数

df_ace <- df[,-(1:6)]
rownames(df_ace) <- df_ace$Specie
df_ace <- df_ace[,-1] * 10000# t(df_ace[,-1])
df_ace <- ceiling(df_ace)
library(vegan)
ace <- t(
  data.frame(
    estimateR(
      t(sapply(df_ace, as.integer))
    ),
    check.names = FALSE
  )
)[, 4, drop = FALSE]
# 计算每个样本的 Evenness
shannoon <- function(sample){
  H <- 0
  v <- sample[sample != 0]
  H_max <- log(length(v))
  for(i in v){
    H <- H + i*log(i)
  }
  eve <- -H/H_max
  return(eve)
}
Evenness <- c()
for(s in Group$sample){
  Evenness <- c(Evenness,shannoon(df[[s]]))
}
# 汇总

out_df <- data.frame(Sample = Group$sample, Observed = observed_df, ACE = ace, Evenness = Evenness)
colnames(out_df)[3] <- 'ACE'

write.table(out_df, file = 'Alpha_diversity_Index_supplement.xls',quote = F, sep = '\t',col.names = T,row.names = F)
