rm(list=ls())
setwd('D:/desk/XMSH_202311_7630/3.bar')
df <- read.table('Abundance.filtered.xls',header = T,quote = "",comment.char = "",sep = '\t')
df <- df[,7:59]
sel <- c('Fusobacterium_nucleatum', 'Porphyromonas_gingivalis', '')














