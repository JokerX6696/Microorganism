rm(list=ls())
setwd('D:/desk/XMSH_202307_5066')
library(ggplot2)
library(reshape2)
# 读取 kegg db 文件
db_file <- 'KEGGpathway_three_levels_v2.xls'
df_db <- read.table(db_file, header = FALSE, sep = '\t', quote = "", comment.char = "")
names(df_db) <- c("ID","L1","L2","L3")
g <- c('EtOH-1__EtOH-4__Con-1__Con-4',)
for (file_g in vector) {
  
}

outname = 'test.pdf'


# 读取结果文件
file <- "KEGG_L2.Kruskal_Wallis.diff.heatmap.xls"
df <- read.table(file, header = TRUE, sep = '\t', quote = "", comment.char = "")
# 处理原始矩阵
rownames(df) <- df$Index
L2 <- df$Index
df <- df[,-1]
# 每个样本均一化 为 100
for (i in colnames(df)) {
  all <- sum(df[,i])
  df[,i] <- df[,i] / all * 100
}
# 将表格整理成输入文件
df <- data.frame(L2 = L2,Abundance = apply(df, 1, mean))

L1 <- c()
for (i in df$L2) {
  L1_temp = df_db[which(df_db$L2 == i)[1],]["L1"]
  L1 <- c(L1, L1_temp)
}

df$L1 <- L1

KEGG <- df


swr = function(string, nwrap = 12){
  paste(strwrap(string,width = nwrap),collapse = "\n")
}
swr = Vectorize(swr)
KEGG$L1 <- swr(KEGG$L1)

p <- ggplot(KEGG,aes(Abundance,L2)) +
  geom_bar(aes(fill = L1),stat = "identity",width = 0.6,show.legend = F) +
  xlab("Relative abundance (%)") +
  ylab("KEGG Pathway") +
  facet_grid(L1~.,space = "free_y",scales = "free_y") +
  theme(panel.background = element_rect(fill = "white",colour='black'),
        panel.grid.major = element_line(color = "grey",linetype = "dotted",size = 0.3),
        panel.grid.minor = element_line(color = "grey",linetype = "dotted",size = 0.3),
        axis.ticks.length = unit(0.4,"lines"),
        axis.ticks = element_line(color='black'),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_text(colour='black', size=12,face = "bold"),
        axis.title.y=element_text(colour='black', size=12),
        axis.text.x=element_text(colour='black',size=8),
        strip.text.y = element_text(angle = 0, hjust = 0.5,size = 8)
        )
  
ggsave(filename = outname,plot = p,device = 'pdf',width = 6,height = 9)
        