rm(list=ls())
setwd('D:/desk/XMSH_202307_5180')
library(ggplot2)
library(reshape2)
# 读取 kegg db 文件
db_file <- 'KEGGpathway_three_levels_v2.xls'
df_db <- read.table(db_file, header = FALSE, sep = '\t', quote = "", comment.char = "")
names(df_db) <- c("ID","L1","L2","L3")


# 读取结果文件
file <- "KEGG_L3.Kruskal_Wallis.xls"
df_raw <- read.table(file, header = TRUE, sep = '\t', quote = "", comment.char = "")
# 处理原始矩阵
rownames(df_raw) <- df_raw$Index
L <- df_raw$Index
df_raw <- df_raw[,-1]
fin <- dim(df_raw)[2]
df_raw <- df_raw[,-c(fin,fin-1,fin-2)]
df_raw <- df_raw[,-which(c("CE2_5","CE8_2")==names(df_raw))]
# 每个样本均一化 为 100
for (i in colnames(df_raw)) {
  all <- sum(df_raw[,i])
  df_raw[,i] <- df_raw[,i] / all * 100
}
# 分组文件处理
group_file <- "group.txt"
group <- read.table(group_file, header = TRUE, sep = '\t', quote = "", comment.char = "")
group$samples <- sub("CE2_5,|CE8_2,","",group$samples)

group_list <- c('EtOH-1__EtOH-4',
                'EtOH-2__EtOH-4',
                'EtOH-3__EtOH-4',
                'EtOH-4__Con-4'
                )
group_list <- gsub("-","_",group_list,perl = T)
#####################
q = group_list[1]
################

df <- df_raw
outname = paste0(q,'.pdf')
all_group <- strsplit(q,"__")[[1]]
A <- list(
  group1=strsplit(group[match(all_group[1] ,group$group),'samples'],',')[[1]],
  group2=strsplit(group[match(all_group[2] ,group$group),'samples'],',')[[1]]
  )

# 将表格整理成输入文件
df1 <- df[,A$group1]
df2 <- df[,A$group2]
df <- data.frame(L = L,Abundance1 = apply(df1, 1, mean),Abundance2 = apply(df2, 1, mean))

L1 <- c()
for (i in df$L) {
  L1_temp = df_db[which(df_db$L3 == i)[1],][["L1"]]
  #print(L1_temp)
  L1 <- c(L1, L1_temp)
}

df$L1 <- L1

L2 <- c()
for (i in df$L) {
  L2_temp = df_db[which(df_db$L3 == i)[1],][["L2"]]
  L2 <- c(L2, L2_temp)
}

df$L2 <- L2

df <- data.frame(L1=df$L1,L2=df$L2,L3=df$L,Abundance1=df$Abundance1,Abundance2=df$Abundance2)
KEGG <- df
################# 差异 top 15
KEGG <- KEGG[rev(order(abs(KEGG$Abundance1 - KEGG$Abundance2))),]
KEGG <- KEGG[1:15,] #
swr = function(string, nwrap = 12){
  paste(strwrap(string,width = nwrap),collapse = "\n")
}
swr = Vectorize(swr)
KEGG$L1 <- swr(KEGG$L1)
p <- ggplot(KEGG,aes(Abundance,L3)) +
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
ggsave(filename = sub("pdf","png",outname),plot = p,device = 'png',width = 6,height = 9)    
KEGG$L1 <- gsub("\\n","",KEGG$L1)
KEGG$L2 <- gsub("\\n","",KEGG$L2)
KEGG$L3 <- gsub("\\n","",KEGG$L3)
KEGG$Abundance1 <- gsub("\\n","",KEGG$Abundance1)
KEGG$Abundance2 <- gsub("\\n","",KEGG$Abundance2)
write.table(KEGG,file = sub("pdf","xls",outname),quote = F, col.names = T,row.names = F,sep = '\t')

