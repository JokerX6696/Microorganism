rm(list = ls())
library(igraph)
library(reshape2)
library(dplyr)
library(Hmisc)
setwd('D:/desk/XMSH_202306_4587/6.network')
# g1 <- graph( edges=c(1,2, 2,3, 3, 1), n=3, directed=F ) 
# plot(g1)
abd_file <- 'D:/desk/XMSH_202306_4587/Abundance_Stat.filter.anno.xls'
g <- 'group1.txt'
Group <- read.table(g, sep = '\t', comment.char = "", header = FALSE, quote = "")
names(Group) <- c("sample","group")
# 读取丰度总表
df <- read.table(abd_file, sep = '\t', comment.char = "", header = TRUE, quote = "")
# 处理丰度表
df <- df[,-c(8,9)]
names(df)[1] <- "Kingdom"
for (k in unique(Group$group)) {
samples <- Group[Group$group == k,1]
tmp_df <- df[,samples]

rownames(tmp_df) <- df$Specie
gt<- t(tmp_df)
sumcol <- apply(gt,2,sum)
gt <- gt[,rev(order(sumcol))]
gt <- gt[,sumcol != 0]
# if(dim(gt)[2] > 30 ){
#   gt <- gt[,1:30]
# }
corr<- rcorr(gt,type = "spearman")
r.cor<-corr$r
p.cor<-corr$P
p.adj <- p.adjust(p.cor, method= "BH")

r.cor[lower.tri(r.cor,diag = T)] <- NA
p.cor[lower.tri(p.cor,diag = T)] <- NA
rel1<- melt(r.cor,
            
            varnames=c( "source", "target"),
            
            value.name= "correlation",
            
            na.rm = T)

rel2<- melt(p.cor,
            
            varnames=c( "source", "target"),
            
            value.name= "pvalue",
            
            na.rm = T)
rel3<- data.frame(rel1,pvalue=rel2$pvalue)
r.cutoff=0.6
p.cutoff=0.05
filted<- filter(rel3,abs(correlation) >= r.cutoff & pvalue < p.cutoff)



#
# igraph <- graph_from_adjacency_matrix(filted,mode = "undirected",
#                             weighted=TRUE,
#                             diag=FALSE)

names(filted)[3] <- 'cor'
igraph <- graph_from_data_frame(filted, direct=F) 


V(igraph)$size <- 10
# 生成一些随机颜色
cols <- c('#00A6FB', '#0582CA', '#fff0f3', '#006494', '#c9e4ca', '#31572c', '#90a955', '#ecf39e', '#a4133c', '#c9184a', '#ff4d6d')
V(igraph)$color <- sample(cols, length(V(igraph)), replace = T) # 从随机颜色中采样给节点上色
E(igraph)$color[E(igraph)$cor >= 0.6] <- "darkgray" # 正相关则边为深灰色
E(igraph)$color[E(igraph)$cor <= -0.6] <- "red" # 负相关则边为红色
E(igraph)$width <- abs(E(igraph)$cor)*0.5 # 边的粗细与相关系数成正比，进行0.5倍放缩
coords <- layout_with_fr(igraph, niter=9999,grid="nogrid") # 生成布局
out_plot <- paste0(k, "_network")
out_file <- paste0(k, "_network_stat.xls")
 # 保存图为PDF，指定宽和高

if(length(V(igraph)$name) > 100){lb = NA}else{lb = V(igraph)$name}
pdf(file = paste0(out_plot,".pdf"), height = 5, width = 5)
plot(igraph, layout=coords, vertex.label = lb, vertex.frame.color=NA,vertex.label.color='black') # 画图
dev.off()

png(file = paste0(out_plot,".png"), height = 500, width = 500) # 保存图为PDF，指定宽和高
plot(igraph, layout=coords, vertex.label = lb, vertex.frame.color=NA,vertex.label.color='black') # 画图
dev.off()

write.table(filted,file = out_file,sep = '\t',quote = F,col.names = T,row.names = F)

}
