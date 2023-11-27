rm(list=ls())
setwd('D:/desk/XMSH_202311_7957/')
library('ggplot2')
library('reshape2')
###   para
g <- 'Group_map.txt'
lev <- c('family','genus','order','phylum','class','species')
type_p <- 'refseq'
lev_dir <- 'refseq/20231108_DZOE2023090692_马颖_周子薇_2bRAD-M_结题报告/result/2.Community_Structure/abundance/relative_abundance'
mycol <- c(
  "#7FC97F", "#BEAED4", "#FDC086", "#386CB0", "#F0027F", "#BF5B17",
  "#666666", "#1B9E77", "#7570B3", "#66A61E", "#E6AB02", "#A6761D",
  "#A6CEE3", "#B2DF8A", "#FB9A99", "#E31A1C", "#FF7F00", "#6A3D9A",
  "#8DA0CB", "#4DAF4A", "#984EA3", "#c6c386", "#999999", "#66C2A5",
  "#FC8D62", "#A6D854", "#FFD92F", "#BEBADA", "#FB8072", "#80B1D3",
  "#FDB462", "#BC80BD", "#B3B3B3", "#33A02C", "#B3DE69", "#4038b0",
  "#ee7576", "#e94749", "#E78AC3", "#ff0000", "#A65628", "#d80172",
  "#F781BF", "#D95F02", "#E7298A", "#1F78B4", "#FDBF6F", "#CAB2D6",
  "#B15928", "#FBB4AE", "#B3CDE3", "#0173b2", "#de8f05", "#029e73",
  "#d55e00", "#cc78bc", "#ca9161", "#fbafe4", "#949494", "#ece133",
  "#56b4e9", "#00AFBB", "#E7B800", "#FC4E07", "#FFDB6D", "#C4961A",
  "#F4EDCA", "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352"
)
############ 定义函数
plot_zdy <- function(data,methods,biomk,lev,pvalue,col){
  p <- ggplot(data = data,mapping = aes(x=group,y=biomk,fill=group)) + 
    geom_boxplot() + 
    theme_bw() + 
    labs(y=biomk) + 
    scale_fill_manual(values = mycol) + 
    ggtitle(paste0(methods," ",'pvalue = ',round(pvalue, digits = 5)))+
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold",size = 20)  # 居中且加粗
    )
  if (!dir.exists(paste(col,methods,lev,sep='/'))) {
    # 如果不存在，创建目录
    dir.create(paste(col,methods,lev,sep='/'),recursive = TRUE)
  }
  out <- paste0(col,'/',methods,'/',lev,'/',biomk,'.pdf')
  ggsave(filename = out,plot = p,device = 'pdf',width = 9,height = 6)
}
anova_plot <- function(){
  # 将丰度文件处理成检验函数需要的样子 # anova
  for(e in lev){
    abd <- read.table(file = (paste0(lev_dir,'/',e,'.xls')),header = T,row.names = 1)
    abd <- abd[,temp_df$sample]
    # 每次循环检验一个物种
    for (mk in rownames(abd)) {
      abd_anova <- as.data.frame(t(abd[mk,]))
      abd_anova_group <- c()
      for (q in rownames(abd_anova)) {
        w <- temp_df[temp_df$sample == q,2]
        abd_anova_group <- c(abd_anova_group,w)
      }
      abd_anova$group <- abd_anova_group
      biomk <- names(abd_anova)[1]
      names(abd_anova)[1] <-  'biomk'
      anova_result <- aov(biomk ~ group , data = abd_anova)
      result_summary <- summary(anova_result)
      pvalue <- result_summary[[1]]$`Pr(>F)`[1]
      if(is.na(pvalue)){next}
      if(pvalue < 0.05){
        plot_zdy(data = abd_anova,methods='ANOVA',biomk=biomk,lev=e,pvalue=pvalue,col=col)
      }
      
    }
  }
  
}
kruskal_plot <- function(){
  # 将丰度文件处理成检验函数需要的样子 # anova
  for(e in lev){
    abd <- read.table(file = (paste0(lev_dir,'/',e,'.xls')),header = T,row.names = 1)
    abd <- abd[,temp_df$sample]
    # 每次循环检验一个物种
    for (mk in rownames(abd)) {
      abd_anova <- as.data.frame(t(abd[mk,]))
      abd_anova_group <- c()
      for (q in rownames(abd_anova)) {
        w <- temp_df[temp_df$sample == q,2]
        abd_anova_group <- c(abd_anova_group,w)
      }
      abd_anova$group <- abd_anova_group
      biomk <- names(abd_anova)[1]
      names(abd_anova)[1] <-  'biomk'
      anova_result <- kruskal.test(biomk ~ group , data = abd_anova)
      pvalue <- anova_result$p.value
      if(is.na(pvalue)){next}
      if(pvalue < 0.05){
        plot_zdy(data = abd_anova,methods='kruskal-wallis',biomk=biomk,lev=e,pvalue=pvalue,col=col)
      }
      
    }
  }
  
}
test_plot <- function(){
  for(e in lev){
    abd <- read.table(file = (paste0(lev_dir,'/',e,'.xls')),header = T,row.names = 1)
    abd <- abd[,temp_df$sample]
    # 每次循环检验一个物种
    for (mk in rownames(abd)) {
      abd_t_test <- as.data.frame(t(abd[mk,]))
      abd_t_test_group <- c()
      for (q in rownames(abd_t_test)) {
        w <- temp_df[temp_df$sample == q,2]
        abd_t_test_group <- c(abd_t_test_group,w)
      }
      abd_t_test$group <- abd_t_test_group
      biomk <- mk
      names(abd_t_test)[1] <-  'biomk'
      vet_1 = abd_t_test[abd_t_test$group == unique(abd_t_test$group)[1],1]
      vet_2 = abd_t_test[abd_t_test$group == unique(abd_t_test$group)[2],1]
      t_test_result <- t.test(vet_1,vet_2)
      pvalue <- t_test_result$p.value
      if(is.na(pvalue)){next}
      if(pvalue < 0.05){
        plot_zdy(data = abd_t_test,methods='T-test',biomk=biomk,lev=e,pvalue=pvalue,col=col)
      }
      
    }
  }
  
}
wilcox_plot <- function(){
  for(e in lev){
    abd <- read.table(file = (paste0(lev_dir,'/',e,'.xls')),header = T,row.names = 1)
    abd <- abd[,temp_df$sample]
    # 每次循环检验一个物种
    for (mk in rownames(abd)) {
      abd_t_test <- as.data.frame(t(abd[mk,]))
      abd_t_test_group <- c()
      for (q in rownames(abd_t_test)) {
        w <- temp_df[temp_df$sample == q,2]
        abd_t_test_group <- c(abd_t_test_group,w)
      }
      abd_t_test$group <- abd_t_test_group
      biomk <- mk
      names(abd_t_test)[1] <-  'biomk'
      vet_1 = abd_t_test[abd_t_test$group == unique(abd_t_test$group)[1],1]
      vet_2 = abd_t_test[abd_t_test$group == unique(abd_t_test$group)[2],1]
      wilcox_test_result <- wilcox.test(vet_1,vet_2)
      pvalue <- wilcox_test_result$p.value
      if(is.na(pvalue)){next}
      if(pvalue < 0.05){
        plot_zdy(data = abd_t_test,methods='wilcox-test',biomk=biomk,lev=e,pvalue=pvalue,col=col)
      }
      
    }
  }
  
}
#################
# 最外层为每一种方式循环一次
group <- read.table(g,header = T,sep = '\t',comment.char = "",row.names = 1)
for (col in names(group)) {
  temp_df <- data.frame(
    sample=rownames(group)[group[,col] != ""],
    group=group[group[,col] != "",col]
    )
  group_num = length(unique(temp_df$group))
  if(group_num > 2){
    anova_plot()
    kruskal_plot()
  }else{
    test_plot()
    wilcox_plot()
  }
}









