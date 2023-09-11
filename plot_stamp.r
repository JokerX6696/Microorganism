rm(list=ls())
setwd('D:/desk/XMSH_202309_6088')
library('reshape2')
library('ggplot2')
####### para
group_f <- 'mapping.txt'
f <- 'KEGG_L3.Kruskal_Wallis.xls'
list_od <- c('EtOH_1','EtOH_2','EtOH_3','con_4')

########## 处理分组文件
group <- read.table(group_f)
group <- group[,c(1,4)]
names(group) <- c('Sample','Group')
group$Group <- gsub("-","_",group$Group)
########### data df
data <- read.table(f,sep = '\t',header = TRUE, row.names = 1,quote = "")

i <- 'EtOH_1'

selcet_sample_EtOH_4 <- group$Sample[group$Group == 'EtOH_4']

selcet_sample <- group$Sample[group$Group == i]
rownames(data) <- gsub("\"","",rownames(data))
#rownames(data) <- gsub("\\s","_",rownames(data))
##########
df_x <- data[,selcet_sample]
df_y <- data[,selcet_sample_EtOH_4]
# p1 
plot_df1 <- data.frame(apply(df_x,1,mean),apply(df_y,1,mean))
names(plot_df1) <- c(i,'EtOH_4')
p_vet <- c()
for (k in rownames(plot_df1)) {
  ret_p <- wilcox.test(as.numeric(df_x[k,]),as.numeric(df_y[k,]))
  ret_p <- ret_p$p.value
  p_vet <- c(p_vet,ret_p)
}
plot_df1$p_value <- p_vet
plot_df1 <- plot_df1[order(plot_df1$p_value),]
plot_df1 <- plot_df1[1:30,c(1:2)]
plot_df1$L3 <- rownames(plot_df1)
plot_df <- melt(plot_df1,id.vars = 'L3')
names(plot_df) <- c('L3','Group','value')
plot_df$Group <- as.factor(plot_df$Group)
plot_df$L3 <- as.factor(plot_df$L3)
p <- ggplot(plot_df,mapping=aes(x=L3,y=value,fill = Group),) +
  scale_x_discrete(limits = levels(plot_df$L3)) + # 不可缺少 确定范围
  coord_flip() +
  xlab("") +
  ylab("") +
  theme(panel.background = element_rect(fill = 'transparent'),
        panel.grid = element_blank(),
        axis.ticks.length = unit(0.4,"lines"),
        axis.ticks = element_line(color='black'),
        axis.line = element_line(colour = "black"),
        axis.title.x=element_text(colour='black', size=12,face = "bold"),
        axis.text=element_text(colour='black',size=10,face = "bold"),
        legend.title=element_blank(),
        legend.text=element_text(size=12,face = "bold",colour = "black",
                                 margin = margin(r = 20)),
        legend.position = c(0.8,0.9),
        legend.direction = "horizontal",
        legend.key.width = unit(0.8,"cm"),
        legend.key = element_rect(fill = "transparent"),
        #legend.key.height = unit(0.5,"cm"))for (i in 1:(30 - 1))
        ) 
  
  
for (i in 1:29) {
  p <- p + annotate('rect', xmin = i+0.5, xmax = i+1.5, ymin = 0, ymax = Inf,
           fill = ifelse(i %% 2 == 0, 'white', 'gray95'))
}
  
p1 <- p + geom_bar(stat = "identity",position = position_dodge(1)) + 
  scale_fill_manual(values = c("#00468BFF","#ED0000FF"))
########### p2


