rm(list = ls())
setwd('D:/desk/XMSH_202307_5352')
library('ggplot2')
library('reshape2')
library('RColorBrewer')
file = 'D:/desk/XMSH_202307_5352/alpha/Alpha_diversity_Index_supplement.xls'
### para

g = 'S_GC_SN_GN_SF_GN'
group <- c("S_GC_1","S_GC_2","S_GC_4","S_GC_6","S_GC_7","S_GC_8","S_GC_9","S_GC_10","S_GC_11","S_GC_12","S_GC_13","S_GC_14","S_GC_15","S_GC_16","S_GC_17","S_GC_19","S_GC_20","S_GC_21","S_GC_22","S_GC_23","S_GC_24","S_GC_25","S_GC_26","S_GC_27","S_GC_28","S_GC_29","S_GC_30","S_GC_31","S_GC_32","S_GC_33","S_GC_34","S_GC_35","S_GC_36","S_GC_37","S_GC_38","S_GC_39","S_GC_40","S_GC_41","S_GC_42","S_GC_43","S_GC_44","S_GC_45","S_GC_46","S_GC_47","S_GC_48","SF_GN_1","SF_GN_4","SF_GN_5","SF_GN_6","SF_GN_7","SF_GN_8","SF_GN_9","SF_GN_10","SF_GN_11","SF_GN_13","SF_GN_14","SF_GN_17","SF_GN_18","SF_GN_19","SF_GN_20","SF_GN_21","SF_GN_22","SF_GN_23","SF_GN_24","SF_GN_26","SN_GN_1","SN_GN_2","SN_GN_3","SN_GN_4","SN_GN_5","SN_GN_6","SN_GN_7","SN_GN_8","SN_GN_9","SN_GN_10","SN_GN_11","SN_GN_13","SN_GN_15","SN_GN_16","SN_GN_17","SN_GN_18","SN_GN_19","SN_GN_20","SN_GN_21","SN_GN_24","SN_GN_25","SN_GN_26","SN_GN_27","SN_GN_28","SN_GN_29","SN_GN_30","SN_GN_31","SN_GN_32","SN_GN_33","SN_GN_34","SN_GN_35","SN_GN_36")
#group <- c("S_GC_1","S_GC_2","S_GC_4","S_GC_6","S_GC_7","S_GC_8","S_GC_9","S_GC_10","S_GC_11","S_GC_12","S_GC_13","S_GC_14","S_GC_15","S_GC_16","S_GC_17","S_GC_19","S_GC_20","S_GC_21","S_GC_22","S_GC_23","S_GC_24","S_GC_25","S_GC_26","S_GC_27","S_GC_28","S_GC_29","S_GC_30","S_GC_31","S_GC_32","S_GC_33","S_GC_34","S_GC_35","S_GC_36","S_GC_37","S_GC_38","S_GC_39","S_GC_40","S_GC_41","S_GC_42","S_GC_43","S_GC_44","S_GC_45","S_GC_46","S_GC_47","S_GC_48","SN_GN_1","SN_GN_2","SN_GN_3","SN_GN_4","SN_GN_5","SN_GN_6","SN_GN_7","SN_GN_8","SN_GN_9","SN_GN_10","SN_GN_11","SN_GN_13","SN_GN_15","SN_GN_16","SN_GN_17","SN_GN_18","SN_GN_19","SN_GN_20","SN_GN_21","SN_GN_24","SN_GN_25","SN_GN_26","SN_GN_27","SN_GN_28","SN_GN_29","SN_GN_30","SN_GN_31","SN_GN_32","SN_GN_33","SN_GN_34","SN_GN_35","SN_GN_36")
#group <- c("S_GC_1","S_GC_2","S_GC_4","S_GC_6","S_GC_7","S_GC_8","S_GC_9","S_GC_10","S_GC_11","S_GC_12","S_GC_13","S_GC_14","S_GC_15","S_GC_16","S_GC_17","S_GC_19","S_GC_20","S_GC_21","S_GC_22","S_GC_23","S_GC_24","S_GC_25","S_GC_26","S_GC_27","S_GC_28","S_GC_29","S_GC_30","S_GC_31","S_GC_32","S_GC_33","S_GC_34","S_GC_35","S_GC_36","S_GC_37","S_GC_38","S_GC_39","S_GC_40","S_GC_41","S_GC_42","S_GC_43","S_GC_44","S_GC_45","S_GC_46","S_GC_47","S_GC_48","SF_GN_1","SF_GN_4","SF_GN_5","SF_GN_6","SF_GN_7","SF_GN_8","SF_GN_9","SF_GN_10","SF_GN_11","SF_GN_13","SF_GN_14","SF_GN_17","SF_GN_18","SF_GN_19","SF_GN_20","SF_GN_21","SF_GN_22","SF_GN_23","SF_GN_24","SF_GN_26")
#group <- c("SF_GN_1","SF_GN_4","SF_GN_5","SF_GN_6","SF_GN_7","SF_GN_8","SF_GN_9","SF_GN_10","SF_GN_11","SF_GN_13","SF_GN_14","SF_GN_17","SF_GN_18","SF_GN_19","SF_GN_20","SF_GN_21","SF_GN_22","SF_GN_23","SF_GN_24","SF_GN_26","SN_GN_1","SN_GN_2","SN_GN_3","SN_GN_4","SN_GN_5","SN_GN_6","SN_GN_7","SN_GN_8","SN_GN_9","SN_GN_10","SN_GN_11","SN_GN_13","SN_GN_15","SN_GN_16","SN_GN_17","SN_GN_18","SN_GN_19","SN_GN_20","SN_GN_21","SN_GN_24","SN_GN_25","SN_GN_26","SN_GN_27","SN_GN_28","SN_GN_29","SN_GN_30","SN_GN_31","SN_GN_32","SN_GN_33","SN_GN_34","SN_GN_35","SN_GN_36")
#group <- c("T_GC_1","T_GC_2","T_GC_3","T_GC_4","T_GC_6","T_GC_7","T_GC_8","T_GC_9","T_GC_10","T_GC_11","T_GC_12","T_GC_13","T_GC_14","T_GC_15","T_GC_18","T_GC_19","T_GC_20","T_GC_21","T_GC_22","T_GC_24","T_GC_23","T_GN_1","T_GN_2","T_GN_3","T_GN_4","T_GN_6","T_GN_7","T_GN_8","T_GN_9","T_GN_10","T_GN_11","T_GN_12","T_GN_13","T_GN_14","T_GN_15","T_GN_18","T_GN_19","T_GN_20","T_GN_21","T_GN_22","T_GN_24","T_GN_23")
#########################


df <- read.table(file = file,header = TRUE, sep = '\t',quote = "",row.names = 1)
df <- df[group,]

df$Group <- gsub("_\\d+","",rownames(df))
#### Observed
x <- df[unique(df$Group)[1]==df$Group,1]
y <- df[unique(df$Group)[2]==df$Group,1]
z <- df[unique(df$Group)[3]==df$Group,1]
p <- kruskal.test(list(x,y,z))
p <- round(p$p.value, digits = 4)
P <- ggplot(data = df,mapping = aes(x=Group,y=Observed,fill=Group)) + 
  geom_boxplot(width = 0.6,linetype="dashed") + 
  scale_fill_manual(values = c('#E41A1C','#377EB8','#4DAF4A')) +
  theme_bw(base_line_size = 0) + 
  guides(fill=FALSE) +
  stat_boxplot(aes(ymin=..lower..,ymax=..upper..),size = 1, width = 0.6) +
  stat_boxplot(geom = "errorbar",aes(ymin=..ymax..),width=0.3,color="black") +  # 添加上方最大值横线
  stat_boxplot(geom = "errorbar",aes(ymax=..ymin..),width=0.3,color="black") +  # 添加下方最小值横线
  labs(title = paste0('P=',p)) + 
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(face = "bold",size = 10),
    axis.title = element_text(face = "bold", size = 12),
    title = element_text(face = "bold", size = 14),
    plot.title = element_text(hjust = 0.5)
    )
ggsave(file=paste0(g,'_Observed_Boxplot.png'),plot = P,device = 'png',width = 9,height = 6)
ggsave(file=paste0(g,'_Observed_Boxplot.pdf'),plot = P,device = 'pdf',width = 9,height = 6)

#### ACE
x <- df[unique(df$Group)[1]==df$Group,2]
y <- df[unique(df$Group)[2]==df$Group,2]
z <- df[unique(df$Group)[3]==df$Group,2]
p <- kruskal.test(list(x,y,z))
p <- round(p$p.value, digits = 4)
P <- ggplot(data = df,mapping = aes(x=Group,y=ACE,fill=Group)) + 
  geom_boxplot(width = 0.6,linetype="dashed") + 
  scale_fill_manual(values = c('#E41A1C','#377EB8','#4DAF4A')) +
  theme_bw(base_line_size = 0) + 
  guides(fill=FALSE) +
  stat_boxplot(aes(ymin=..lower..,ymax=..upper..),size = 1, width = 0.6) +
  stat_boxplot(geom = "errorbar",aes(ymin=..ymax..),width=0.3,color="black") +  # 添加上方最大值横线
  stat_boxplot(geom = "errorbar",aes(ymax=..ymin..),width=0.3,color="black") +  # 添加下方最小值横线
  labs(title = paste0('P=',p)) + 
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(face = "bold",size = 10),
    axis.title = element_text(face = "bold", size = 12),
    title = element_text(face = "bold", size = 14),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(file=paste0(g,'_ACE_Boxplot.png'),plot = P,device = 'png',width = 9,height = 6)
ggsave(file=paste0(g,'_ACE_Boxplot.pdf'),plot = P,device = 'pdf',width = 9,height = 6)
#### Evenness
x <- df[unique(df$Group)[1]==df$Group,3]
y <- df[unique(df$Group)[2]==df$Group,3]
z <- df[unique(df$Group)[3]==df$Group,3]
p <- kruskal.test(list(x,y,z))
p <- round(p$p.value, digits = 4)
P <- ggplot(data = df,mapping = aes(x=Group,y=Evenness,fill=Group)) + 
  geom_boxplot(width = 0.6,linetype="dashed") + 
  scale_fill_manual(values = c('#E41A1C','#377EB8','#4DAF4A')) +
  theme_bw(base_line_size = 0) + 
  guides(fill=FALSE) +
  stat_boxplot(aes(ymin=..lower..,ymax=..upper..),size = 1, width = 0.6) +
  stat_boxplot(geom = "errorbar",aes(ymin=..ymax..),width=0.3,color="black") +  # 添加上方最大值横线
  stat_boxplot(geom = "errorbar",aes(ymax=..ymin..),width=0.3,color="black") +  # 添加下方最小值横线
  labs(title = paste0('P=',p)) + 
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(face = "bold",size = 10),
    axis.title = element_text(face = "bold", size = 12),
    title = element_text(face = "bold", size = 14),
    plot.title = element_text(hjust = 0.5)
  )
ggsave(file=paste0(g,'_Evenness_Boxplot.png'),plot = P,device = 'png',width = 9,height = 6)
ggsave(file=paste0(g,'_Evenness_Boxplot.pdf'),plot = P,device = 'pdf',width = 9,height = 6)

