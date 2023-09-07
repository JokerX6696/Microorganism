rm(list=ls())
setwd('D:/desk/XMSH_202308_5641')
library("pROC")
library('ggplot2')
library('reshape2')
library('RColorBrewer')
library("randomForest");set.seed(1311)
############## para
classify <- c(
  'Prevotella_melaninogenica',
  'Neisseria_sp000186165',
  'Neisseria_subflava',
  'Veillonella_atypica',
  'Prevotella_pallens',
  'Porphyromonas_gingivalis',
  'Porphyromonas_pasteri',
  'Prevotella_jejuni',
  'Prevotella_intermedia',
  'Prevotella_histicola',
  'Neisseria_elongata',
  'Veillonella_parvula_A'
)
f <- 'species.xls'
f_group <- 'group.list'
tree_num <- 1000

############# group
Group <- read.table(f_group,sep = '\t')
names(Group) <- c("sample",'group')
############## 矩阵处理
df <- read.table(f,sep = '\t',quote = "", header = TRUE,row.names = 1)

df <- df[rownames(df) %in% classify,]  # 提取指定微生物

df <- as.data.frame(t(df))
temp_vet <- c()
for (i in rownames(df)) {
  num <- which(i == Group$sample)
  temp_vet <- c(temp_vet,Group$group[num])
}
df$Group <- factor(temp_vet,levels = unique(temp_vet))
###################### 获取 mtry 值
idx <- sample(nrow(df),nrow(df)*0.7)

train_df <- df[idx,]
test_df <- df[-idx,]

getmtry <- tuneRF(
  train_df[,-dim(train_df)[2]],
  train_df[,dim(train_df)[2]],
  ntreeTry = 500,
  trace = TRUE,
  improve = 0.05,
  plot = TRUE
)
dev.off()
mtry <- getmtry[as.numeric(which(min(getmtry[,'OOBError']) == getmtry[,'OOBError'])[1]),1];mtry=1
######## 建模
train_rf <- randomForest(
  Group ~ ., 
  data = train_df,
  ntree = tree_num,
  mtry = mtry,
  importance=TRUE,
  proximity=TRUE
)

train_rf$importance
#  基于模型进行预测
pre_ran <- predict(train_rf, newdata = test_df)
obs_p_ran = data.frame(prob=pre_ran,obs=test_df$Group)

ran_roc <- roc(test_df$Group,as.numeric(pre_ran))
plot(ran_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),grid.col=c("green", "red"), max.auc.polygon=TRUE,auc.polygon.col="skyblue", print.thres=TRUE,main='随机森林模型ROC曲线')


# boxplot 可视化
plot_df <- as.data.frame(predict(train_rf, type = "prob") ) # 查看每个样本预测到不同样本的概率
plot_df <- melt(plot_df)
names(plot_df) <- c('Group', 'Value')
plot_df$Group <- gsub("^\\S_","",plot_df$Group)
x <- plot_df[plot_df$Group==unique(plot_df$Group)[1],2]
y <- plot_df[plot_df$Group==unique(plot_df$Group)[2],2]
p <- wilcox.test(x,y);p <- round(p$p.value, digits = 4)

P <- ggplot(data = plot_df,mapping = aes(x=Group,y=Value,fill=Group)) + 
  geom_boxplot(width = 0.6,linetype="dashed") + 
  scale_fill_manual(values = c('#E41A1C','#377EB8','#4DAF4A','#0099B4FF','#925E9FFF')) +
  theme_bw(base_line_size = 0) + 
  guides(fill=FALSE) +
  stat_boxplot(aes(ymin=..lower..,ymax=..upper..),size = 1, width = 0.6) +
  stat_boxplot(geom = "errorbar",aes(ymin=..ymax..),width=0.3,color="black") +  # 添加上方最大值横线
  stat_boxplot(geom = "errorbar",aes(ymax=..ymin..),width=0.3,color="black") +  # 添加下方最小值横线
  labs(y="POD",title = paste0('Probability\nWilcoxon P=',p)) + 
  theme(
    panel.grid = element_blank(),
    axis.text = element_text(face = "bold",size = 10),
    axis.title = element_text(face = "bold", size = 12),
    title = element_text(face = "bold", size = 14),
    plot.title = element_text(hjust = 0.5)
  )
g=""
ggsave(file=paste0(g,'randomForest_POD.png'),plot = P,device = 'png',width = 9,height = 6)
ggsave(file=paste0(g,'randomForest_POD.pdf'),plot = P,device = 'pdf',width = 9,height = 6)

