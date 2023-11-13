rm(list=ls())
setwd('D:/desk/XMSH_202311_7584/2.alpha_beta')
library('ggplot2')
library('ggpubr')

alpha_f <- 'Alpha_diversity_Index.xls'

df <- read.table(alpha_f,sep = '\t',quote = "",comment.char = "",header = T)
g <- gsub("_.*","",df$Sample)
df$Group <- g
df <- df[,c("Sample","Group",'Shannon')]

avg <- c(mean(df$Shannon[1:3]),mean(df$Shannon[4:6]),mean(df$Shannon[7:9]),mean(df$Shannon[10:12]))
errbar = c(sd(df$Shannon[1:3]),sd(df$Shannon[4:6]),sd(df$Shannon[7:9]),sd(df$Shannon[10:12]))

df_avg <- data.frame(Group=unique(g),avg=c(mean(df$Shannon[1:3]),mean(df$Shannon[4:6]),mean(df$Shannon[7:9]),mean(df$Shannon[10:12])))
df_avg$err <- errbar

df_avg$Group <- factor(df_avg$Group,levels = df_avg$Group)
df$Group <- factor(df$Group,levels = unique(df$Group))
df_aov <- data.frame(value=df$Shannon,Group <- factor(g))
names(df_aov) <- c('value','Group')
ret <- aov(value ~ Group,df_aov)
ret <- anova(ret)$"Pr(>F)"[1]
ret <- format(ret, scientific = TRUE)
ret <- signif(as.numeric(ret), digits = 2)
pvalue <- paste0('** P = ',ret)
p <- ggplot() + 
  geom_bar(data = df_avg,mapping = aes(x=Group,y=avg,fill=Group),stat = 'identity',width = 0.6,alpha = 0.7) + 
  geom_point(data = df,mapping = aes(x=Group,y=Shannon,fill=Group), size = 2,show.legend = F) +
  theme_classic() + 
  geom_errorbar(data = df_avg,aes( ymin=(avg-err),ymax=(avg+err),x=Group),width = 0.2,stat = "identity",position = "identity") + 
  scale_fill_manual(values = c('#00468BFF','#ED0000FF','#42B540FF','#0099B4FF')) + 
  labs(y='Shannon') + 
  scale_y_continuous(limits = c(0,4),expand = c(0,0)) + 
  geom_signif(data=df_avg,
              aes(xmin='blank', xmax='A5B5', annotations=pvalue, y_position=3.5),
              textsize = 6, vjust = 0.05, tip_length = c(0.02, 0.02),
              manual=TRUE) 
  
ggsave(filename = 'Shannon_bar_plot.pdf',device = 'pdf',width = 9,height = 6,plot = p)








