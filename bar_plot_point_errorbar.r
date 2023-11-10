rm(list=ls())
setwd('D:/desk/XMSH_202311_7584/2.alpha_beta')
library('ggplot2')

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

ggplot() + 
  geom_bar(data = df_avg,mapping = aes(x=Group,y=avg,fill=Group),stat = 'identity') + 
  geom_point(data = df,mapping = aes(x=Group,y=Shannon,fill=Group), size = 2,show.legend = F) +
  theme_bw() + 
  geom_errorbar(data = df_avg,aes( ymin=(avg-err),ymax=(avg+err),x=Group),width = 0.2,stat = "identity",position = "identity") + 
  scale_fill_manual(values = c('#00468BFF','#ED0000FF','#42B540FF','#0099B4FF'))









