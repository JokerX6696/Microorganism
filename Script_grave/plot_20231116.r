rm(list=ls())
setwd('D:/desk/XMSH_202311_7584/4.bar')
library('ggplot2')
library('ggpubr')
library('dplyr')
library(reshape2)
df <- read.table('Abundance_Stat.filter.xls',sep='\t',quote = "",comment.char = "",header = TRUE)
bc <- 'Family';ipt = c('Bifidobacterium','Streptococcus','Lactobacillus','Lactobacillus','Lactobacillus','Lactobacillus','Streptococcus','Veillonella','Actinomyces','Porphyromonas','Fusobacterium','Helicobacter','Staphylococcus','Prevotella','Xanthomonas')
df <- data.frame(name=df[,bc],df[,8:19])
df <- df[df$name %in% ipt,]
df <- as.data.frame(df %>% group_by(name) %>% summarize(across(.cols = 1:12, .fns = sum)))
rownames(df) <- df[,'name']
df <- as.data.frame(t(df[,-1]))
df$Sample <- rownames(df)
xh <- names(df)[-length(names(df))]
raw_df <- df
for (i in xh) {
  df <- raw_df[,c(i,'Sample')]
  names(df)[1] <- 'Abundance'
  
  df$Group <- gsub('_.+',"",df$Sample)
  df <- df[,c("Sample","Group",'Abundance')]
  df <- df[order(df$Group),]
  avg <- c(mean(df$Abundance[1:3]),mean(df$Abundance[4:6]),mean(df$Abundance[7:9]),mean(df$Abundance[10:12]))
  errbar = c(sd(df$Abundance[1:3]),mean(df$Abundance[4:6]),mean(df$Abundance[7:9]),mean(df$Abundance[10:12]))
  
  df_avg <- data.frame(Group=unique(df$Group),avg=avg)
  df_avg$err <- errbar
  
  df_avg$Group <- factor(df_avg$Group,levels = c('blank','PBS','CHX','A5B5'))
  df$Group <- factor(df$Group,levels = c('blank','PBS','CHX','A5B5'))
  df_aov <- data.frame(value=df$Abundance,Group <- factor(unique(df$Group)))
  names(df_aov) <- c('value','Group')
  ret <- aov(value ~ Group,df_aov)
  ret <- anova(ret)$"Pr(>F)"[1]
  ret <- format(ret, scientific = TRUE)
  ret <- signif(as.numeric(ret), digits = 2)
  pvalue <- paste0('P = ',ret)
  
  p <- ggplot() + 
    geom_bar(data = df_avg,mapping = aes(x=Group,y=avg,fill=Group),stat = 'identity',width = 0.6,alpha = 0.7) + 
    geom_point(data = df,mapping = aes(x=Group,y=Abundance,fill=Group), size = 2,show.legend = F) +
    theme_classic() + 
    geom_errorbar(data = df_avg,aes( ymin=ifelse((avg-err)<0,0,(avg-err)),ymax=(avg+err),x=Group),width = 0.2,stat = "identity",position = "identity") + 
    scale_fill_manual(values = c('#00468BFF','#ED0000FF','#42B540FF','#0099B4FF')) + 
    labs(y='Abundance') + 
    scale_y_continuous(limits = c(0,(max(df$Abundance)*1.5)),expand = c(0,0)) + 
    geom_signif(data=df_avg,
                aes(xmin='blank', xmax='A5B5', annotations=pvalue, y_position=(max(df$Abundance)*1.2)),
                textsize = 6, vjust = 0.05, tip_length = c(0.02, 0.02),
                manual=TRUE)  + 
    ggtitle(i)+ 
    theme(plot.title = element_text(hjust = 0.5, size = 20))
  
  ggsave(filename = paste0(i,'_Abundance_bar_plot.pdf'),device = 'pdf',width = 9,height = 6,plot = p)
  
  
  
}



