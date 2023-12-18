rm(list=ls())
setwd('D:/desk/XMSH_202312_8753')
library('ggplot2')
sel <- c('Samples','chao1','simpson','shannon')



color <- c('#00468BFF','#ED0000FF','#42B540FF','#0099B4FF','#925E9FFF','#FDAF91FF','#AD002AFF','#ADB9B6FF','#1B1919FF')
files <- dir(pattern = "*single_alpha_index.xls")
for (file in files) {
  df <- read.table(file=file,sep = '\t',header = T, quote = "",comment.char = "")
  
  df <- df[,sel]
  df$Group <- gsub('_\\d','',df$Samples)
  for (k in sel[2:4]) {
    
    df_plot <- df[,c('Samples',k,'Group')]
    g_names <- unique(df_plot$Group)
    g_num <- length(g_names)
    
    # 根据组 做 t.test 或 anova
    if(g_num<3){
      nums1 <- df_plot[df_plot$Group == g_names[1],k]
      nums2 <- df_plot[df_plot$Group == g_names[2],k]
      ret <- t.test(nums1,nums2)
      p = ret$p.value
    }else{
      df_test <- data.frame(value=df_plot[,k],group=df_plot$Group)
      ret <- summary.aov(aov(value ~ group, data = df_test))
      p=ret[[1]]['Pr(>F)'][1,1]
    }
    names(df_plot)[2]='value'
    p <- sprintf("%.4f", p)
    # out
    tmp=strsplit(file,'_')[[1]][1]
    out <- paste0(tmp,"_",k,".pdf")
    # 画图
    p <- ggplot(data = df_plot,mapping = aes(x=Group,y=value,fill=Group)) + 
      geom_boxplot(outlier.shape =  NA) + 
      theme_bw()+
      scale_fill_manual(values = color) + 
      labs(title = paste0('p = ',p),y=k) +
      stat_boxplot(geom = "errorbar",aes(ymin=..ymax..),width=0.1,color="black") +  # 添加上方最大值横线
      stat_boxplot(geom = "errorbar",aes(ymax=..ymin..),width=0.1,color="black") +  # 添加下方最小值横线
      theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5)
      )
    ggsave(filename = out,plot = p,device = 'pdf',width = 9,height = 6)
    
  }
}




