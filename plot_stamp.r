rm(list=ls())
setwd('D:/desk/XMSH_202309_6088')
library('reshape2')
library('ggplot2')
library(patchwork)
####### para
group_f <- 'mapping.txt'
f <- 'KEGG_L3.Kruskal_Wallis.xls'
list_od <- c('EtOH_1','EtOH_2','EtOH_3','Con_4')
###########  wilcox 函数封装
wilcox_test_zfx <- function(group1,group2){
  if(identical(group1,group2)){
    p_value <- 999
    lower_bound = 999
    upper_bound = 999
    median_difference = (lower_bound + upper_bound) / 2
  }else{
    result <- wilcox.test(group1, group2,conf.int = TRUE)
    p_value <- result$p.value
    lower_bound = result$conf.int[1]
    upper_bound = result$conf.int[2]
    median_difference = (lower_bound + upper_bound) / 2
  }

  t_l <- c(p_value,median_difference,lower_bound,upper_bound)
  return(t_l)
}
########## 处理分组文件
group <- read.table(group_f)
group <- group[,c(1,4)]
names(group) <- c('Sample','Group')
group$Group <- gsub("-","_",group$Group)
########### data df
data <- read.table(f,sep = '\t',header = TRUE, row.names = 1,quote = "")
for (Sample in list_od) {
  if (Sample == 'EtOH_4') {
    next
  }else{
    selcet_sample_EtOH_4 <- group$Sample[group$Group == 'EtOH_4']
    
    selcet_sample <- group$Sample[group$Group == Sample]
    rownames(data) <- gsub("\"","",rownames(data))
    #rownames(data) <- gsub("\\s","_",rownames(data))
    ##########
    df_x <- data[,selcet_sample]
    df_y <- data[,selcet_sample_EtOH_4]
    # p1 
    plot_df1 <- data.frame(apply(df_x,1,mean),apply(df_y,1,mean))
    names(plot_df1) <- c(Sample,'EtOH_4')
    p_vet <- c()
    for (k in rownames(plot_df1)) {
      group1 <- as.numeric(df_x[k,])
      group2 <- as.numeric(df_y[k,])
      ret_p <- wilcox_test_zfx(group1,group2)
      p_vet <- c(p_vet,ret_p)
    }
    
    info_df <- as.data.frame(matrix(p_vet,ncol=4,byrow = TRUE));names(info_df) <- c('p_value','median','low','up')
    rownames(info_df) <- rownames(df_x)
    
    plot_info_df <- cbind(plot_df1,info_df)
    plot_info_df <- plot_info_df[order(plot_info_df$p_value),]
    plot_info_df <-plot_info_df[1:30,]
    plot_df1 <- plot_info_df[,1:2]
    
    plot_df1$L3 <- rownames(plot_df1)
    
    plot_df <- melt(plot_df1,id.vars = 'L3')
    
    names(plot_df) <- c('L3','Group','value')
    plot_df$Group <- as.factor(plot_df$Group)
    levels(plot_df$Group)=c(Sample,'EtOH_4')
    levels(plot_df$L3) <- rev(rownames(plot_info_df))
    #levels(plot_df$L3) <- rev(rownames(plot_info_df))
    p1 <- ggplot(plot_df,mapping=aes(x=L3,y=value,fill = Group),) +
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
            legend.position = 'top',
            legend.direction = "horizontal",
            legend.key.width = unit(0.8,"cm"),
            legend.key = element_rect(fill = "transparent"),
            #legend.key.height = unit(0.5,"cm"))for (i in 1:(30 - 1))
      ) 
    
    
    for (i in 1:29) {
      p1 <- p1 + annotate('rect', xmin = i+0.5, xmax = i+1.5, ymin = 0, ymax = Inf,
                        fill = ifelse(i %% 2 == 0, 'white', 'gray95'))
    }
    
    p1 <- p1 + geom_bar(stat = "identity",position = position_dodge(1)) + 
      scale_fill_manual(breaks=c(Sample,'EtOH_4'),values = c("#ED0000FF","#00468BFF"))
    ########### p2
    plot_df_2 <- plot_info_df[,4:6]
    plot_df_2$L3 <- factor(rownames(plot_df_2),levels = levels(plot_df$L3))
    plot_df_2$Group <- ifelse(plot_df_2$median < 0,'EtOH_4',Sample)
    p2 <- ggplot(data = plot_df_2,mapping = aes(x=L3,y=median,fill=Group)) +
      theme(panel.background = element_rect(fill = 'transparent'),
            panel.grid = element_blank(),
            axis.ticks.length = unit(0.4,"lines"),
            axis.ticks = element_line(color='black'),
            axis.line = element_line(colour = "black"),
            axis.title.x=element_text(colour='black', size=12,face = "bold"),
            axis.text=element_text(colour='black',size=10,face = "bold"),
            axis.text.y = element_blank(),
            legend.position = "none",
            axis.line.y = element_blank(),
            axis.ticks.y = element_blank(),
            plot.title = element_text(size = 15,face = "bold",colour = "black",hjust = 0.5,vjust = -8)) +
      scale_x_discrete(limits = levels(plot_df_2$L3)) +
      coord_flip() +
      xlab("") +
      ylab("") +
      labs(title="95% confidence intervals")
    for (i in 1:29) {
      p2 <- p2 + annotate('rect', xmin = i+0.5, xmax = i+1.5, ymin = -Inf, ymax = Inf,
                          fill = ifelse(i %% 2 == 0, 'white', 'gray95'))
    }
    p2 <- p2 +
      geom_errorbar(aes(ymin = low, ymax = up),
                    position = position_dodge(0.8), width = 0.5, size = 0.5) +
      geom_point(shape = 21,size = 3) +
      scale_fill_manual(breaks = c(Sample,'EtOH_4'),values = c("#ED0000FF","#00468BFF")) +
      geom_hline(aes(yintercept = 0), linetype = 'dashed', color = 'black')
    ################### p3
    plot_df_3 <- data.frame(
      name=factor(rownames(plot_info_df),
                  levels=levels(plot_df$L3)),
      p_value=round(plot_info_df[,3], digits = 4))
    rownames(plot_df_3) <- plot_df_3$name
    levels(plot_df_3$p_value) <- levels(plot_df$L3)
    
    
    
    p3 <- ggplot(plot_df_3,aes(name,p_value)) +
      geom_text(aes(y = 0,x = name),label = plot_df_3$p_value,
                hjust = 0,fontface = "bold",inherit.aes = FALSE,size = 3) +
      geom_text(aes(x = 15 +0.5,y = 0.7),label = "P-value (Wilcox.test)",
                srt = 90,fontface = "bold",size = 5) +
      coord_flip() +
      ylim(c(0,1)) +
      theme(panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            axis.title = element_blank())
    
    
    p <- p1 + p2 + p3 + plot_layout(widths = c(6,6,2))
    ggsave(filename = paste0(Sample,'_vs_EtOH_4_','stamp.png'),plot = p,device = 'png',width = 12,height = 6)
    ggsave(filename = paste0(Sample,'_vs_EtOH_4_','stamp.pdf'),plot = p,device = 'pdf',width = 12,height = 6)
    sel <- group$Sample[group$Group == Sample | group$Group == 'EtOH_4']
    t_df <- data[rownames(plot_info_df),sel]
    wt_df <- data.frame(L3=rownames(plot_info_df),t_df,plot_info_df[,1:3])
    wt_df <- wt_df[,-c(18,19)]
    write.table(x = wt_df,file = paste0(Sample,'_vs_EtOH_4_','stat.xls'),sep = '\t',row.names = FALSE,quote = F)
  }
}


