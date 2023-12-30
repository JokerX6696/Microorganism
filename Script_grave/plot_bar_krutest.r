rm(list=ls())
setwd('D:/desk/XMSH_202312_8664/3.stamp')
library('reshape2')
library('ggplot2')
library('patchwork')
####### para
Sample <- 'Con_3'
Sample2 <- "EtOH_3"
group_f <- 'mapping.txt'
f <- 'KEGG_L3.Kruskal_Wallis.xls'
list_od <- c('EtOH_1','EtOH_2','EtOH_3','Con_4')
###########  ANOVA 函数封装

########## 处理分组文件
Group <- c('PT','LT','PEpT','VT','TT','PhT')
########### data df
f <- 'L2.Top15_phylum.xls'
df <- as.data.frame(t(read.table(f,sep = '\t',quote = "",comment.char = "",header = T,row.names = 1)))
df <- df[,rev(names(df))]
df$Group <- gsub("_\\d","",rownames(df))

df_bak <- df
df <- melt(df)
names(df)[2:3] <- c('name','value')
m <- max(df$value)
# p1 
p1 <- ggplot() + 
  scale_x_discrete(limits = df$name) + # 不可缺少 确定范围
  coord_flip() +
  xlab("") +
  ylab("") +
  theme_bw() +
  scale_y_continuous(expand = c(0,0),limits = c(0,m+0.15)) +
  geom_bar(data = df,mapping = aes(x=name,y=value,fill=Group),stat = "identity",position = "dodge") + 
  scale_fill_manual(values = c("#ED0000FF","#00468BFF","#42B540FF","#0099B4FF","#925E9FFF",'#FDAF91FF'))
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
        legend.position = 'right',
        legend.direction = "vertical",
        legend.key.width = unit(0.8,"cm"),
        legend.key = element_rect(fill = "transparent"),
        legend.box = "vertical",
        
  ) 

  # 获取不同菌种的 kruskal 检验结果
  ret <- list()
  vec <- c()
  for (i in names(df_bak)) {
    if(i=='Group'){next}
    dff <- df_bak[,c(i,'Group')]
    names(dff)[1] = 'value'
    model <- kruskal.test(value ~ Group,dff)
    value <- model$p.value
    #value <- model$`Pr(>F)`[1]
    ret[i] <- value
    vec <- c(vec,value)
  }
  anova <- as.data.frame(ret)
  anova <- data.frame(name=names(anova),value=vec)
  sig <- c()
  for (i in anova$value) {
    if(i > 0.05){sig <- c(sig,"    ")}
    else if(i <= 0.05 & i > 0.01){sig <- c(sig," *  ")}
    else if(i <= 0.01 & i > 0.001){sig <- c(sig," ** ")}
    else{sig <- c(sig,"*** ")}
  }
  anova$value <- sprintf("%.4f", anova$value)
  anova$sig <- sig
  anova$value2 <- paste(anova$value,anova$sig)
  
  p1 <- p1 + 
    geom_text(data=anova,aes(label = value2,y = m+0.05,x=name), vjust = -0.5, size = 3.5) + 
    labs(title = 'kruskal test') + 
    theme(plot.title = element_text(hjust = 0.5,size = 25, face = "bold"))
    
  