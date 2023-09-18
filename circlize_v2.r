rm(list=ls())
library(circlize)
# 处理 代谢矩阵
met_f <- '代谢物通路汇总表.xlsx'
met <- data.frame(readxl::read_excel(met_f))   

met <- met[ ! is.na(met$ID.Annotation),]
met <- data.frame(Metabolites=met$Metabolites,ID=met$ID.Annotation,Annotation=met$Annotation,met[,32:ncol(met)])

colnames(met) <- gsub("\\.","_",colnames(met))

cor_met_df <- met[,4:ncol(met)]
rownames(cor_met_df) <- met$Metabolites
# rownames(cor_met_df) <- gsub("\\(","__",rownames(cor_met_df))
# rownames(cor_met_df) <- gsub("\\)","___",rownames(cor_met_df))
# cor_met_df <- data.frame(t(cor_met_df))
# 处理微生物矩阵
mic_f <- 'species.xls'
mic <- read.table(mic_f,sep = '\t',quote = "",comment.char = "",header = T,row.names = 1)
mic <- mic[,colnames(cor_met_df)]

mic_names <- rownames(mic)
met_names <- rownames(cor_met_df)
# 计算 相关性 显著性
temp <- c()

for (j in mic_names) {
  v_x <- as.numeric(mic[j,])
  for (k in met_names) {
    v_y <- as.numeric(cor_met_df[k,])
    td <- cor.test(x = v_x,y=v_y,method = 'spearman')
    p <- td$p.value
    cor <- as.numeric(td$estimate)
    temp <- c(temp,j,k,cor,p)
  }
}
# 处理相关性矩阵
cor_df <- data.frame(matrix(temp,ncol = 4,byrow = T))
names(cor_df) <- c('Taxonomy','Metabolites','cor','Pvalue')
cor_df$cor <- as.numeric(cor_df$cor)
cor_df$Pvalue <- as.numeric(cor_df$Pvalue)
cor_df <- cor_df[cor_df$Pvalue<0.05,]
cor_df <- cor_df[rev(order(abs(cor_df$cor)))[1:20],]

# 添加通路信息
aa <- c()
ab <- c()
for (m in cor_df$Metabolites) {
  pathway <- as.character(met[met$Metabolites == m,]['ID'])
  aa <- c(aa,pathway)
  pathway_Anno <- as.character(met[met$Metabolites == m,]['Annotation'])
  ab <- c(ab,pathway_Anno)
}
# cor_df$pathway <- aa
# cor_df$Anno <- ab
col = 1
all <- c()
for (j in aa) {
  if(grepl('\\|',j)){
    temp1 <- strsplit(j,'\\|')[[1]]
    temp2 <- strsplit(ab[col],'\\|')[[1]]
    l <- length(temp1)
    if(length(temp1) != length(temp2)){print('error');break}
    for (i in 1:l) {
      all <- c(all,as.character(cor_df[col,]),temp1[i],temp2[i])
    }
    
  }
  col = col + 1
}
cor_df_plot <- data.frame(matrix(all,ncol = 6,byrow = T))
names(cor_df_plot) <- c(names(cor_df),'pathway','Annotation')
path_anno <- data.frame(pathway=cor_df_plot$pathway,Annotation=cor_df_plot$Annotation)
cor_df_plot <- data.frame(cor_df_plot[,c(1,5,3)])
names(cor_df_plot) <- c('from','to','value')
cor_df_plot$value <- as.numeric(cor_df_plot$value)
# 绘制图片
###########
#
#
circos.clear()
circos.par(circle.margin = c(0.2,3,0.6,0.6))
############ test
# 画轮廓
sectors = c(unique(cor_df_plot$from),unique(cor_df_plot$to))
col_lenth1 <- length(unique(cor_df_plot$from))
col_lenth2 <- length(sectors) - col_lenth1
all_col <- c(rep(c('#3300ff','#6600cc'),50)[1:col_lenth1] , rep(c('#FF0000','#ff9999'),50)[1:col_lenth2])

s1 = factor(sectors,levels = c(unique(cor_df_plot$from),unique(cor_df_plot$to)))
circos.initialize(s1, xlim = c(0, 1))
circos.track(sectors, ylim = c(0.5, 1),track.height=cm_h(0.5),bg.col=all_col,bg.border='white') # 绘制轨道
# 添加名字
for(si in get.all.sector.index()) {
  xlim = get.cell.meta.data("xlim", sector.index = si, track.index = 1)
  ylim = get.cell.meta.data("ylim", sector.index = si, track.index = 1)
  if(si %in% cor_df_plot$to){ft <- 2}else{ft <- 1}
  circos.text(mean(xlim), mean(ylim), si, sector.index = si, track.index = 1, 
              facing = "clockwise", niceFacing = TRUE, col = "black",adj = c(-0.25, 0),font = ft,cex=0.5)
}

# 添加连接线
for (pt in 1:nrow(cor_df_plot)) {
  from <- cor_df_plot[pt,1]
  to <- cor_df_plot[pt,2]
  coll <- ifelse(cor_df_plot[pt,3]<0,yes = 'blue',no = 'red')
  circos.link(from, 0.5, to, 0.5,col = coll)
}


 

legend_labels <- c()
for (i in 1:nrow(path_anno)) {
  if(path_anno[i,1] %in% cor_df_plot$to){
    temp <- paste0(path_anno[i,1],':',path_anno[i,2])
    legend_labels <- c(legend_labels,temp)
  }
  
}
legend_labels <- unique(legend_labels)
legend(
  "right",                          # 图例的位置（可以是"top"、"bottom"、"left"、"right"等）
  legend = legend_labels,          # 图例标签            # 图例颜色
  title = "",                # 图例标题
  cex = 0.8,                       # 字体大小
  title.col = "black",             # 标题颜色
  bg = 'white',                    # 背景颜色
  inset = c(-0.06, 0) ,                 # 图例与图的距离（可调整）
  border = 'white',
  #fill = c("transparent", "transparent"),
  bty = 0,
  #fill = "transparent", border = "transparent", box.col = "white", box.lty = 0, box.lwd = 0
)








if(0){# 添加丰度线
  abd_list <- list()
  for(i in unique(cor_df_plot$from)){
    abd_list[i] <- mean(as.numeric(mic[i,]))
  }
  
  met_list <- list()
  for(i in unique(cor_df_plot$to)){
    met_list[i] <- mean(as.numeric(cor_met_df[i,]))
  }
  
  
  line_y <- names(abd_list)
  
  
  #######################################
  temp1 <- as.numeric(apply(mic[unique(cor_df_plot$from),], 1, mean) ) # 计算 微生物数列
  
  temp2 <- c()
  for (i in unique(cor_df_plot$to)) {
    
  }
  
  
  
  line_df <- 
    circos.clear()
  circos.initialize(s1, xlim = c(0, 1))
  circos.track(as.character(s1[1]), ylim=c(0,0.5),bg.border='white')
  for(i in sectors){
    circos.lines(c(-0.125,0.5,1.125),c(1,2,1),type='l',sector.index = i)
  }
  
  
  dev.off()
  
  
  
  
  
  
  
  
  
  
  
  
  circos.clear()
  
  #######################
  n <- 90
  df <- data.frame(
    sectors = rep(letters[1:9], each = 10),
    x = rep(1:10, 9), 
    y = runif(n)
  )
  
  circos.par("track.height" = 0.3)
  circos.initialize(df$sectors, x = df$x)
  circos.track(
    df$sectors, x = df$x, y = df$y, 
    panel.fun = function(x, y) {
      switch(CELL_META$sector.index,
             'a' = circos.lines(x, y, type = 'l'),
             'b' = circos.lines(
               x, y, type = 'o', pt.col = 'blue', cex = 0.7),
             'c' = circos.lines(x, y, type = 'h'),
             'd' = circos.lines(
               x, y, type = 'h', baseline = 0.5, 
               col = rainbow(10)),
             'e' = circos.lines(x, y, type = 's'),
             'f' = circos.lines(
               x, y, type = 'l', area = TRUE),
             'g' = circos.lines(
               x, y, type = 'o', area = TRUE),
             'h' = circos.lines(
               x, y, type = 's', area = TRUE),
             'i' = circos.lines(
               x, y, type = 'l', area = TRUE,
               baseline = 'top')
      )
      circos.text(CELL_META$xcenter, 
                  CELL_META$cell.ylim[2] + mm_y(3), 
                  CELL_META$sector.index)
    }
  )
  
  #######################
  
  # col_fun = colorRamp2(range(cor_df_plot$value), c('blue', "#FF0000"), transparency = 0.25) # 根据相关性进行颜色渐变处理
  # circos.par(
  #   circle.margin = c(0.6,1.5)
  # )
  # 
  # 
  # chordDiagram(
  #   x = cor_df_plot,
  #   col = col_fun,
  #   directional = 1,                               # 箭头方向。选项有1,0,-1
  #   direction.type = c("arrows", "diffHeight"),    # 线条两端的形状
  #   diffHeight = -0.02,                            # 线条两端距离边缘的距离差
  #   annotationTrack = c( "grid"),   # 都绘制哪些内容，name标签；grid边缘形状；axis刻度
  #   annotationTrackHeight = c(0.05, 0.08),         # 标签距离图形的距离; 环形边缘的宽度
  #   link.arr.type = "big.arrow",                   # 形状"curved", "triangle", "circle", "ellipse".
  #   link.sort = TRUE,                              # 内部排序
  #   link.largest.ontop = TRUE,                     # 控制添加链接的顺序，是否基于绝对值?
  #   transparency = 0.25 ,                           # 线条透明度,
  #   link.lwd = 0.1,
  #   link.lty = 2
  # )
  # 
  # for(si in get.all.sector.index()) {
  #   xlim = get.cell.meta.data("xlim", sector.index = si, track.index = 1)
  #   ylim = get.cell.meta.data("ylim", sector.index = si, track.index = 1)
  #   if(si %in% cor_df_plot$to){ft <- 2}else{ft <- 1}
  #   circos.text(mean(xlim), mean(ylim), si, sector.index = si, track.index = 1, 
  #               facing = "clockwise", niceFacing = TRUE, col = "black",adj = c(0.5, 0),font = ft,cex=0.5)
  # }
  # 
  # abd_list <- list()
  # for(i in unique(cor_df_plot$from)){
  #   abd_list[i] <- mean(as.numeric(mic[i,]))
  # }
  # 
  # met_list <- list()
  # for(i in unique(cor_df_plot$to)){
  #   met_list[i] <- mean(as.numeric(cor_met_df[i,]))
  # }
  
  
  
  
  
}
