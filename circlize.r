rm(list=ls())
library(circlize)

f_list <- dir('./all',pattern = 'Correlation.xls')

for (f in f_list) {
  file <- paste0('./all/',f)
  lev <- gsub('.Correlation.xls','',f)
  out <- paste0(lev,'_top20_circos','.pdf')
  dt <- read.table(file,sep = '\t',header = T)
  
  dt <- dt[rev(order(abs(dt$Correlation))),]
  dt <- dt[dt$p.value < 0.05,]
  if(nrow(dt)<=20){aaa <- nrow(dt)}else{aaa <- 20}
  dt <- dt[1:aaa,1:3]
  
  names(dt) <- c('from',	'to',	'value')
  
  
  pdf(out,height = 12,width = 12)
  par(mar = c(2, 2, 2, 2))  # 减小图形边距
  circos.clear()
  col_fun = colorRamp2(range(dt$value), c('blue', "#FF0000"), transparency = 0.25) # 根据相关性进行颜色渐变处理
  circos.par(
    circle.margin = c(0.6,1.5)
  )
  chordDiagram(
    x = dt,
    col = col_fun,
    directional = 1,                               # 箭头方向。选项有1,0,-1
    direction.type = c("arrows", "diffHeight"),    # 线条两端的形状
    diffHeight = -0.02,                            # 线条两端距离边缘的距离差
    annotationTrack = c( "grid"),   # 都绘制哪些内容，name标签；grid边缘形状；axis刻度
    annotationTrackHeight = c(0.05, 0.08),         # 标签距离图形的距离; 环形边缘的宽度
    link.arr.type = "big.arrow",                   # 形状"curved", "triangle", "circle", "ellipse".
    link.sort = TRUE,                              # 内部排序
    link.largest.ontop = TRUE,                     # 控制添加链接的顺序，是否基于绝对值?
    transparency = 0.25                            # 线条透明度,
    
  )
  
  for(si in get.all.sector.index()) {
    xlim = get.cell.meta.data("xlim", sector.index = si, track.index = 1)
    ylim = get.cell.meta.data("ylim", sector.index = si, track.index = 1)
    if(si %in% dt$from){ft <- 2}else{ft <- 1}
    circos.text(mean(xlim), mean(ylim), si, sector.index = si, track.index = 1, 
                facing = "clockwise", niceFacing = TRUE, col = "black",adj = c(-0.1, 0),font = ft)
  }
  
  dev.off()
  
  
}



# 设置要查找的目录路径
directory_path <- "D:/desk/XMSH_202309_6289/files/result/DZQD2023040473_DZQD2023040472_micro_metabonomics_Report"  # 替换为你要查找的目录路径

# 设置要查找的字符串
target_string <- "*Correlation*"  # 替换为你要查找的字符串

find_files_with_string <- function(directory, target_string) {
  matching_files <- list()
  
  # 获取目录下的所有文件和子目录
  files_and_dirs <- list.files(directory, full.names = TRUE)
  
  for (item in files_and_dirs) {
    if (file.info(item)$isdir) {
      # 如果是子目录，则递归调用函数
      matching_files <- c(matching_files, find_files_with_string(item, target_string))
    } else {
      # 如果是文件，则查找文件中是否包含目标字符串
      file_content <- readLines(item, warn = FALSE)
      if (any(grepl(target_string, file_content))) {
        matching_files <- c(matching_files, item)
      }
    }
  }
  
  return(matching_files)
}

# 调用函数查找符合条件的文件
matching_files <- find_files_with_string(directory_path, target_string)
files <- c()
for (f in matching_files) {
  if(grepl('Correlation',f)){
    files <- c(files,f)
  }
}

for (file in files) {
  if(grepl('kruskal_wallis',file)){
    m <- 'kruskal_wallis'
  }else if(grepl('ANOVA',file)){
    m <- 'ANOVA'
  }else{
      print('error')
      q()
  }
  lev <- strsplit(file,'/')[[1]][9]
  group <- strsplit(file,'/')[[1]][7]
  out <- paste0(group,'_',lev,'_',m,'_top20_circos','.pdf')
  dt <- read.table(file,sep = '\t',header = T,quote = "",comment.char = "")
  dt <- dt[dt$Pvalue < 0.05,]
  if(nrow(dt)<2){next}
  if(nrow(dt)<=20){aaa <- nrow(dt)}else{aaa <- 20}
  dt <- dt[rev(order(abs(dt$Correlation)))[1:aaa],]
  
  
  dt <- dt[,1:3]
  
  names(dt) <- c('from',	'to',	'value')
  
  
  pdf(out,height = 12,width = 12)
  par(mar = c(2, 2, 2, 2))  # 减小图形边距
  circos.clear()
  col_fun = colorRamp2(range(dt$value), c('blue', "#FF0000"), transparency = 0.25) # 根据相关性进行颜色渐变处理
  circos.par(
    circle.margin = c(0.6,1.5)
  )
  chordDiagram(
    x = dt,
    col = col_fun,
    directional = 1,                               # 箭头方向。选项有1,0,-1
    direction.type = c("arrows", "diffHeight"),    # 线条两端的形状
    diffHeight = -0.02,                            # 线条两端距离边缘的距离差
    annotationTrack = c( "grid"),   # 都绘制哪些内容，name标签；grid边缘形状；axis刻度
    annotationTrackHeight = c(0.05, 0.08),         # 标签距离图形的距离; 环形边缘的宽度
    link.arr.type = "big.arrow",                   # 形状"curved", "triangle", "circle", "ellipse".
    link.sort = TRUE,                              # 内部排序
    link.largest.ontop = TRUE,                     # 控制添加链接的顺序，是否基于绝对值?
    transparency = 0.25                            # 线条透明度,
    
  )
  
  for(si in get.all.sector.index()) {
    xlim = get.cell.meta.data("xlim", sector.index = si, track.index = 1)
    ylim = get.cell.meta.data("ylim", sector.index = si, track.index = 1)
    if(si %in% dt$from){ft <- 2}else{ft <- 1}
    circos.text(mean(xlim), mean(ylim), si, sector.index = si, track.index = 1, 
                facing = "clockwise", niceFacing = TRUE, col = "black",adj = c(-0.1, 0),font = ft)
  }
  
  dev.off()
  
}


