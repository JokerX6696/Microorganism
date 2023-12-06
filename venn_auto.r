rm(list=ls())
setwd('D:/desk/XMSH_202311_8242/')
library('VennDiagram')
df <- read.table('species.xls',sep='\t',comment.char = "",header = T,row.names = 1)

groups <- names(df)
# C 6 2
sel <- combn(groups, 2)
for (seled in 1:ncol(sel)) {
  seleds <- sel[,seled]
  g1 <- seleds[1]
  g2 <- seleds[2]
  vet_1 <- rownames(df[df[,g1] != 0,])
  vet_2 <- rownames(df[df[,g2] != 0,])
  lst <- list(g1=vet_1,g2=vet_2)
  names(lst) <- c(g1,g2)
  out <- paste0(g1,"_",g2)
  venn.diagram(x=lst,
               
               scaled = T, # 根据比例显示大小
               
               alpha= 0.5, #透明度
               
               lwd=1,lty=1,col=c('#00468BFF','#AD002AFF'), #圆圈线条粗细、形状、颜色；1 实线, 2 虚线, blank无线条
               
               label.col ='black' , # 数字颜色abel.col=c('#FFFFCC','#CCFFFF',......)根据不同颜色显示数值颜色
               
               cex = 2, # 数字大小
               
               fontface = "bold",  # 字体粗细；加粗bold
               
               fill=c('#00468BFF','#AD002AFF'), # 填充色 配色https://www.58pic.com/
               
               #category.names = c("Set1", "Set2") , #标签名
               
               cat.dist = 0.02, # 标签距离圆圈的远近
               
               cat.pos = -180, # 标签相对于圆圈的角度cat.pos = c(-10, 10, 135)
               
               cat.cex = 2, #标签字体大小
               
               cat.fontface = "bold",  # 标签字体加粗
               
               cat.col='black' ,   #cat.col=c('#FFFFCC','#CCFFFF',.....)根据相应颜色改变标签颜色
               
               cat.default.pos = "outer",  # 标签位置, outer内;text 外
               
               output=TRUE,
               
               filename=paste0(out,'.png'),# 文件保存
               
               imagetype="png",  # 类型（tiff png svg）
               
               resolution = 400,  # 分辨率
               
               compression = "lzw"# 压缩算法
               
  )
}





