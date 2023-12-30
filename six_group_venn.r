rm(list=ls())
setwd('D:/desk/XMSH_202312_8664')
library(venn)
f <- 'species.xls'
df <- read.table(f,sep = '\t',header = T,row.names = 1,quote = "",comment.char = "")

venn_list <- list()

for (sample in names(df)) {
  venn_list[[sample]] <- rownames(df)[df[,sample] != 0]
}

pdf(file = 'Group_venn.pdf',width = 9,height = 6)

venn(venn_list,
     zcolor='style', # 调整颜色，style是默认颜色，bw是无颜色，当然也可以自定义颜色
     opacity = 0.3,  # 调整颜色透明度
     box = F,        # 是否添加边框
     ilcs = 0.5,     # 数字大小
     sncs = 1# 组名字体大小
)
dev.off()
