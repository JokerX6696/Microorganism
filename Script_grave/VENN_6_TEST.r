dat <- data.frame()
set1 <- 1:100
set2 <- 51:150
set3 <- 101:200
set4 <- 151:250
set5 <- 201:300
set6 <- 251:350

venn6List <- as.list(set1,set2,set3,set4,set5,set6)
draw.quintuple.venn(venn6List,
     zcolor='style', # 调整颜色，style是默认颜色，bw是无颜色，当然也可以自定义颜色
     opacity = 0.3,  # 调整颜色透明度
     box = F,        # 是否添加边框
     ilcs = 0.5,     # 数字大小
     sncs = 1# 组名字体大小
)
