rm(list=ls())
setwd('D:/desk/XMSH_202311_7584/3.TOP30/ANOVA')
library('ggplot2')
library(reshape2)
ipt <- c('Bifidobacteriaceae','Streptococcaceae','Lactobacillaceae','Lactobacillaceae','Lactobacillaceae','Lactobacillaceae','Streptococcaceae','Veillonellaceae','Actinomycetaceae','Porphyromonadaceae','Fusobacteriaceae','Helicobacteraceae','Staphylococcaceae','Bacteroidaceae','Xanthomonadaceae')
df_abd <- read.table('Abundance_Stat.group.xls',sep='\t',quote = "",comment.char = "",header = TRUE)
df_abd <- df_abd[,-c(1:4,6,7)]
color <- c("#00008B", "#00BFFF","#FFFF00", "#E34234", "#7CFC00", "#008B8B", "#39FF14",  "#00A86B", "#00FF7F", 
           "#8B00FF", "#FFA500", "#4682B4","#808000", "#4169E1", "#8B0000", "#00C957", "#A52A2A", "#FF007F", "#C0C0C0", 
           "#228B22", "#FF4500", "#9400D3", "#6A5ACD", "#FF4500", "#000080","#FFD700", "#FBEC5D", "#4B0082", "#008080", 
           "#C71585","#BC8F8F",
             "#FFD700",   "#FF007F","#FF69B4", "#C0C0C0", "#EEE8AA", "#00FF00","#FF1493", "#000000", "#F7E7CE")


df <- read.table('family.ANOVA.Group.add_sum.xls',sep='\t',quote = "",comment.char = "",header = TRUE)

sel <- df$family[df$P <= 0.05]
if(length(sel)>30){sel <- sel[1:30]}
jj <- unique(c(sel,ipt))
jj <- jj[jj %in% df_abd$Family]


df_plot <- df_abd[df_abd$Family %in% jj,]
lev <- unique(rev(df_plot$Family[order(df_plot$A5B5)]))
df_plot <- melt(df_plot)
names(df_plot)[2:3] <- c('Group','Abundance')
df_plot$Group <- factor(df_plot$Group,levels = unique(df_plot$Group))
df_plot$Family <- factor(df_plot$Family,levels = rev(lev))
p <- ggplot(data = df_plot,mapping = aes(x=Group,y=Abundance,fill=Family)) + 
  geom_bar(stat = 'identity',position = 'fill')+
  scale_fill_manual(values = color) + 
  scale_y_continuous(expand = c(0,0)) +
  theme_classic()
ggsave(filename = 'TOP30_special_family_bar.pdf',device = 'pdf',plot = p,width = 9,height = 6)
