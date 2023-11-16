rm(list=ls())
setwd('D:/desk/XMSH_202311_7584/3.TOP30/ANOVA')
library('ggplot2')
library(reshape2)
ipt <- c('Bifidobacterium','Streptococcus','Lactobacillus','Lactobacillus','Lactobacillus','Lactobacillus','Streptococcus','Veillonella','Actinomyces','Porphyromonas','Fusobacterium','Helicobacter','Staphylococcus','Prevotella','Xanthomonas')
df_abd <- read.table('Abundance_Stat.group.xls',sep='\t',quote = "",comment.char = "",header = TRUE)
df_abd <- df_abd[,-c(1:5,7)]
color <- c("#00008B", "#FFFF00", "#E34234", "#7CFC00", "#008B8B", "#39FF14", "#FF007F", "#00A86B", "#00FF7F", 
           "#8B00FF", "#FFA500", "#4682B4", "#8B0000", "#00C957", "#FF007F", "#C0C0C0", "#A52A2A", 
           "#228B22", "#FF4500", "#9400D3", "#6A5ACD", "#FFD700", "#FBEC5D", "#4B0082", "#008080", 
           "#C71585","#000080","#BC8F8F",
            "#FF4500", "#00BFFF", "#FFD700", "#FF69B4", "#C0C0C0", "#EEE8AA", "#4169E1", "#808000",  "#00FF00","#FF1493", "#000000", "#F7E7CE")


df <- read.table('genus.ANOVA.Group.add_sum.xls',sep='\t',quote = "",comment.char = "",header = TRUE)

sel <- df$genus[df$P <= 0.05]
if(length(sel)>30){sel <- sel[1:30]}
jj <- unique(c(sel,ipt))
jj <- jj[jj %in% df_abd$Genus]


df_plot <- df_abd[df_abd$Genus %in% jj,]
lev <- unique(rev(df_plot$Genus[order(df_plot$A5B5)]))
df_plot <- melt(df_plot)
names(df_plot)[2:3] <- c('Group','Abundance')
df_plot$Group <- factor(df_plot$Group,levels = unique(df_plot$Group))
df_plot$Genus <- factor(df_plot$Genus,levels = rev(lev))
p <- ggplot(data = df_plot,mapping = aes(x=Group,y=Abundance,fill=Genus)) + 
  geom_bar(stat = 'identity',position = 'fill')+
  scale_fill_manual(values = color) + 
  scale_y_continuous(expand = c(0,0)) +
  theme_classic()
ggsave(filename = 'TOP30_special_Genus_bar.pdf',device = 'pdf',plot = p,width = 9,height = 6)
