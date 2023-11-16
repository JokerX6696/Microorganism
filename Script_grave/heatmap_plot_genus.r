rm(list=ls())
setwd('D:/desk/XMSH_202311_7584/3.TOP30/ANOVA')
library('ggplot2')
library('reshape2')
library('pheatmap')
library('dplyr')
ipt <- c('Bifidobacterium','Streptococcus','Lactobacillus','Lactobacillus','Lactobacillus','Lactobacillus','Streptococcus','Veillonella','Actinomyces','Porphyromonas','Fusobacterium','Helicobacter','Staphylococcus','Prevotella','Xanthomonas')
df_abd <- read.table('Abundance_Stat.group.xls',sep='\t',quote = "",comment.char = "",header = TRUE)
df_abd <- df_abd[,-c(1:5,7)]


df <- read.table('genus.ANOVA.Group.add_sum.xls',sep='\t',quote = "",comment.char = "",header = TRUE)

sel <- df$genus[df$P <= 0.05]
if(length(sel)>30){sel <- sel[1:30]}
jj <- unique(c(sel,ipt))
jj <- jj[jj %in% df_abd$Genus]


df_plot <- df_abd[df_abd$Genus %in% jj,]
df_plot <- as.data.frame(df_plot %>% group_by(Genus) %>% summarize(across(.cols = 1:4, .fns = sum)))

rownames(df_plot) <- df_plot$Genus
df_plot <- df_plot[,-1]

annotation_col = data.frame(
  Group = factor(names(df_plot))
)
rownames(annotation_col) = names(df_plot)
ann_colors = list(Group=c(blank = '#00468BFF',PBS = '#ED0000FF',CHX = '#42B540FF',A5B5 ='#0099B4FF'))
pdf(file = 'TOP30_special_genus_heatmap.pdf',width = 6,height = 9)
pheatmap(
  mat = df_plot,
  scale = "row",
  color = colorRampPalette(c("navy", "white", "firebrick3"))(50),
  cluster_row = TRUE,
  cluster_cols = FALSE,
  annotation_col = annotation_col,
  cellwidth = 24,
  angle_col = 45,
  annotation_colors = ann_colors
)
dev.off()




