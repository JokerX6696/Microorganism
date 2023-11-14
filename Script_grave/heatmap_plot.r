rm(list=ls())
setwd('D:/desk/XMSH_202311_7584/3.TOP30/')
library('ggplot2')
library('reshape2')
library('pheatmap')
ipt <- c('Bifidobacterium','Streptococcus','Lactobacillus_rhamnosus','Lactobacillus_plantarum','Lactobacillus_reuteri','Lactobacillus_paracasei','Streptococcus_mutans','Veillonella','Actinomyces_viscosus','Porphyromonas_gingivalis','Fusobacterium','Helicobacter_pylori','Staphylococcus_aureus','Prevotella','Xanthomonas')
df_abd <- read.table('Abundance_Stat.group.xls',sep='\t',quote = "",comment.char = "",header = TRUE)
df_abd <- df_abd[,-c(1:6)]


df <- read.table('species.ANOVA.Group.add_sum.xls',sep='\t',quote = "",comment.char = "",header = TRUE)

sel <- df$species[df$P <= 0.05]
if(length(sel)>30){sel <- sel[1:30]}
jj <- unique(c(sel,ipt))
jj <- jj[jj %in% df_abd$Specie]


df_plot <- df_abd[df_abd$Specie %in% jj,]
rownames(df_plot) <- df_plot$Specie
df_plot <- df_plot[,-1]

annotation_col = data.frame(
  Group = factor(names(df_plot))
)
rownames(annotation_col) = names(df_plot)
ann_colors = list(Group=c(blank = '#00468BFF',PBS = '#ED0000FF',CHX = '#42B540FF',A5B5 ='#0099B4FF'))
pdf(file = 'TOP30_special_Species_heatmap.pdf',width = 6,height = 9)
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




