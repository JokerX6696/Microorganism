rm(list=ls())
setwd('D:/desk/XMSH_202311_7584/3.TOP30/')
library('ggplot2')
library(reshape2)
ipt <- c('Bifidobacterium','Streptococcus','Lactobacillus_rhamnosus','Lactobacillus_plantarum','Lactobacillus_reuteri','Lactobacillus_paracasei','Streptococcus_mutans','Veillonella','Actinomyces_viscosus','Porphyromonas_gingivalis','Fusobacterium','Helicobacter_pylori','Staphylococcus_aureus','Prevotella','Xanthomonas')
df_abd <- read.table('Abundance_Stat.group.xls',sep='\t',quote = "",comment.char = "",header = TRUE)
df_abd <- df_abd[,-c(1:6)]


df <- read.table('species.ANOVA.Group.add_sum.xls',sep='\t',quote = "",comment.char = "",header = TRUE)

sel <- df$species[df$P <= 0.05]
if(length(sel)>30){sel <- sel[1:30]}
jj <- unique(c(sel,ipt))
jj <- jj[jj %in% df_abd$Specie]


df_plot <- df_abd[df_abd$Specie %in% jj,]
lev <- df_plot$Specie[order(df_plot$A5B5)]
df_plot <- melt(df_plot)
names(df_plot)[2:3] <- c('Group','Abundance')
df_plot$Group <- factor(df_plot$Group,levels = unique(df_plot$Group))
df_plot$Specie <- factor(df_plot$Specie,levels = lev)
p <- ggplot(data = df_plot,mapping = aes(x=Group,y=Abundance,fill=Specie)) + 
  geom_bar(stat = 'identity',position = 'fill')+
  scale_fill_manual(values = rev(c("#330000", "#003300", "#000033", "#333300", "#330033", "#003333", "#663300", "#003366", "#660033", "#336600", "#330066", "#006633", "#663333", "#336633", "#663366", "#336633", "#336699", "#996633", "#663399", "#330099", "#993300", "#003399", "#990033", "#339966", "#669966", "#996699", "#996666", "#cc9966", "#9966cc", "#cc6699", "#66cc99", "#6699cc", "#cc9966", "#cc6699"
))) + 
  scale_y_continuous(expand = c(0,0)) +
  theme_classic()
ggsave(filename = 'TOP30_special_Species_bar.pdf',device = 'pdf',plot = p,width = 9,height = 6)
