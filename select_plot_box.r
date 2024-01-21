rm(list=ls())
library(ggplot2)
library(dplyr)
library(reshape2)
##################################################################################################################
sel <- c('Sphingomonas','Tannerella','Alloprevotella','Malassezia','Catonella','Pseudomonas','Bacteroides')
abd <- read.table(file = 'genus.xls',sep='\t',quote = "",comment.char = "",header = T)
abd <- abd[abd$Taxonomy %in% sel,]

df = melt(abd,id.vars = 'Taxonomy')

df$Group <- ifelse(grepl('LB',x = df$variable),'A','B')
names(df)[2:3] <- c('genus','Abundance')
# df_plot = df
# df_plot$Abundance = -log10(df_plot$Abundance)
p <- ggplot() + 
  geom_boxplot(data = df,mapping = aes(x=Taxonomy,y=Abundance,fill=Group)) + 
  theme_bw() + 
  #ylim(0, 0.2) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
ggsave(filename = 'genus_boxplot.pdf',plot = p,device = 'pdf',width = 9,height = 6)
ggsave(filename = 'genus_boxplot.png',plot = p,device = 'png',width = 9,height = 6)
##################################################################################################################
sel2 <- c('Sphingomonas_sp000797515','Lactobacillus_johnsonii','Alloprevotella_tannerae','Tannerella_forsythia','Porphyromonas_gingivalis','Limosilactobacillus_vaginalis_A')
abd <- read.table(file = 'species.xls',sep='\t',quote = "",comment.char = "",header = T)
abd <- abd[abd$Taxonomy %in% sel2,]

df = melt(abd,id.vars = 'Taxonomy')

df$Group <- ifelse(grepl('LB',x = df$variable),'A','B')
names(df)[2:3] <- c('species','Abundance')
# df_plot = df
# df_plot$Abundance = -log10(df_plot$Abundance)
p <- ggplot() + 
  geom_boxplot(data = df,mapping = aes(x=Taxonomy,y=Abundance,fill=Group)) + 
  theme_bw() + 
  #ylim(0, 0.2) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
ggsave(filename = 'species_boxplot.pdf',plot = p,device = 'pdf',width = 9,height = 6)
ggsave(filename = 'species_boxplot.png',plot = p,device = 'png',width = 9,height = 6)
