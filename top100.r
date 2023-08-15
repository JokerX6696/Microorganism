#!/data/USER/liujiang/bin/Rscript
rm(list=ls())
#setwd('D:/desk/R_temp')
file = c(
  './all/relative_abundance/otu_table_even_L7.txt',
  './DBS__HBS/relative_abundance/otu_table_even_L7.txt'
  
)
top_num = 100

for(i in file){
  df <- read.table(i, sep='\t', header = TRUE,row.names = 1)
  
  df$sum = apply(df,1,sum)
  
  
  df <- df[rev(order(df$sum)),]
  
  
  df <- df[c(1:top_num),-length(df)]
  
  df <- data.frame(taxon=rownames(df),df)
  
  
  out_name <- sub("\\.txt$",paste0('_top_',top_num,'.xls'), i)
  write.table(x = df,file=out_name, quote = FALSE, sep = '\t',row.names = FALSE)
}




