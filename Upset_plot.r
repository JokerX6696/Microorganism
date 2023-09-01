rm(list=ls())
setwd('D:/desk/file_bak/XMSH_202308_5809')
if(!require('UpSetR')){
  install.packages('UpSetR')
}
library('UpSetR')

out <- 'species'
f <- 'species.xls'
df <- read.table(f,header = T,row.names = 1,sep = '\t',comment.char = "",quote = "")
g <- unique(gsub('_\\d+$', '',names(df)))


all <- list()
for (i in g) {
  pos <- grepl(i,names(df))
  temp_df <- df[,pos]
  temp_vec <- apply(temp_df,1,sum)
  temp_pos <- names(which(0 != temp_vec))
  all[[i]] <- temp_pos
}


pdf(file = paste0(out,'_Upset.pdf'),width = 24,height = 6)
upset(
  fromList(all),
  nsets=length(all),
  #nsets = 100,
  nintersects = NA
  )


dev.off()


inter <- get.venn.partitions(all)
for (i in 1:nrow(inter)) inter[i,'values'] <- paste(inter[[i,'..values..']], collapse = '|')
inter <- subset(inter, select = -..values.. )
inter <- subset(inter, select = -..set.. )
write.table(inter, "result.csv", row.names = FALSE, sep = ',', quote = FALSE)
