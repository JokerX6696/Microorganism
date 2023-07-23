rm(list=ls())
setwd('D:/desk/temp')
human_file <- paste0('human','_Abundance.xls')
mm_file <- paste0('mm','_Abundance.xls')

df_human <- read.table(human_file,sep = '\t', quote = "", comment.char = "",header = T)
df_mm <- read.table(mm_file,sep = '\t', quote = "", comment.char = "",header = T)


df_all <- merge(x=df_human,y=df_mm,by = 'Species',all = TRUE)


pattern = 'Kingdom|Phylum|Class|Order|Family|Genus|Species'

grepl(pattern = pattern,x = names(df_all))
df1 <- df_all[,2:7]
df2 <- df_all[,18:23]
specie <- df_all[,1]
for(i in 1:dim(df2)[1]){
  if(is.na(df2[i,1])){
    df2[i,] = df1[i,]
  }
}

df_all <- df_all[,-c(18:23)]
df_all <- df_all[,-c(1:7)]
df_all[is.na(df_all)] <- 0
df <- data.frame(df2,specie,df_all)
names(df) <- gsub(pattern = '\\.*|^X',replacement = "",names(df))


