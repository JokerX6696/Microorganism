rm(list=ls())
setwd('D:/desk/XMSH_202305_4066')
file <- 'abd.txt'
df <- read.table(file,sep = '\t',quote = "",header = TRUE)
df <- df[,-c(1,2,3,4,5,6,8,9)]


out_name <- 'group_t.test.xls'

stage_4 <- c("XK0001C","XK0002C","XK0008C","XK0009C")
stage_3 <- c("XK0005C","XK0007C","XK0003C")
SCT <- c("XK0001C","XK0005C","XK0007C","XK0006C")
SCC <- c("XK0003C","XK0004C","XK0008C")
ADE <- c("XK0002C","XK0009C")
Infected <- c("XK0001C","XK0002C","XK0004C","XK0005C","XK0006C")
no_Infected <- c("XK0003C","XK0007C","XK0008C","XK0009C")
hypertension <- c("XK0003C","XK0004C","XK0006C","XK0007C","XK0009C")
no_hypertension <- c("XK0001C","XK0002C","XK0005C","XK0008C")
for(i in c(stage_4,stage_3,SCT,SCC,ADE,Infected,no_Infected,hypertension,no_hypertension)){
  if(i %in% names(df)){
    print('yes')
  }else{
    print('no')
    print(i)
    break
  }
}
specie <- c()

p_value_st34 <- c()
p_value_SCT_SCC <- c()
p_value_SCT_ADE <- c()
p_value_SCC_ADE <- c()
p_value_Infected <- c()
p_value_hypertension <- c()
for(s in 1:dim(df)[1]){
  df_line <- df[s,]
  specie <- c(specie, df_line$Specie)
  # st3 st4
  st4 <- as.numeric(df_line[stage_4])
  st3 <- as.numeric(df_line[stage_3])
  st3_4 <- t.test(st3,st4)
  st3_4 <- st3_4$p.value
  p_value_st34 <- c(p_value_st34,st3_4)
  # SCT SCC ADE
  sct_tmp <- as.numeric(df_line[SCT])
  scc_tmp <- as.numeric(df_line[SCC])
  ade_tmp <- as.numeric(df_line[ADE])
  sct_scc <- t.test(sct_tmp, scc_tmp)
  sct_scc <- sct_scc$p.value
  p_value_SCT_SCC <- c(p_value_SCT_SCC,sct_scc)
  sct_ade <- t.test(sct_tmp, ade_tmp)
  sct_ade <- sct_ade$p.value
  p_value_SCT_ADE <- c(p_value_SCT_ADE, sct_ade)
  scc_ade <- t.test(scc_tmp,ade_tmp)
  scc_ade <- scc_ade$p.value
  p_value_SCC_ADE <- c(p_value_SCC_ADE,scc_ade)
  # Infected
  ined <- as.numeric(df_line[Infected])
  uined <- as.numeric(df_line[no_Infected])
  i_u <- t.test(ined,uined)
  i_u <- i_u$p.value
  p_value_Infected <- c(p_value_Infected,i_u)
  # hypertension
  hy <- as.numeric(df_line[hypertension])
  uhy <- as.numeric(df_line[no_hypertension])
  h <- t.test(hy,uhy)
  h <- h$p.value
  p_value_hypertension <- c(p_value_hypertension,h)
}
df_p <- data.frame(specie,
                   p_value_st34,
                   p_value_SCT_SCC,
                   p_value_SCT_ADE,
                   p_value_SCC_ADE,
                   p_value_Infected,
                   p_value_hypertension
                   )
names(df_p) <- c("Specie","p_value(分期)","p_value(小细胞_鳞癌)",
                 "p_value(小细胞_腺癌)","p_value(鳞癌_腺癌)","p_value(感染)",
                 "p_value(高血压病史)")

write.table(df_p,file = out_name,quote = F,sep = '\t',row.names = FALSE)
