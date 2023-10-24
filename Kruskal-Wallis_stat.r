rm(list=ls())
setwd('D:/desk/XMSH_202310_6956/20230908_DZQD2023080757-b1_LIM_Tong_Wah-王浩軒-24个人2bRAD-M项目/')
group <- 'Group_map.txt'
group <- read.table(group,header = T,comment.char = "",row.names = 1)
group$Analysis4 <- c(rep('SPC',6),rep('AP',6),rep('SPC',6),rep('AP',6))
path_df <- read.table('pathogenicity_index.txt',sep = '\t',header = T,row.names = 1)
radio_df <- read.table('Fungi_Bacteria_ratio.txt',sep = '\t',header = T,row.names = 1)
names(radio_df)[3] <- 'ratio'

ret_kru_radio_v <- c()
ret_t_radio_v <- c()
ret_aov_radio_v <- c()
ret_wil_radio_v <- c()
ret_kru_path_v <- c()
ret_t_path_v <- c()
ret_aov_path_v <- c()
ret_wil_path_v <- c()
out <- c()

for (g in 1:ncol(group)) {
  print(g)
  all_g <- unique(group[,g])
  all_g <- all_g[! is.na(all_g)]
  print(all_g)
  radio <- list();path <- list()
  for (k in all_g) {
    temp_name <- rownames(group)[group[,g] == k]
    temp_name <- temp_name[! is.na(temp_name)]
    radio[[k]] <- radio_df[temp_name,3]
    path[[k]] <- path_df[temp_name,1]
  }
  # 本次循环属性
  group_1 <- all_g[1]
  group_2 <- all_g[2]
  out <- c(out,paste0(group_1,'_',group_2))
  # path diff
  ret_kru_path <- kruskal.test(path)$p.value
  ret_t_path <- t.test(path[[1]],path[[2]])$p.value
  data <- data.frame(value=c(path[[1]],path[[2]]), group=c(rep(names(path)[1],length(path[[1]])),rep(names(path)[2],length(path[[2]]))))
  ret_aov_path <- summary(aov(value ~ group,data = data))[[1]][1,4]
  ret_wil_path <- wilcox.test(path[[1]],path[[2]])$p.value
  # radio diff
  ret_kru_radio <- kruskal.test(radio)$p.value
  ret_t_radio <- t.test(radio[[1]],radio[[2]])$p.value
  data <- data.frame(value=c(radio[[1]],radio[[2]]), group=c(rep(names(radio)[1],length(radio[[1]])),rep(names(radio)[2],length(radio[[2]]))))
  ret_aov_radio <- summary(aov(value ~ group,data = data))[[1]][1,4]
  ret_wil_radio <- wilcox.test(radio[[1]],radio[[2]])$p.value
  # stat
  ret_kru_radio_v <- c(ret_kru_radio_v,ret_kru_path)
  ret_t_radio_v <- c(ret_t_radio_v,ret_t_path)
  ret_aov_radio_v <- c(ret_aov_radio_v,ret_aov_path)
  ret_wil_radio_v <- c(ret_wil_radio_v,ret_wil_path)
  ret_kru_path_v <- c(ret_kru_path_v,ret_kru_radio)
  ret_t_path_v <- c(ret_t_path_v,ret_t_radio)
  ret_aov_path_v <- c(ret_aov_path_v,ret_aov_radio)
  ret_wil_path_v <- c(ret_wil_path_v,ret_wil_radio)
}

df <-t(data.frame(
  kruskal.test_radio=ret_kru_radio_v,
  t.test_radio=ret_t_radio_v,
  ANOVA_radio=ret_aov_radio_v,
  wilcox.test_ratio=ret_wil_radio_v,
  kruskal.test_pathogenicity_index=ret_kru_path_v,
  t.test_pathogenicity_index=ret_t_path_v,
  ANOVA_pathogenicity_index=ret_aov_path_v,
  pwilcox.test_athogenicity_index=ret_wil_path_v
))
colnames(df) <- out
df <- as.data.frame(df)
write.table(df,file = 'All_test_stat.xls',sep = '\t',quote = F)
