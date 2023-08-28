import get_tag
import pandas as pd
# para
host = 'human'
# 获取矩阵
df = get_tag.get_tag_matrix(wd='D:/desk/python_wkdir/Quan')
# 删除宿主
df = df[df.index != host]
# 计算各个样本 alpha 多样性
samples = list(df.columns)
alpha_dict = {}
for sample in samples:
    alpha_dict[sample] = {}
    alpha_dict[sample]['chao1'] = Chao1(df[sample])
    alpha_dict[sample]['ACE'] = ACE(df[sample])









print(list(df.columns))