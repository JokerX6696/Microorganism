from get_tag import get_tag_matrix
import pandas as pd
from method import Chao1, ACE
# para
host = 'human'
# 获取矩阵
df = get_tag_matrix(wd='D:/desk/python_wkdir/Quan')
# 删除宿主
df = df[df.index != host]
# 计算各个样本 alpha 多样性
samples = list(df.columns)
alpha_dict = {}
for sample in samples:
    alpha_dict[sample] = {}
    alpha_dict[sample]['observe'], alpha_dict[sample]['chao1'] = Chao1(df[sample])
    #alpha_dict[sample]['ACE'] = ACE(df[sample])


df_alpha = pd.DataFrame(alpha_dict)






print(df_alpha)