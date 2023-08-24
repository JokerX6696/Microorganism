#  Merger 丰度矩阵
import pandas as pd
import argparse

# 两个矩阵合并,返回一个新矩阵

def merge(df1,df2): 
    # 合并后的新矩阵 待处理
    new_df = pd.concat([df1, df2],axis=1)
    
    # 提取 种水平列
    Species = list(new_df.index)
    # 提取 列索引
    col_idx = list(new_df.columns)
    # 提取 样本矩阵
    samples = []
    for i in col_idx:
        if i not in level_list and i != 'Species':
            samples.append(i)
    df_samples = new_df[samples].reset_index(drop=True)
    # 处理 level 矩阵
    lev = {}
    for i in level_list:
        lev[i] = []
        for index, row in new_df[i].iterrows():
            temp_list = list(row)
            for k in temp_list:
                if (pd.isna(k)):
                    continue
                else:
                    lev[i].append(k)
                    break
    lev['Species'] = Species
    df_lev = pd.DataFrame(lev,columns=(level_list + ['Species']))
    df_fin = pd.concat([df_lev, df_samples], axis=1)
    return df_fin


level_list = ['#Kingdom', 'Phylum', 'Class', 'Order', 'Family', 'Genus'] # 按照 Speices 进行 merge 所以这里没有 Species

# 传参
parser = argparse.ArgumentParser()
parser.add_argument('-i1', help='丰度文件 1', type=str)
parser.add_argument('-i2', help='丰度文件 2', type=str)
parser.add_argument('-l', help='如果需要 merge 多个丰度矩阵 可使用此参数 使用 "," 分割. eg:/wkdir1/Abundance.filtered.xls,/wkdir2/Abundance.filtered.xls,/wkdir3/Abundance.filtered.xls,', type=str)
parser.add_argument('-s', help='选取样本,最终产生的文件包含的样本,使用 "," 分割. eg:sample1,sample2,sample3  默认输出所有丰度表中包含的样本', type=str)
parser.add_argument('-o', help='输出文件路径！', type=str,default='Abundance.filtered.xls')

args = parser.parse_args()
# 处理参数
file1 = args.i1
file2 = args.i2
files = args.l
out = args.o
sample_list = args.s

if file1 != None:
    pass
else:
    file1 = ""

if file2 != None:
    pass
else:
    file2 = ""

if files != None:
    files = files.split(',')
else:
    files = []
if sample_list != None:
    sample_list = ['#Kingdom', 'Phylum', 'Class', 'Order', 'Family', 'Genus', 'Species'] + sample_list.split(',')
    sel = True
else:
    sel = False


##########
Abd_list = [ i for i in list(set([file1] + [file2] + files )) if i != '']

if len(Abd_list) < 2:
    print('至少需要 2 个丰度表才可以 merge !!!')
    exit(1)




#####
f = Abd_list[0]
raw_df = pd.read_csv(f, sep='\t',index_col='Species')
for file in Abd_list[1:]:
    f_add = file 
    add_df = pd.read_csv(f_add, sep='\t',index_col='Species')
    df = merge(df1=raw_df,df2=add_df)
    raw_df = df.set_index('Spices')

# 选择指定样本
if sel:
    df = df[sample_list]
# 最终将缺失 值 NaN 填充为 0
df = df.fillna(0)
df.to_csv(out,sep='\t',index=False) 



