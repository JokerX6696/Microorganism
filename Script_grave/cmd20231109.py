#!D:/Application/python/python.exe
#import sys
import pandas as pd
wkdir = 'D:/desk/XMSH_202311_7596/item1/'
all = 'Abundance_Stat.filter.xls'
df = pd.read_csv(wkdir+all,sep='\t')
zt = df['#Kingdom'] == 'Eukaryota'
fungi = df[zt]
lev = list(fungi.columns)[:7]
samples = list(fungi.columns)[7:]
dict_all = {}
for l in lev:
    dict_all[l] = {}
    temp_list = list(set(list(fungi[l])))
    for z in temp_list:
        zt = fungi[l] == z
        new = fungi[zt]
        dict_all[l][z] = list(new.iloc[:,7:].sum())
        
for i in dict_all:
    df = pd.DataFrame(dict_all[i],index=samples)
    out = i
    df =df.T
    df.to_csv(sep='\t',path_or_buf=(wkdir + out + '.xls'))
