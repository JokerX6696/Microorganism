#!D:/Application/python/python.exe
import pandas as pd
wkdir = 'D:/desk/asd/'
f1 = wkdir + 'sp2spe.txt'
#  cog0000 -> sp -> mic_name
sp2species = {}
with open(f1,'r') as f:   # 得到sp 与 物种对应关系
    for line in f:
        info = line.replace('\n', '').split('\t')
        sp2species[info[0]] = info[1].strip().split(' ')[-1].replace('s__', '')

df = pd.read_csv(wkdir + 'COG_predicted.tsv',sep='\t',index_col='sequence')
paths = list(df.columns)
cog_mic = {} # cog 与 微生物对应关系 sp
for i in paths:
    con = df[i] != 0
    idx = list(df[con].index)
    cog_mic[i] = idx

final_file = wkdir + 'cog_info_TEST3.tsv'
fo1 = wkdir + 'cog2species.xls'
fo = open(fo1,'w')
with open(final_file,'r') as f:
    for line in f:
        info = line.replace('\n', '').split('\t')
        cog_id = info[0]
        if cog_id not in cog_mic:
            continue
        sp_id_list=cog_mic[cog_id]
        print(info[0],info[1],sep='\t',end='\t',file=fo)
        for i in sp_id_list:
            if i not in sp2species:
                continue
            print(sp2species[i],end=';',file=fo)
        print('',file=fo)

fo.close