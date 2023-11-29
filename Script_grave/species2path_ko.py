#!D:/Application/python/python.exe
import pandas as pd
wkdir = 'D:/desk/asd/'
#  ko -> KO -> sp -> mic_name
df = pd.read_csv(wkdir + 'KO.format.xls',sep='\t',index_col='KO')
df = df['Pathway']
KO_ko = {}
KO = df.index
for i in KO:
    info = df[i]
    if str(info) == 'nan':
        continue
    else:
        info = str(info).split('|')
        for k in info:
            if k not in KO_ko:
                KO_ko[k] = [i]
            else:
                KO_ko[k].append(i)
            



f1 = wkdir + 'sp2spe.txt'

sp2species = {}
with open(f1,'r') as f:   # 得到sp 与 物种对应关系
    for line in f:
        info = line.replace('\n', '').split('\t')
        sp2species[info[0]] = info[1].strip().split(' ')[-1].replace('s__', '')

df = pd.read_csv(wkdir + 'KO_predicted.tsv',sep='\t',index_col='sequence')
paths = list(df.columns)
cog_mic = {} # KO 与 微生物对应关系 sp
for i in paths:
    con = df[i] != 0
    idx = list(df[con].index)
    cog_mic[i] = idx

final_file = wkdir + 'KEGGpathway_three_levels_v2.xls'
fo1 = wkdir + 'ko2species.xls'
fo = open(fo1,'w')
with open(final_file,'r') as f:
    for line in f:
        info = line.replace('\n', '').split('\t')
        print(info[0],info[1],info[2],info[3],sep='\t',end='\t',file=fo)
        cog_id = info[0]

        if cog_id not in KO_ko:
            continue
        sp_id_list_pre=KO_ko[cog_id]
        all = []
        for j in KO_ko[cog_id]:
            if j not in cog_mic:
                continue
            sp_id_list = cog_mic[j]
            for i in sp_id_list:
                if i not in sp2species:
                    continue
                else:
                    all.append(sp2species[i])
        all = list(set(all))
        for l in all:
            print(l,end=';',file=fo)
        print('',file=fo)

fo.close



