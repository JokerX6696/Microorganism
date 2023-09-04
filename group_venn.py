#!D:\application\anaconda\python.exe
# -*- coding: utf-8 -*-
# 处理微生物 两组之间画 venn
import pandas as pd


# para
g_f = 'OM_NOM_group.xlsx'
data_file = 'D:/desk/XMSH_202308_5860/species.xls'
fo = open('D:/desk/XMSH_202308_5860/OM_NOM_group.txt','w')

#####################################
df = pd.read_excel('D:/desk/XMSH_202308_5860/'+g_f,sheet_name='分组信息')

group = {}

count = 0
while True:
    con = df['Group'] == list(set(list(df['Group'])))[count]
    name = list(set(list(df['Group'])))[count]
    group[name] = list(df[con]['#SampleID'])
    count += 1
    if count >= len(list(set(list(df['Group'])))):
        break


data = pd.read_csv(data_file,sep='\t')

col_num = data.shape[1]
species = {}
for j in group:
    species[j] = []
    for k in group[j]:
        
        con = data[k] > 0
        species[j] += list(data[con]['Taxonomy'])
    
    species[j] = list(set(species[j]))




m = 0
for i in species:
    if len(species[i]) > m:
        m = len(species[i])

print(list(species.keys())[0],list(species.keys())[1],sep='\t',file=fo)

for i in range(0,m):

    if i >= len(species[list(species.keys())[0]]):
        a = ''
    else:
        a = species[list(species.keys())[0]][i]
    if i >= len(species[list(species.keys())[1]]):
        b = ''
    else:
        b = species[list(species.keys())[1]][i]
    print(a,b,sep='\t',file=fo)

fo.close
