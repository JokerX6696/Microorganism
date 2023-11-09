#!D:/application/anaconda/python.exe
import pandas as pd
abd = 'D:/desk/XMSH_202311_7705/Abundance.filtered.xls'

df = pd.read_csv(abd,sep='\t',index_col='Species')
df = df.iloc[:,6:]
writer = pd.ExcelWriter('D:/desk/XMSH_202311_7705/venn_stat.xlsx', engine='xlsxwriter')
for i in range(1,32):
    s1 = 'C' + str(i)
    s2 = 'T' + str(i)
    C = set(df[df[s1] != 0].index)
    T = set(df[df[s2] != 0].index)
    gongyou = C & T
    C_teyou = C - T
    T_teyou = T - C
    temp_dict = {}

    temp_dict[(s1 + '_unique')] = {}
    for j in C_teyou:
        name = j
        abd = df.loc[j,s1]
        temp_dict[(s1 + '_unique')][name] = abd
    
    temp_dict['common'+ ' in ' + s1] = {}
    temp_dict['common'+ ' in ' + s2] = {}
    for j in gongyou:
        name = j
        abd = df.loc[j,s1]
        temp_dict['common'+ ' in ' + s1][name] = abd
        temp_dict['common'+ ' in ' + s2][name] = abd

    temp_dict[(s2 + '_unique')] = {}
    for j in T_teyou:
        name = j
        abd = df.loc[j,s2]
        temp_dict[(s2 + '_unique')][name] = abd
    
    out = pd.DataFrame(temp_dict)
    out.to_excel(writer,sheet_name=(s1 + '_' + s2))
#writer.save()
writer.close()