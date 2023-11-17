# 删除微生物丰度定量表宿主信息
import pandas as pd
f = 'D:/desk/XMSH_202311_7876/GTDB.40w.g5.2_step.xls'

df = pd.read_csv(filepath_or_buffer=f,sep='\t')
rownum = 183
df_raw = df.iloc[:rownum,:]
df_host = df.iloc[rownum:,:]

ret = df_host.iloc[:,7:].sum()

df_raw.iloc[:,7:] = df_raw.iloc[:,7:] / (1-ret)
df_raw.to_csv('D:/desk/XMSH_202311_7876/GTDB.40w.g5.2_Abundance.filtered_rmHost.xls',sep='\t',index=False)


# f = 'D:/desk/XMSH_202311_7876/GTDB.40w.g5.2_step.xls'

# df = pd.read_csv(filepath_or_buffer=f,sep='\t')

# df_raw = df.iloc[:581,:]
# df_host = df.iloc[581:,:]

# ret = df_host.iloc[:,7:].sum()

# df_raw.iloc[:,7:] = df_raw.iloc[:,7:] / (1-ret)
# df_raw.to_csv('D:/desk/XMSH_202311_7876/GTDB.40w.g5.2_Abundance.filtered_rmHost.xls',sep='\t',index=False)