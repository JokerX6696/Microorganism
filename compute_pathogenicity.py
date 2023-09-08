

# para
input = 'D:/desk/XMSH_202309_6215/Abundance.all.xls'
output = 'D:/desk/XMSH_202309_6215/pathogenicity_index.xls'
data_db = 'D:/desk/XMSH_202309_6215/pathogenicity_index.db'

def get_pathogenicity(I,O,db):
    db_set=set()
    with open(db,'r') as DB:
            for line in DB:
                    line = line.strip()
                    if line.startswith('#') or not line:continue
                    tmp = line.split('\t')
                    db_set.add(tmp[0])

    sample_sort=[]
    col2name_dict={}
    sample_index_dict={}
    sample_species_dict={}
    with open(I,'r') as IN:
            for line in IN:
                    line = line.strip()
                    if not line:continue
                    tmp=line.split('\t')
                    if line.startswith('#Kingdom'):
                            for i in range(7,len(tmp)):
                                    sample_sort.append(tmp[i]) #记录样本顺序
                                    col2name_dict[i]=tmp[i] #记录列对应的样本名称
                    elif tmp[6] in db_set: #判断种名是否在数据库中
                            for i in range(7,len(tmp)):
                                    sample_name=col2name_dict[i]
                                    sample_index_dict.setdefault(sample_name,[]).append(float(tmp[i])) #记录数值
                                    if float(tmp[i])!=0:
                                            sample_species_dict.setdefault(sample_name,[]).append(tmp[6]) #记录致病菌名称

    with open(O,'w') as OU:
            OU.write('Sample_name\tPathogenicity_index\tSpecies_name\n')
            for sample_name in sample_sort:
                if 'OENC' in sample_name:
                    continue
                else:
                    try:
                            OU.write('{}\t{}\t{}\n'.format(sample_name,sum(sample_index_dict[sample_name]),';'.join(sample_species_dict[sample_name])))
                    except:
                            OU.write('{}\t{}\tNone\n'.format(sample_name,sum(sample_index_dict[sample_name])))


get_pathogenicity(I=input,O=output,db=data_db)