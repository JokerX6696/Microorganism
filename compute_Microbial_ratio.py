

# para
input = 'D:/desk/XMSH_202309_6215/Abundance.filtered.xls'
out_f = 'D:/desk/XMSH_202309_6215/Fungi_Bacteria_ratio.txt'



level_list = ['#Kingdom','Phylum','Class','Order','Family','Genus','Species']

bl1 = 'Fungi'
bl2 = 'Bacteria'

f = open(input, 'r')
data = f.readlines()
f.close

compute_dict = {}
samples = []
for line in data:
    col_all = line.strip().split('\t')
    if col_all[0] == level_list[0]:
        for i in col_all:
            if i in level_list:
                continue
            else:
                compute_dict[i] = {}
                compute_dict[i][bl1] = 0
                compute_dict[i][bl2] = 0
                samples.append(i)
    elif bl1 == col_all[0] or bl1 == col_all[1] or bl1 == col_all[2] or bl1 == col_all[3] or bl1 == col_all[4] or bl1 == col_all[5] or bl1 == col_all[6]:
        idx = 0
        for i in col_all[7:]:
            sample = samples[idx]
            compute_dict[sample][bl1] += float(i)
            idx += 1
    elif bl2 == col_all[0] or bl2 == col_all[1] or bl2 == col_all[2] or bl2 == col_all[3] or bl2 == col_all[4] or bl2 == col_all[5] or bl2 == col_all[6]:
        idx = 0
        for i in col_all[7:]:
            sample = samples[idx]
            compute_dict[sample][bl2] += float(i)
            idx += 1

print(compute_dict)

out = open(out_f, 'w')
print('Sample',bl1,bl2,'%s / %s' %(bl1,bl2),sep='\t',file=out)
for i in compute_dict:
    ret = compute_dict[i][bl1]/compute_dict[i][bl2]
    if 'ONEC' not in i:
        print(i,compute_dict[i][bl1],compute_dict[i][bl2],ret,sep='\t',file=out)