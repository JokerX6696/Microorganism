import re
import argparse
import pandas as pd

# para

parser = argparse.ArgumentParser()
parser.add_argument('-i', help='输入丰度矩阵文件 eg: Abundance.filtered.xls', type=str)
parser.add_argument('-o', help='输出文件路径,默认输出文件为 ./ratio.txt', type=str,default='./ratio.txt')
parser.add_argument('-b1', help='比例分子,可以是界门纲目科属种任意一级的的名称,默认是 Fungi(真菌).', type=str,default='Fungi')
parser.add_argument('-b2', help='比例分子,可以是界门纲目科属种任意一级的的名称,默认是 Bacteria(细菌).', type=str,default='Bacteria')
args = parser.parse_args()

input = args.i
out_f = args.o
bl1 = args.b1
bl2 = args.b2


level_list = ['#Kingdom','Phylum','Class','Order','Family','Genus','Species']



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

#print(compute_dict)

out = open(out_f, 'w')
print('Sample',bl1,bl2,'%s / %s' %(bl1,bl2),sep='\t',file=out)
for i in compute_dict:
    ret = compute_dict[i][bl1]/compute_dict[i][bl2]
    if 'ONEC' not in i:
        print(i,compute_dict[i][bl1],compute_dict[i][bl2],ret,sep='\t',file=out)