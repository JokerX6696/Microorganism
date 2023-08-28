#!/bin/env python3
def get_tag_matrix(wd, jk='BcgI'):
    import os
    import pandas as pd

    samples_dir = []
    for i in os.listdir(wd):
        if 'OENC' not in i:
            samples_dir.append(i)

    all_dict = {}
    for sample in samples_dir:
        all_dict[sample] = {}
        temp_dir = wd + '/' + sample + '/' + sample + '.' + jk + '/'
        all_mic = os.listdir(temp_dir)
        for mic in all_mic:
            file_temp = temp_dir + mic
            tag_num = 0
            with open(file_temp, 'r') as f:
                for line in f:
                    tag_num += int(line.split('\t')[1])
                mic_name = mic.replace('.xls','')
                all_dict[sample][mic_name] = tag_num

    df = pd.DataFrame(all_dict)
    df = df.fillna(0)
    df[samples_dir] = df[samples_dir].astype(int)
    return df

