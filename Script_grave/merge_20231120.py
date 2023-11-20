#!D:/Application/python/python.exe

wkdir = 'D:/desk/XMSH_202311_7872/Union/'
avg = 'Abundance_Stat.group.xls'
# t-test
sps = [
'Prevotella_salivae',
'Porphyromonas_sp_KLE_1280',
'Devosia_sp_Root635',
'Candidatus_Saccharibacteria_bacterium_TM7_EAM_G5_1_HOT_356',
'Schaalia_odontolytica',
'Rothia_sp_HMSC036D11',
'Capnocytophaga_sp_oral_taxon_329_str_F0087',
'Veillonella_parvula',
'Bacteroidetes_oral',
'Porphyromonas_sp_oral_taxon_278_str_W7784',
'Bergeyella_cardium',
'Devosia_sp_Gsoil_520',
'Candidatus_Saccharibacteria_bacterium_TM7_ANC_38_39_G1_1',
'Leptotrichia_sp_oral_taxon_498',
'Lachnoanaerobaculum_saburreum',
'Ralstonia_pickettii',
'Porphyromonas_gingivalis',
'Rhodoplanes_piscinae'
]

out = wkdir + 'T-test_sig_4group_stat.xls'
new = []
f = open(wkdir+avg,'r')
fo = open(out,'w')
for line in f:
    info = line.split('\t')
    if '#' in info[0]:
        print(line,file=fo,end='')
    elif info[6] in sps:
        new.append(line)

new = list(set(new))
for i in new:
    print(i,end='',file=fo)


sps = [
'Lachnoanaerobaculum_saburreum',
'Olsenella_uli',
'Porphyromonas_gingivalis',
'Ralstonia_pickettii',
'Schaalia_odontolytica',
'Atopobium_parvulum',
'Prevotella_marshii',
'Slackia_exigua',
'Rhodoplanes_piscinae',
'Ideonella_sakaiensis',
'Prevotella_multiformis',
'Prevotella_sp_HJM029',
'Aggregatibacter_aphrophilus',
'Hydrogenophaga_sp_H7',
'Phocaeicola_abscessus',
'Porphyromonas_sp_KLE_1280',
'Prevotella_salivae',
'Capnocytophaga_sp_oral_taxon_329_str_F0087',
'Aggregatibacter_segnis',
'Candidatus_Saccharibacteria_bacterium_TM7_EAM_G5_1_HOT_356',
'Bergeyella_cardium',
'Schaalia_odontolytica',
'Devosia_sp_Root635',
'Selenomonas_massiliensis',
'Phreatobacter_cathodiphilus',
'Eubacterium_minutum',
'Prevotella_sp_oral_taxon_473_str_F0040',
'Streptococcus_mutans',
'Selenomonas_flueggei',
'Prevotella_baroniae',
'Ralstonia_sp_NFACC01',
'Piscinibacter_caeni',
'Veillonella_parvula',
'Candidatus_Saccharibacteria_bacterium_TM7_ANC_38_39_G1_1',
'Rothia_sp_HMSC071F11',
'Leptotrichia_sp_oral_taxon_498',
'Capnocytophaga_sp_oral_taxon_863_str_F0517',
'Pyramidobacter_piscolens',
'Prevotella_sp_KCOM_3155',
'Rothia_sp_HMSC036D11',
'Capnocytophaga_leadbetteri',
'Ralstonia_sp_UNC404CL21Col',
'Prevotella_maculosa',
'Pseudopropionibacterium_propionicum',
'Olsenella_sp_oral_taxon_807',
'Devosia_sp_Gsoil_520',
'Bacteroidetes_oral',
'Porphyromonas_sp_oral_taxon_278_str_W7784',
'Prevotella_denticola'
]


out = wkdir + 'Wilcoxon_sig_4group_stat.xls'
new = []
f = open(wkdir+avg,'r')
fo = open(out,'w')
for line in f:
    info = line.split('\t')
    if '#' in info[0]:
        print(line,file=fo,end='')
    elif info[6] in sps:
        new.append(line)

new = list(set(new))
for i in new:
    print(i,end='',file=fo)

