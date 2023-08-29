def stat_list(lst):  # 统计列表频次
    stat_dict = {}
    for i in lst:
        if i in stat_dict:
            stat_dict[i] += 1
        else:
            stat_dict[i] = 1
    return stat_dict



def Chao1(value_list):  # 顺便返回 S_obs
    S_obs = sum(1 for q in value_list if q != 0)
    F1 = sum(1 for q in value_list if q == 1)
    F2 = sum(1 for q in value_list if q == 2)
    S_chao1 = S_obs + F1*( F1 -1 )/2*( F2 + 1 )
    return S_obs, S_chao1


def ACE(value_list, rare_tag_threshold=10):
    num_rare = sum(1 for q in value_list if q <= rare_tag_threshold and q > 0) #  默认 小于 10个 标签的 认为是 稀有物种
    
    # 计算稀有物种 标签数 小于 10 的 物种 Σ i*Fi
    n_rare = 0
    stat_num = stat_list(value_list)
    s_rare = 0 # 稀有物种的物种数
    F_i_sum = 0 # 计算 gamma_ace 用到的参数
    s_high_abd = 0 # 高丰度物种 标签数大于阈值的物种
    for i in stat_num:  # 这里在字典的 key 中循环 也就是 unique 后的 tag 数量
        if i <= rare_tag_threshold and i != 0:
            n_rare += (i * stat_num[i])
            s_rare += 1
            F_i_sum += (i*(i-1)*stat_num[i])
        elif i > rare_tag_threshold:
            s_high_abd += stat_num[i]
    F1 = sum(1 for q in value_list if q == 1) # F1 为 仅有一个标签的物种
    print(n_rare)
    cover_ace = 1 - (F1/n_rare)   # cover_ace 为样本覆盖度的估计值
    gamma_ace = max([(s_rare * F_i_sum)/(cover_ace*n_rare*(n_rare-1)), 0])  # 稀有物种的变异系数
    s_ace = s_high_abd + s_rare/cover_ace + F1*gamma_ace/cover_ace
    
    return s_ace