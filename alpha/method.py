def Chao1(value_list):  # 顺便返回 S_obs
    S_obs = sum(1 for q in value_list if q != 0)
    F1 = sum(1 for q in value_list if q == 1)
    F2 = sum(1 for q in value_list if q == 2)
    S_chao1 = S_obs + F1*( F1 -1 )/2*( F2 + 1 )
    return S_obs, S_chao1


def ACE(value_list, rare_tag_threshold=10):
    num_rare = sum(1 for q in value_list if q <= rare_tag_threshold and q > 0) #  默认 小于 10个 标签的 认为是 稀有物种
    F1 = sum(1 for q in value_list if q == 1) # F1 为 仅有一个标签的物种
    cover_ace = 1 - F1/num_rare   # cover_ace 为样本覆盖度的估计值
    return 0