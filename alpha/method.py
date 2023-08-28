def Chao1(value_list):  # 顺便返回 S_obs
    S_obs = sum(1 for q in value_list if q != 0)
    F1 = sum(1 for q in value_list if q == 1)
    F2 = sum(1 for q in value_list if q == 2)
    S_chao1 = S_obs + F1*( F1 -1 )/2*( F2 + 1 )
    return S_obs, S_chao1


def ACE(value_list):
    return 0