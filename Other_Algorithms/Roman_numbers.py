
# Given an integer, return the corresponding Roman number:

# Support N* (positive natural numbers without including zero)

# Ddictionary of correspondences:

from _typeshed import IdentityFunction
from os import CLD_DUMPED, XATTR_SIZE_MAX
from typing import ValuesView


corr = {1: 'I',
        5: 'V',
        10: 'X',
        50: 'L',
        100: 'C', 
        500: 'D',
        1000: 'M'}

cor_list = ['I', 'V', 'X', 'L', 'C', 'D', 'M']

# Example:

# 675 (DCLXXV)

input = 675

num_str = [leter for leter in str(input)]
num_str

num_int = [int(num) for num in str(input)]
num_int

ind_exp = []
for i in range(len(num_str)):
    ind_exp.append((len(num_str) - i)-1)

ind_exp

segs = []
for i in range(len(num_str)):
    segs.append(int(num_str[i]) * (10 ** ind_exp[i]))

segs

corr.get(list(corr)[4 -1])

level = []
for i in range(10):
    print(i)
    print(level)
    if i == 0:
        level.append(corr.get(10 ** ind_exp[0]))
    elif 0 < i <= 3:
        level.append(corr.get(10 ** ind_exp[0]) + level[i-1])
    elif i == 4:
        level.append(corr.get(list(corr)[4-1])) + corr.get(list(corr)[4])
    elif i == 5:
        level.append(corr.get(list(corr)[4]))
    elif 5 < i <= 8:
        level.append(corr.get(list(corr)[4]) + level[0])
    elif i == 9:


            

    




level = [corr.get(10 ** ind_exp[0]) for n in range(10)]
level


input
segs
ind_exp


cor_list = ['I', 'V', 'X', 'L', 'C', 'D', 'M']
i=0
output = []




1 = I str(0)
2 = II str(0) + str(0)
3 = III str(0) + str(0)
4 = IV str(0) + str(1)
5 = V str(1)
6 = VI str(1) + str(0)
7 = VII str(1) + str(0) + str(0)
8 = VIII str(1) + str(0) + str(0) + str(0)
9 = IX str(0) + str(2)


20 = XX
30 = XXX
40 = XL
50 = L
60 = LX
70 = LXX
80 = LXXX
90 = XC



100 = C
200 = CC
300 = CCC
400 = CD
500 = D
600 = DC
700 = DCC
800 = DCCC
900 = CM









