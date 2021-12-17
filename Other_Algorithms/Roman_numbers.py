
# Given an integer, return the corresponding Roman number:

# Support N* (positive natural numbers without including zero)

def get_roman(input):

    if((not isinstance(input, int)) or (input <= 0)) or (input > 3999999):
        exit('Input must be a positive, bigger than 0 or lower than 3999999')

    # Dictionary of correspondences:
    corr = {1: 'I',
            5: 'V',
            10: 'X',
            50: 'L',
            100: 'C', 
            500: 'D',
            1000: 'M',
            5000: '_V',
            10000: '_X',
            50000: '_L',
            100000: '_C',
            500000: '_D',
            1000000: '_M'}
    # List of separated numbers in string format of the original integer:
    num_str = [leter for leter in str(input)]

    # List of separated numbers in integer format of the original integer:
    num_int = [int(num) for num in str(input)]

    # List of decreasing Naturals:
    ind_exp = []
    for i in range(len(num_str)):
        ind_exp.append((len(num_str) - i)-1)

    # List of decreasing decimal system numbers asociated with the original number: 
    segs = []
    for i in range(len(num_str)):
        segs.append(int(num_str[i]) * (10 ** ind_exp[i]))

    fin = []
    for i in range(len(ind_exp)):
        output = []
        if(num_int[i]!= 0):
            
            output.append(str(corr.get(10 ** ind_exp[i])))
            output.append(str(corr.get(10 ** ind_exp[i]))+str(corr.get(10 ** ind_exp[i])))
            output.append(str(corr.get(10 ** ind_exp[i]))+str(corr.get(10 ** ind_exp[i]))+str(corr.get(10 ** ind_exp[i])))
            output.append(str(corr.get(10 ** ind_exp[i]))+str(corr.get(10 ** ind_exp[i]*5)))
            output.append(str(corr.get(10 ** ind_exp[i]*5)))
            output.append(str(corr.get(10 ** ind_exp[i]*5))+str(corr.get(10 ** ind_exp[i])))
            output.append(str(corr.get(10 ** ind_exp[i]*5))+str(corr.get(10 ** ind_exp[i]))+str(corr.get(10 ** ind_exp[i])))
            output.append(str(corr.get(10 ** ind_exp[i]*5))+str(corr.get(10 ** ind_exp[i]))+str(corr.get(10 ** ind_exp[i]))+str(corr.get(10 ** ind_exp[i])))
            output.append(str(corr.get(10 ** ind_exp[i]))+str(corr.get(10 ** ind_exp[i]*10)))
            fin.append(output[num_int[i]-1])
        else:
            fin.append('')

    return(''.join(fin))

# Example 654 = DCLIV:
get_roman(654)
