
# Algorithm to get the nth term of the Fibonacci series, in python:

# 0, 1, 1, 2, 3, 5, 8, 13 ...

## Considering F_0 = 0 and F_1 = 1

def fib_n(n):

    x = [0, 1]
    i = 0

    while i < n+1:
        if i > 1:
            x.append(x[i-1] + x[i-2])
        i = i + 1

    return x[n]

fib_n(20)