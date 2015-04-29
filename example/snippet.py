import math


def binomialCoeff(n, k):
    result = 1
    for i in range(1, k+1):
        result = result * (n-i+1) / i
    return result


def n_0(n):
    return int(math.floor(n / 4.0))


def N(n, M):
    n_sub_zero = n_0(n)
    sigma = n + n_sub_zero + 1
    division = math.log(10) / math.log(2 * math.e * M)
    return int(math.ceil(sigma * division))


def B(n, M):
    big_N = N(n, M)
    result = 0
    for k in range((M + 1) * (big_N - 1)):
        dividend = math.pow(-1, k) * (4 * math.pow(10, n) % (2 * (k + 1)))
        result += dividend / (2 * (k + 1))
    return result


def s(k, M, N):
    return sum(binomialCoeff(N, j) % (2 * M * N + 2 * k + 1) for j in range(k))


def C(n, M):
    big_N = N(n, M)
    result = 0
    for k in range(big_N - 1):
        dividend = math.pow(-1, k) * math.pow(5, big_N - 2) * \
            math.pow(10, n - big_N + 2) * s(k, M,  big_N) \
            % (2 * M * big_N + 2 * k + 1)
        result += dividend / (2 * M * big_N + 2 * k + 1)
    return result
