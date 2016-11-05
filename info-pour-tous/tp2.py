#! /usr/bin/python3
# -*- coding:utf-8 -*-

"""TP2 ipt"""

from timeit import timeit
from matplotlib import pyplot as plt
from matplotlib import axes
from math import log

def fact_it(n):
    sortie = 1
    for i in range(1,n+1):
        sortie *= i
    return sortie

def fact_rec(n):
    if n <= 1:
        return 1
    return n * fact_rec(n-1)

def prod_range(a,b):
    if b <= a:
        return 1
    if b == a+1:
        return a
    m = (a+b)/2
    return prod_range(a,m)*prod_range(m,b)

def fact(n):
    return prod_range(1,n+1)

a,b,n = 1,10000,100

absc = range(a,b,n)
logab = [log(x) for x in absc]
vals_fact = [timeit(lambda:fact(n),number=1) for n in absc]
vals_fact_it = [timeit(lambda:fact_it(n),number=1) for n in absc]

print("calcul terminé")

plt.xkcd()

plt.clf()

fig = plt.figure()
ax = fig.add_subplot(111)


plt.plot(absc,vals_fact,"-b",label="fact")
plt.plot(absc,vals_fact_it,"-r",label="fact_it")
plt.title("fact "+str(a)+" "+str(b)+" "+str(n))
plt.legend()
plt.show()

