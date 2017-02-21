#! /usr/bin/python3
# -*- coding:utf-8 -*-

import numpy as np
import matplotlib.pyplot as plt
from math import pi,cos

def zero1(f, a, b, eps):
    """zero par dichotomie
    prec f continue, f(a) < 0, f(b) > 0, a < b"""
    m = 0.5*(a+b)
    if b-a < eps:
        return m
    elif f(m) < 0:
        return zero1(f, m, b, eps)
    else:
        return zero1(f, a, m, eps)

def zero2(f, a, b, eps):
    # inv : un zero de f est dans [a,b]
    # var : ent(log_2((b-a)/eps))
    while b-a > eps:
        m = (a+b)/2
        if f(m) < 0:
            a = m
        else:
            b = m
    return m

def zero3(f, a, b, eps):
    while b-a > eps:
        m = (a+b)/2
        if f(a)*f(m) > 0:
            a = m
        else:
            b = m
    return m

def fausse_position(f, a, b, eps):
    s = (a*f(b) - b*f(a))/(f(b)-f(a))
    while b-a > eps:
        s = (a*f(b) - b*f(a))/(f(b)-f(a))
        if f(a)*f(s) > 0:
            a = s
        else:
            b = s
    return s

def illinois(f, ls, apl, eps):
    
    s = (a*f(b) - b*f(a))/(f(b)-f(a))
    while abs(ls-apl) > eps:
        s = (a*f(b) - b*f(a))/(f(b)-f(a))
        
    

def test(appx,f,a,b,eps):
    plt.clf()
    z = appx(f,0,10,(1e-11))
    les_x = np.linspace(a,b,1000)
    les_y = [(f(x),0) for x in les_x]
    plt.plot(les_x,les_y)
    plt.plot([z],[0],"xc")
    plt.show()
    return z
    
test(fausse_position,lambda x: cos(x),-10,10,0.0001)
