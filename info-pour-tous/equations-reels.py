#! /usr/bin/python3
# -*- coding:utf-8 -*-

def dichotomie(f,a,b,err):
    "recherche dichotomique"
    mil = (a+b)/2
    if b-a < err:
        return mil
    else:
        if f(a)*f(mil) > 0:
            return dichotomie(f,mil,b,err)
        else:
            return dichotomie(f,a,mil,err)

def newton(f,fp,u0,n):
    if n == 0:
        return u0
    return newton(f,fp,u0-f(u0)/fp(u0),n-1)
