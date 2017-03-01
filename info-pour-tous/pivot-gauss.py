#! /usr/bin/python3
# -*- coding:utf-8 -*-

def echange(t,l1,l2):
    "echange les lignes no l1 et l2 du tableau t en place"
    t[l1],t[l2] = t[l2],t[l1]

def comblin(t,l1,l2,alpha):
    "t[l1] <- t[l1] + alpha*t[l2] transvection"
    for i,v in enumerate(t[l2]):
        t[l1][i] += v*alpha

def cherche_pivot(t,i):
    "cherche le meilleur pivot pour t a l'etape i"
    nbl = len(t)
    c = i
    for k in range(i+1,nbl):
        if abs(t[c][i]) < abs(t[k][i]):
            c = k
    return c

def triangule(A,Y):
    nbl = len(A)
    for i in range(nbl):
        j = cherche_pivot(A,i)
        echange(A,i,j)
        echange(Y,i,j)
        for k in range(i+1, nbl):
            fact = -A[k][i]/A[i][i]
            comblin(A,k,i,fact)
            comblin(Y,k,i,fact)

def remonte(A,Y):
    "donne la solution du systeme triangulaire AX=Y"
    nbl = len(A)
    X = [0.]*nbl
    for i in range(nbl-1,-1,-1):
        X[i] = (Y[i][0] - sum(A[i][k]*X[k] for k in range(i+1,nbl)))/A[i][i]
    return X

def pivot_gauss(A,Y):
    triangule(A,Y)
    return remonte(A,Y)
