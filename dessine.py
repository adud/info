#! /Users/antonin/miniconda3/bin/python3
# -*- coding:utf-8 -*-

from math import log
import matplotlib.pyplot as plt
from sys import argv

n = len(argv)

if n==1:print("pas de fichier")
elif n>=2:
    fichier = argv[1]
    with open(fichier) as fich:
        source = fich.readlines()
        absc = []
        ordon = []
        for ligne in source:
            l = ligne.strip()
            l = l.split(",")
            absc.append(log(float(l[0])))
            ordon.append(log(float(l[1])))

    
    plt.clf()
    simul = [2*x - 15.5 for x in absc]
    plt.plot(absc,ordon)
    plt.plot(absc,simul)
    plt.xlabel("ln(n)")
    plt.ylabel("temps d'exécution sur un tableau de taille n")
    plt.title("expérimentation de insere")
    if n==2:
        plt.show()
    if n==3:
        plt.savefig(argv[2])
