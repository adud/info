#! /usr/bin/lib/python3
# -*- coding:utf-8 -*-

import matplotlib.pyplot as plt
from math import log
from numpy import array,ones,random,linalg

fichier = input("fichier ?\n")

with open(fichier,"r") as f:
    absc = []
    ordon = []
    ligne = f.readline()
    while ligne:
        ligne = ligne.strip()
        ligne = ligne.split(",")
        ligne = [float(x) for x in ligne]
        absc.append(ligne[0])
        ordon.append(ligne[1])
        ligne = f.readline()

#absc = array([log(x) for x in absc])
#ordon = array([log(y) for y in ordon])

A = array((absc,ones(len(absc))))
w = linalg.lstsq(A.T,ordon)[0]

line = absc*w[0] + w[1]

plt.clf()
plt.plot(absc,ordon,".r")#,label="ln(t)=ln(a)+b*ln(n)")
plt.plot(absc,line,"-b")#,label="y="+str(w[0])+"x+"+str(w[1]))
#plt.legend(bbox_to_anchor=(0., 1.02, 1., .102),ncol = 2,loc = 3)
plt.show()
