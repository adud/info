#! /usr/bin/python3
# -*- coding:utf-8 -*-

"""texte LaTeX genere"""

for i in range(9):
    x = i%3
    y = i//3
    print("\t\\draw ({0},{1}) ++ (0.5,0.5) node ".format(x,y)\
          + "{"+str(i+1) +"};")
