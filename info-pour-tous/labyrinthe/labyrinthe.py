#! /usr/bin/python3
# -*- coding:utf-8 -*-

import random

# Fonctions pou gérer les piles 
def creer_pile(c):
    return[]

def depiler(p):
    assert len(p) > 0
    return p.pop()

def empiler(p, v):
    p.append(v)

def sommet(p):
    assert len(p) > 0
    return p[-1]

def taille(p):
    return len(p)

def est_vide(p):
    return taille(p) == 0

#un exercice annexe : renvoyer la copie renversee d'une pile


def revdup(p):
    """prend en argument la pile p retourne une pile contenant
    p retournee sans modifier p"""
    n = taille(p)
    s,t = creer_pile(n),creer_pile(n)
    for i in range(n):
        v = depiler(p)
        empiler(s,v)
        empiler(t,v)
    for j in range(n):
        empiler(p,depiler(s))
    return t

#Fonctions utilisées lors de la génération du labyrinthe (recopiée du cours)
def visiter(c):
    (x,y) = c
    if x < 0 or x>= n or y<0 or y>=n:
        return
    atteinte[x][y] = True

def est_atteinte(c):
    (x,y) = c
    if x < 0 or x>= n or y<0 or y>=n:
        return True
    return atteinte[x][y]

def choix(c):
    (x,y) = c
    r = []
    def ajouter(p):
        if not est_atteinte(p):
            r.append(p)
    ajouter((x-1,y))
    ajouter((x+1,y))
    ajouter((x,y-1))
    ajouter((x,y+1))
    return r

def tirage(L):
    m = len(L)
    assert m > 0
    return L[random.randint(0, m-1)]

def labyrinthe():
    # Création de lislab, tableau de dimensions (2*n+1)
    #contenant la représentation du labyrinthe
    
    lislab = [[0]*(2*n+1) for i in range(2*n + 1)]
    
    for i in range(n):
        for j in range(n):
            lislab[2*i+1][2*j+1] = blanc
            
    pile = creer_pile(n*n)
    chemin = creer_pile(n*n)
    
    empiler(pile, (0,0))
    visiter((0,0))
    
    while not est_vide(pile):
        cellule = depiler(pile)
        #construction du chemin a la volee
        if cellule == (n-1,n-1) and est_vide(chemin):
            empiler(pile,cellule)
            chemin = revdup(pile)
            cellule = depiler(pile)

        #print(cellule)
        c = choix(cellule)
        if len(c) > 0:
            suivante = tirage(c)
            # Lie les deux cases
            xc,yc = cellule
            xs,ys = suivante
            lislab[xc+xs+1][yc+ys+1]=blanc
            visiter(suivante)
            ## grace aux deux lignes suivantes :
            ## si l'etat de la pile est
            ## | ... |
            ## | c 2 | ^^^ (top)
            ## | c 1 |
            ## | ... |
            ## alors c1 est adjacence a c2, ainsi : l'unique chemin allant de
            ## c1 a c2 est [c1,c2]
            ## et par récurrence : si la pile est
            ## |  c n  |
            ## | c n-1 |
            ## |  ...  |
            ## |  c 1  | ^^^ (top)
            ## |  c 0  |
            ## le chemin allant de c0 a cn est [c0,c1,...,cn]
            ## or c0 = (0,0), donc, pour cn = (n-1,n-1) le chemin est la pile
            empiler(pile, cellule)
            empiler(pile, suivante)

    #ajout du chemin a lislab
            
    xp,yp = depiler(chemin)
    lislab[2*xp+1][2*yp+1] = gris
    while not est_vide(chemin):
        (x,y) = depiler(chemin)
        lislab[2*x+1][2*y+1] = gris
        lislab[x+xp+1][y+yp+1] = gris
        xp,yp = x,y
    return lislab

##variables importantes

n = 20

gris = 12 #quelques couleurs...
blanc = 15

atteinte = [[False] * n for i in range(n)]

def save_lab(fichier, lislab):
    """Enregistre le labyrinthe en pgm"""
    #conversion des éléments de lislab en str
    long = len(lislab)
    for i in range(long):
        for j in range(long):
            lislab[i][j] = str(lislab[i][j])
    with open(fichier, "w") as f:
        f.write("P2\n"+str(long)+' '+str(long)+'\n'+str(blanc)+'\n')
        for i in lislab:
            f.write(' '.join(i)+'\n')

def scale(p,t):
    """multiplie par p le tableau représentant le bitmap"""
    l,w = len(t)*p,len(t[0])*p
    return [[t[x//p][y//p]for y in range(w)] for x in range(l)]

if __name__=="__main__":
    save_lab(input("fichier de sortie ?\t"),scale(20,labyrinthe()))
