# Feuille réponse

## Question 1

Jeu instatisfiable : On met toute les clefs dans la salle fin, ainsi le joueur ne peut ouvrir aucun verrou, donc insatisfiable

Jeu satisfiable : On met toute les clefs dans la case début. Ainsi, le joueur a tout les clefs dans son inventaire.

## Question 3

On pose l'invariant de boucle suivant :

H(i) "Au début de la ième itération, les éléments de T[0;i[ ont subit, au moins une permutation, quitte à ne pas changer T[i] de sa position i."

C'est un invariant de boucle, car

- Il est vrai avant d'entrer dans la boucle (Les élémennts compris entre T[0;0[ ont tous, au moins, subit une permutation)

## Question 6

On effectue un tri croissant du tableau :

- Tri fusion
  Complexité : O(nlog(n))
  Explication : On divise le tableau en deux pour faire des sous problèmes, et trier les tableaux
- Tri par insertion
  Complexité : O(n²)
  Explication : On garde le tableau trié d'un côté puis de l'autre on regarde l'élément le plus petit pour l'ajouter à la suite du tableau trié

## Question 7

Première règle :

Soit une case k qui contient une clef i :

N(k) / O(i)

Deuxième règle :

Soit k1,k2 les indices de deux cases tq a=(k1,W,k2) est une arête. On note W = {c1,...,cn} l'ensemble des clefs nécessaire pour aller au noeud k2 depuis k1.

O(c1) ^ O(c2) ^ ... ^ O(cn) ^ N(k1) / N(k2)

## Question 19

L'état initial correspond aux éléments suivants :

- Noeud = 0
- Temps = 0
- Inventaire = [None; None; None]

Pour franchir le verrou V1 :

On va de case début vers la case où il y a les clefs C0 et C1 (que l'on suppose être le noeud 1)

EF :

- Noeud = 1
- Temps = 10
- Inventaire = [None; None; None]

On récupère la clef 1 :

- Noeud = 1
- Temps = 80
- Inventaire = [None; Some 70; None]

On ouvre le verrou V1 et on va vers le noeud qui suit le verrou V1 (que l'on désigne par le noeud 2)

EF :

- Noeud = 2
- Temps = 90
- Inventaire = [None; Some 70; None]

## Question 20

L'alogrithme de djikstra. Il consite à utiliser une file de priorité et à chaque exploration d'un nouveau noeud non insérer dans la file de priorité d'un graphe, on l'ajoute avec son coût. Ensuite, on tire le somme ayant le coût minimal (donc le premier de la file). Puis on forme un chemin de coût minimal avec ce principe

Complexité : O(|S| + |A|) avec S : nombre de sommets du graphe
                          avec A : nombre d'arrêtes du graphe
