# Parcours de graphes

On veut appliquer une fonction f à tous les sommets d'un graphe (connexe)

Exemple de graphe :

```
   A
  / \
 B   C
  \ /
   D
```

## Algo générique de parcours

On dispose d'un "sac" qui contient les sommets à visiter d'une structure qui contient les sommets déjà visités.

__**Algo**__ :

```
Mettre le sommet source dans le sac

WHILE le sac n'est pas vide
  Prendre un sommet du sac
  SI ce sommet n'est pas dans la structure des sommets visités
    Appliquer f à ce sommet
    Ajouter ce sommet à la structure des sommets visités
    Mettre tous les voisins de ce sommet dans le sac
  SINON
    Passer au sommet suivant (On fait rien)
```
