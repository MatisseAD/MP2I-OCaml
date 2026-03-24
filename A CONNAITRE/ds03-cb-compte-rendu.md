# Tri topologie et chaîne CCMP

## Implémentation dans le sujet CCMP

- Chaque itération, on séléctionne un sommet de degré sortant nul
- On le numérote en lui donnant un numéro courant et on incrémente ce numéro courant
- On actualise le tableau des degrés sortants en les diminuant de 1 pour tous les degrés sortants des voisins de S.

(Voir GoodNotes pour la suite)

## Implémentation usuelle

- Le graphe est représenté par le tableau des liste de voisins entrants

__Exemple__ :

g = [| ...; i ; ... |]
g.(i) = [Voisins entrants de i]

- On met dans un sac / files tous les sommets de degré sortant nul

A CHAQUE ITERATION :

- On séléctionne un sommet de ce sac / file
- On le numérote en lui donnant un numéro courant et on incrémente ce numéro courant
- Pour tous les voisins de ce sommet, on diminue leur degré sortant de 1
- Si le degré sortant d'un voisin devient nul, on l'ajoute au sac / file

## Detecter si une liste d'éléments est une chaîne

Si les émelent de C forment une chaîne on peut les enfiler dans un TasMin

Si x est un élement qui forme une chaîne avec les éléments déjà présent, on peut insérer x en le mettant à la 1 ère feuille dispo et en vérifiant que la propriété de tas est respecté :

- Si le père est supérieur à x, on échange x avec son père
- Si le père est inférieur à x, on arrête
- Si le père et x ne sont pas comparables, on arrête

Si l'enfilage s'est bien passé

## Montrer qu'un algorithme n'existe pas pour une telle complexité

Supposons que nous avons un algorithme qui résout le problème en O(n)

- On peut construire une instance du problème à partir d'une instance d'un problème connu pour lequel il n'existe pas d'algorithme en O(n)
- On peut montrer que la solution de notre algorithme pour l'instance construite correspond à la solution de l'instance du problème connu
- On arrive à une contradiction, car cela impliquerait que nous avons un algorithme en O(n) pour le problème connu, ce qui est impossible.
