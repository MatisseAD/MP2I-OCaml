# Implémentation d'un dictionnaire

- Structure qui construit de couples (clef, valeur)

On veut pouvoir faire facilement les opérations suivantes :

- Créer un dictionnaire vide
- Vérifier si une clef est présente dans le dictionnaire
- Ajouter une association (clef, valeur) dans le dictionnaire
- Sortie une valeur à partir d'une clef

__Exemple__ :  Le Larousse est un dictionnaire qui associe à chaque mot une définition, (mot, définition) est un couple (clef, valeur)

## Implémentation

__Idée__ : On utilise un tableau

- Les mots possibles ont moins de 26 lettres
- On établit alors une bijection entre les mots a priori possible vers 26^26

__Problème__ : On obitent un tableau dont la plus part des cases sont vides

__Amélioration d'idée__ : Soit N = nb de mots du dictionnaire

- On crée une fonctuin h (pas tout a fait bijective mais presque) de l'ensemble des mots possibles vers [0, 1, ..., 3N[

Donc,

dico = [..........] avec par exemple dico.(h(informatique)) = 'science de l'algorithme ... '
