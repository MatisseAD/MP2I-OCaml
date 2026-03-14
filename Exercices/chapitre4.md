# Exercices

## Exercice 4

### Q1

*Parmi les permutations suivantes, lesquelles peuvent être engendrées par une pile ?*  
(3, 1, 2), (3, 4, 2, 1), (4, 5, 3, 7, 2, 1, 6), (3, 5, 7, 6, 8, 4, 9, 2, 10, 1)

#### Essayons (3, 1, 2)

| Pile     | Sortie | Opérations |
|----------|--------|------------|
| []       |        |            |
| [1]      |        |            |
| [2; 1]   |        |            |

Même en ajoutant 3 et en le dépilant, on ne pourra jamais avoir 3 puis 1 à la suite.  
**Conclusion :** cette permutation ne peut pas être générée par une pile.

#### Essayons (3, 4, 2, 1)

| Pile        | Sortie   | Opérations |
|-------------|----------|------------|
| []          |          |            |
| [1]         |          | E          |
| [2; 1]      |          | E          |
| [3; 2; 1]   |          | E          |
| [2; 1]      | (3)      | D          |
| [4; 2; 1]   | (3)      | E          |
| [2; 1]      | (3, 4)   | D          |

Puis on dépile le reste.

#### Essayons (4, 5, 3, 7, 2, 1, 6)

| Pile         | Sortie         | Opérations |
|--------------|----------------|------------|
| []           |                |            |
| [1]          |                | E          |
| [2; 1]       |                | E          |
| [3; 2; 1]    |                | E          |
| [4; 3; 2; 1] |                | E          |
| [3; 2; 1]    | (4)            | D          |
| [5; 3; 2; 1] | (4)            | E          |
| [3; 2; 1]    | (4, 5)         | D          |
| [2; 1]       | (4, 5, 3)      | D          |
| [6; 2; 1]    | (4, 5, 3)      | E          |
| [7; 6; 2; 1] | (4, 5, 3)      | E          |
| [6; 2; 1]    | (4, 5, 3, 7)   | D          |

La suite est impossible à réaliser, car on ne peut pas dépiler 6 avant d'avoir dépilé 7.  
**Conclusion :** cette permutation ne peut pas être générée par une pile.

#### Essayons (3, 5, 7, 6, 8, 4, 9, 2, 10, 1)

| Pile             | Sortie                     | Opérations |
|------------------|---------------------------|------------|
| []               |                           |            |
| [1]              |                           | E          |
| [2; 1]           |                           | E          |
| [3; 2; 1]        |                           | E          |
| [2; 1]           | (3)                       | D          |
| [5; 4; 2; 1]     | (3)                       | E * 2      |
| [4; 2; 1]        | (3, 5)                    | D          |
| [6; 4; 2; 1]     | (3, 5)                    | E          |
| [7; 6; 4; 2; 1]  | (3, 5)                    | E          |
| [6; 4; 2; 1]     | (3, 5, 7)                 | D          |
| [4; 2; 1]        | (3, 5, 7, 6)              | D          |
| [8; 4; 2; 1]     | (3, 5, 7)                 | E          |
| [4; 2; 1]        | (3, 5, 7, 8)              | D          |
| [2; 1]           | (3, 5, 7, 8, 4)           | D          |
| [9; 2; 1]        | (3, 5, 7, 8, 4)           | E          |
| [2; 1]           | (3, 5, 7, 8, 4, 9)        | D          |
| [1]              | (3, 5, 7, 8, 4, 9, 2)     | D          |
| [10; 1]          | (3, 5, 7, 8, 4, 9, 2)     | E          |
| [1]              | (3, 5, 7, 8, 4, 9, 2, 10) | D          |
| []               | (3, 5, 7, 8, 4, 9, 2, 10, 1) | D      |

**Conclusion :** cette permutation peut être générée par une pile.
