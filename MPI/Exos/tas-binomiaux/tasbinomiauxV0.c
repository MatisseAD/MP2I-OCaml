#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#include "tasbinomiaux.h"

arbrebin *init_arbrebin(int valeur, int priorite) {
    arbrebin *a = malloc(sizeof(arbrebin));
    a->valeur = valeur;
    a->ordre = 0;
    a->priorite = priorite;
    a->pere = NULL;
    a->frere = NULL;
    a->aine = NULL;
    return (a);
}

// On suppose que les tas ne sont pas triés par ordre croissant

arbrebin *fusion_arbrebin(arbrebin *p, arbrebin *f) {
    // fusion de deux arbres du même ordre : on attache l'arbre f à p, où p est issu du tas
    if (p->priorite < f->priorite) {
        f->frere = NULL;
        f->pere = p;
        p->ordre = p->ordre + 1;
        arbrebin *current = p->aine;
        if (current != NULL) {
            while (current->frere != NULL) {
                current = current->frere;
            }
            current->frere = f;
        } else {
            p->aine = f;
        }
        return p;
    } else {
        p->frere = NULL;
        p->pere = f;
        f->ordre = f->ordre + 1;
        arbrebin *current = f->aine;
        if (current != NULL) {
            while (current->frere != NULL) {
                current = current->frere;
            }
            current->frere = p;
        } else {
            f->aine = p;
        }

        return f;
    }
}

arbrebin *check_meme_ordre(arbrebin *t, arbrebin *a) {
    // Cette fonction permet de vérifier si il existe deux arbres de même ordre dans un tas
    arbrebin *current = t;

    // On test la tête du tas

    if (current->ordre == a->ordre && current != a) {
        arbrebin *next_maillon = current->frere;
        t = fusion_arbrebin(a, current);
        t->frere = next_maillon;
        return check_meme_ordre(t, t);
    }


    while (current->frere != NULL) {
        if ((current->frere)->ordre == a->ordre && (current->frere) != a) {
            arbrebin *next_maillon = (current->frere)->frere;
            current->frere = fusion_arbrebin(a, current->frere);
            (current->frere)->frere = next_maillon;
            return check_meme_ordre(t, current->frere);
        }

        current = current->frere;
    }

    return t;
}


arbrebin *inserer_arbrebin(arbrebin *t, arbrebin *a) {
    // insere l'arbre seul a dans le tas t
    // On suppose que t pointe directement vers la racine la plus à gauche des arbres

    if (t->frere == NULL && t->ordre != a->ordre) {
        t->frere = a;
        return t;
    } else if (t->frere == NULL && t->ordre == a->ordre) {
        // Cas où il y a un seul arbre dans le tas
        return fusion_arbrebin(t, a);
    } else {
        arbrebin *frere = t;

        while (frere->frere != NULL && frere->ordre != a->ordre) {
            frere = frere->frere;
        }

        if (frere->ordre == a->ordre) {
            frere = fusion_arbrebin(frere, a);
            // On enleve frere de l'arbre et on vérifie si tout les ordes sont distincts
            return check_meme_ordre(t, frere);
        } else {
            frere->frere = a;
            return t;
        }
    }
}

arbrebin *inserer_valeur_prior(arbrebin *tas, int v, int p) {
    arbrebin *arbre = init_arbrebin(v, p);
    return (inserer_arbrebin(tas, arbre));
}

void impression_arbrebin(arbrebin *arbre) {
    if (arbre != NULL) {
        printf("%d %d ", arbre->valeur, arbre->priorite);
        impression_arbrebin(arbre->frere);
        printf("\n");
        impression_arbrebin(arbre->aine);
    };
}

void impression_tasbin(arbrebin *tas) {
    if (tas != NULL) {
        printf("%d %d \n", tas->valeur, tas->priorite);
        impression_arbrebin(tas->aine);
        printf("arbre suivant \n");
        impression_tasbin(tas->frere);
    };
}


void impression_ordre(arbrebin *tas) {
    // On suppose que l'odre d'un tas est le max des ordres des arbres contenus dans le tas

    int max_ordre = 0;

    arbrebin *a = tas;

    while (a != NULL) {
        int ordre_a = a->ordre;

        if (ordre_a > max_ordre) {
            max_ordre = ordre_a;
        }

        a = a->frere;
    }

    printf("%d", max_ordre);
}

int recherche_priormin(arbrebin *tas, int *p) {
    // Priormin -> Comme chaque priorité d'un noeud est supérieur à celle de son père, il suffit de comparer la valeur de chaque racine des arbres

    arbrebin *a = tas;

    int prio_min = a->priorite;

    while (a != NULL) {
        if (a->priorite < prio_min) {
            prio_min = a->priorite;
        }
        a = a->frere;
    }

    *p = prio_min;
    return prio_min;
}

void afficherArbre(arbrebin *noeud, int profondeur) {
    if (noeud == NULL)
        return;

    for (int i = 0; i < profondeur; i++)
        printf("    ");
    printf("- valeur=%d (priorite=%d)\n", noeud->valeur, noeud->priorite);

    arbrebin *enfant = noeud->aine;
    while (enfant != NULL) {
        afficherArbre(enfant, profondeur + 1);
        enfant = enfant->frere;
    }
}

/* Affiche le tas entier : chaque racine correspond à un arbre
   binomial B_k, affiché séparément avec son ordre. */
void afficherTas(arbrebin *tas) {
    if (tas == NULL) {
        printf("Le tas est vide.\n");
        return;
    }

    printf("=== Tas binomial ===\n");
    arbrebin *racine = tas;
    while (racine != NULL) {
        printf("\nArbre B%d (racine : valeur=%d, priorite=%d) :\n",
               racine->ordre, racine->valeur, racine->priorite);
        afficherArbre(racine, 0);
        racine = racine->frere;
    }
    printf("\n====================\n");
}

arbrebin *fusion_tas(arbrebin *tas1, arbrebin *tas2) {
    arbrebin *t = tas1;
    while (t != NULL) {
        inserer_arbrebin(t, tas2);
        t = t->frere;
    }
    return tas2;
}
// fusion de deux tas en un seul
// algo1 : par insertion des arbres de l'un des tas dans l'autre

int main(void) {
    int t[10] = {5, 7, 2, 8, 9, 10, 1, 4, 6, 3};

    int N = 10;

    arbrebin *tas = init_arbrebin(t[0], 0);

    for (int i = 1; i < N; i++) {
        tas = inserer_valeur_prior(tas, t[i], i % 6);
    }

    afficherTas(tas);

    impression_ordre(tas);

    int p = -1;
    printf("\n %d %d", recherche_priormin(tas, &p), p);

    return (0);
}
