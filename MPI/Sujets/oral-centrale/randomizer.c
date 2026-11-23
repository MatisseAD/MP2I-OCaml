#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include <assert.h>


// Structure pour une proposition
struct prop_ {
    bool N;// Booléen indiquant si c'est une proposition N_i ou O_i
    int i; // Indice i associé à la proposition
};
typedef struct prop_ prop;

// Structure pour une règle reliée aux règles suivantes.
struct regle_ {
    struct regle_ *suivant;
    prop *premisse;
    int taille_premisse;
    prop conclusion;
};
typedef struct regle_ regle;


//////////////////////////////////////////////////////////////////////
// Définition du jeu.

// Libération des règles restantes.
void libere_regles(regle *r) {
    // Attention à bien libérer la premisse.
    if (r != NULL) {
        libere_regles(r->suivant);
        free(r->premisse);
        free(r);
    }
}


//////////////////////////////////////////////////////////////////////
// Generation d'une permutation

void init_i(int *tableau, int k) {
    for (int i = 0; i < k; i++) {
        tableau[i] = i;
    }
}

void pp_permutation(int *permutation, int k) {
    // On écrit le tableau des permutations
    printf("Permutations : \n");

    for (int i = 0; i < k; i++) {
        printf("%d", permutation[i]);
    }
}

int generateAleatoire(int k) {
    return rand() % (k + 1);
}

void melange(int *tableau, int k) {

    for (int i = 0; i < k; i++) {
        int pivot = generateAleatoire(i);
        int temp = tableau[pivot];
        tableau[pivot] = tableau[i];
        tableau[i] = temp;
    }
}

//////////////////////////////////////////////////////////////////////
// Règles de déduction

regle *ajoute_regles_clefs(regle *r, int n1, int *clefs, int nb_clefs) {
    prop ans;
    ans.N = false;
    ans.i = n1;
    prop *premisse_ans = malloc(sizeof(prop));
    int taille_premisse_ans = 1;
    premisse_ans[0] = ans;

    regle *new = malloc(sizeof(regle));

    for (int i = 0; i < nb_clefs; i++) {
        prop conclusion_ans;
        conclusion_ans.N = true;
        conclusion_ans.i = clefs[i];

        new->premisse = premisse_ans;
        new->taille_premisse = taille_premisse_ans;
        new->conclusion = conclusion_ans;
        new->suivant = r;
        r = new;

        regle *temp = malloc(sizeof(regle));
        new = temp;
    }

    return new;
}

regle *ajoute_regles_arete(regle *r, int n1, int n2, int *verr, int nb_verr) {
    int taille_premisse = (nb_verr + 1);

    // Première règle : de n1 vers n2

    regle *r1 = malloc(sizeof(regle));
    prop *premisse1 = malloc(sizeof(prop) * taille_premisse);
    premisse1[0].N = false;
    premisse1[0].i = n1;

    for (int i = 1; i < taille_premisse; i++) {
        premisse1[i].N = true;
        premisse1[i].i = verr[i];
    }

    r1->conclusion = {.N = false, .i = n2};
    r1->taille_premisse = taille_premisse;
    r1->premisse = premisse1;
    r1->suivant = r;

    // Première règle : de n2 vers n1

    regle *r2 = malloc(sizeof(regle));
    prop *premisse2 = malloc(sizeof(prop) * taille_premisse);
    premisse2[0].N = false;
    premisse2[0].i = n2;

    for (int i = 1; i < taille_premisse; i++) {
        premisse2[i].N = true;
        premisse2[i].i = verr[i];
    }

    r2->conclusion = {.N = false, .i = n1};
    r2->taille_premisse = taille_premisse;
    r2->premisse = premisse2;
    r2->suivant = r1;
}

void lire_regle(regle r) {
    printf("Les pré-requis de la règle sont : \n");
    int taille = r->taille_premisse;
    prop *premisse = r->premisse;
    for (int i = 0; i < taille; i++) {
        printf("Propositon %d : \n", i);
        printf("Booléen : %d \n", premisse[i].N);
        printf("Indice associé : %d \n", premisse[i].i);
    }
    printf("La conclusion de la règle est : \n");
    printf("Booléen : %d \n", r.conclusion.N);
    prinft("Indice associé : %d \n", r.conclusion.i);
    prinft("\n");
}

void lire_regles(regle *r) {
    while (r->suivant != NULL) {
        lire_regle(*r);
        r = r->suivant;
    }

    lire_regle(*r);
}

void init_faux(bool *tableau, int taille_tableau) {
    for (int i = 0; i < taille_tableau; i++) {
        tableau[i] = false;
    }
}

bool verifie_premisse(regle r, bool *noeuds, bool *clefs) {
    bool ans = true;

    int taille_premisse = r.premisse;
    prop *premisse = r->premisse;
    for (int i = 0; i < taille_premisse; i++) {
        if (premisse[i].N) {
            if (!clefs[premisse[i].i]) {
                ans = false;
            }
        } else {
            if (!noeuds[premisse[i].i]) {
                ans = false;
            }
        }
    }

    return ans;
}

regle *parcours_regles(regle *r, bool *noeuds, bool *clefs, bool *progres) {
    if (r == NULL) return NULL;

    if (verifie_premisse(r, noeuds, clefs)) {
        *progres = true;

        if (r->conclusion.N) {
            clefs[r->conclusion.i] = true;
        } else {
            noeuds[r->conclusion.i] = true;
        }

        regle *suite = r->suivant;
        libere_regles(r);
        return parcours_regles(suite, noeuds, clefs, progres);
    }

    r->suivant = parcours_regles(r->suivant, noeuds, clefs, progres);
    return r;
}

bool est_sat(regle *r, int nb_noeuds, int nb_clefs, int D, int F) {

    bool *noeuds = malloc(sizeof(bool) * nb_noeuds);
    init_faux(noeuds, nb_noeuds);
    bool *clefs = malloc(sizeof(bool) * nb_clefs);
    init_faux(clefs, nb_clefs);
    bool *progres = malloc(sizeof(bool));
    *progres = false;

    noeuds[D] = true;

    while (*progres) {
        parcours_regles(r, noeuds, clefs, progres);
    }


    bool ans = noeuds[F];

    regle *current = r;

    while (current->suivant != NULL) {
        regle *temp = current;
        current = current->suivant;
        libere_regles(temp);
    }

    libere_regles(current);

    return ans;
}

//////////////////////////////////////////////////////////////////////
// Avec permutation

regle *ajoute_regles_clefs_sigma(regle *r, int n1, int *clefs, int nb_clefs, int *sigma) {
    prop ans;
    ans.N = false;
    ans.i = n1;
    prop *premisse_ans = malloc(sizeof(prop));
    int taille_premisse_ans = 1;
    premisse_ans[0] = ans;

    regle *new = malloc(sizeof(regle));

    for (int i = 0; i < nb_clefs; i++) {
        prop conclusion_ans;
        conclusion_ans.N = true;
        conclusion_ans.i = clefs[sigma[i]];

        new->premisse = premisse_ans;
        new->taille_premisse = taille_premisse_ans;
        new->conclusion = conclusion_ans;
        new->suivant = r;
        r = new;

        regle *temp = malloc(sizeof(regle));
        new = temp;
    }

    return new;
}

regle *ajoute_regles_arete(regle *r, int n1, int n2, int *verr, int nb_verr, int *sigma) {
    int taille_premisse = (nb_verr + 1);

    // Première règle : de n1 vers n2

    regle *r1 = malloc(sizeof(regle));
    prop *premisse1 = malloc(sizeof(prop) * taille_premisse);
    premisse1[0].N = false;
    premisse1[0].i = n1;

    for (int i = 1; i < taille_premisse; i++) {
        premisse1[i].N = true;
        premisse1[i].i = verr[i];
    }

    r1->conclusion = {.N = false, .i = n2};
    r1->taille_premisse = taille_premisse;
    r1->premisse = premisse1;
    r1->suivant = r;

    // Première règle : de n2 vers n1

    regle *r2 = malloc(sizeof(regle));
    prop *premisse2 = malloc(sizeof(prop) * taille_premisse);
    premisse2[0].N = false;
    premisse2[0].i = n2;

    for (int i = 1; i < taille_premisse; i++) {
        premisse2[i].N = true;
        premisse2[i].i = verr[i];
    }

    r2->conclusion = {.N = false, .i = n1};
    r2->taille_premisse = taille_premisse;
    r2->premisse = premisse2;
    r2->suivant = r1;
}

void lire_regle(regle r) {
    printf("Les pré-requis de la règle sont : \n");
    int taille = r->taille_premisse;
    prop *premisse = r->premisse;
    for (int i = 0; i < taille; i++) {
        printf("Propositon %d : \n", i);
        printf("Booléen : %d \n", premisse[i].N);
        printf("Indice associé : %d \n", premisse[i].i);
    }
    printf("La conclusion de la règle est : \n");
    printf("Booléen : %d \n", r.conclusion.N);
    prinft("Indice associé : %d \n", r.conclusion.i);
    prinft("\n");
}

void lire_regles(regle *r) {
    while (r->suivant != NULL) {
        lire_regle(*r);
        r = r->suivant;
    }

    lire_regle(*r);
}

void init_faux(bool *tableau, int taille_tableau) {
    for (int i = 0; i < taille_tableau; i++) {
        tableau[i] = false;
    }
}

bool verifie_premisse(regle r, bool *noeuds, bool *clefs, int *sigma) {
    bool ans = true;

    int taille_premisse = r.premisse;
    prop *premisse = r->premisse;
    for (int i = 0; i < taille_premisse; i++) {
        if (premisse[i].N) {
            if (!clefs[sigma[premisse[i].i]]) {
                ans = false;
            }
        } else {
            if (!noeuds[premisse[i].i]) {
                ans = false;
            }
        }
    }

    return ans;
}

regle *parcours_regles(regle *r, bool *noeuds, bool *clefs, bool *progres, int *sigma) {
    if (r == NULL) return NULL;

    if (verifie_premisse(r, noeuds, clefs)) {
        *progres = true;

        if (r->conclusion.N) {
            clefs[sigma[r->conclusion.i]] = true;
        } else {
            noeuds[r->conclusion.i] = true;
        }

        regle *suite = r->suivant;
        libere_regles(r);
        return parcours_regles(suite, noeuds, clefs, progres);
    }

    r->suivant = parcours_regles(r->suivant, noeuds, clefs, progres);
    return r;
}

bool est_sat(regle *r, int nb_noeuds, int nb_clefs, int D, int F, int *sigma) {

    bool *noeuds = malloc(sizeof(bool) * nb_noeuds);
    init_faux(noeuds, nb_noeuds);
    bool *clefs = malloc(sizeof(bool) * nb_clefs);
    init_faux(clefs, nb_clefs);
    bool *progres = malloc(sizeof(bool));
    *progres = false;

    noeuds[D] = true;

    parcours_regles(r, noeuds, clefs, progres);

    bool ans = noeuds[F];

    regle *current = r;

    while (current->suivant != NULL) {
        regle *temp = current;
        current = current->suivant;
        libere_regles(temp);
    }

    libere_regles(current);

    return ans;
}

//////////////////////////////////////////////////////////////////////
// Tests

regle *genere_jeu_1() {

    regle *r = NULL;
    /*
    // Définition des règles représentant le jeu 1 donné en exemple.

    r = ajoute_regles_clefs(r, 2, (int []){1}, 1);
    r = ajoute_regles_clefs(r, 3, (int []){0}, 1);
    r = ajoute_regles_clefs(r, 4, (int []){0, 2}, 2);

    r = ajoute_regles_arete(r, 0, 1, (int []){0}, 1);
    r = ajoute_regles_arete(r, 1, 2, (int []){1}, 1);
    r = ajoute_regles_arete(r, 1, 2, (int []){}, 0);
    r = ajoute_regles_arete(r, 0, 3, (int []){}, 0);
    r = ajoute_regles_arete(r, 1, 4, (int []){1}, 1);
    r = ajoute_regles_arete(r, 2, 5, (int []){1, 2}, 2);
    r = ajoute_regles_arete(r, 3, 4, (int []){1}, 1);
    r = ajoute_regles_arete(r, 4, 5, (int []){0, 1}, 2);


    // Noeud initial d'indice 0, et noeud final d'indice 5
    // Ce jeu comprend 6 noeuds et 3 clefs différentes

    */
    return r;
}

regle *genere_jeu_2() {

    regle *r = NULL;
    /*
    // Définition des règles représentant le jeu 2 donné en annexe.

    r = ajoute_regles_clefs(r, 1, (int []){2}, 1);
    r = ajoute_regles_clefs(r, 2, (int []){5}, 1);
    r = ajoute_regles_clefs(r, 3, (int []){0, 1}, 2);
    r = ajoute_regles_clefs(r, 5, (int []){8}, 1);
    r = ajoute_regles_clefs(r, 6, (int []){14}, 1);
    r = ajoute_regles_clefs(r, 7, (int []){12}, 1);
    r = ajoute_regles_clefs(r, 8, (int []){6}, 1);
    r = ajoute_regles_clefs(r, 9, (int []){3, 4}, 2);
    r = ajoute_regles_clefs(r, 10, (int []){13}, 1);
    r = ajoute_regles_clefs(r, 11, (int []){10}, 1);
    r = ajoute_regles_clefs(r, 12, (int []){11, 9}, 2);
    r = ajoute_regles_clefs(r, 14, (int []){15}, 1);
    r = ajoute_regles_clefs(r, 15, (int []){7}, 1);
    r = ajoute_regles_clefs(r, 16, (int []){5}, 1);
    r = ajoute_regles_clefs(r, 17, (int []){16}, 1);

    r = ajoute_regles_arete(r, 0, 3, (int []){11, 12, 13}, 3);
    r = ajoute_regles_arete(r, 1, 4, (int []){6, 7}, 2);
    r = ajoute_regles_arete(r, 2, 3, (int []){1}, 1);
    r = ajoute_regles_arete(r, 3, 4, (int []){3}, 1);
    r = ajoute_regles_arete(r, 3, 4, (int []){0}, 1);
    r = ajoute_regles_arete(r, 4, 5, (int []){0, 2}, 2);
    r = ajoute_regles_arete(r, 2, 6, (int []){5, 6}, 2);
    r = ajoute_regles_arete(r, 2, 7, (int []){}, 0);
    r = ajoute_regles_arete(r, 3, 7, (int []){4}, 1);
    r = ajoute_regles_arete(r, 3, 8, (int []){}, 0);
    r = ajoute_regles_arete(r, 4, 11, (int []){}, 0);
    r = ajoute_regles_arete(r, 5, 12, (int []){3}, 1);
    r = ajoute_regles_arete(r, 6, 7, (int []){4, 5}, 2);
    r = ajoute_regles_arete(r, 7, 8, (int []){4}, 1);
    r = ajoute_regles_arete(r, 8, 9, (int []){6}, 1);
    r = ajoute_regles_arete(r, 9, 10, (int []){7}, 1);
    r = ajoute_regles_arete(r, 10, 11, (int []){4, 7}, 2);
    r = ajoute_regles_arete(r, 7, 12, (int []){0}, 1);
    r = ajoute_regles_arete(r, 8, 12, (int []){0, 6}, 2);
    r = ajoute_regles_arete(r, 10, 13, (int []){}, 0);
    r = ajoute_regles_arete(r, 12, 13, (int []){0, 9}, 2);
    r = ajoute_regles_arete(r, 13, 14, (int []){7, 8}, 2);
    r = ajoute_regles_arete(r, 13, 15, (int []){2}, 1);
    r = ajoute_regles_arete(r, 13, 16, (int []){10}, 1);
    r = ajoute_regles_arete(r, 16, 17, (int []){15, 16}, 2);

    // Noeud initial d'indice 3, et noeud final d'indice 0
    // Ce jeu comprend 18 noeuds et 17 clefs différentes

    */

    return r;
}


int main() {
    srand(1);// seed à changer pour modifier l'aléatoire généré.

    //int p0[] = {8, 10, 14, 12, 5, 13, 4, 16, 11, 15, 9, 3, 7, 0, 1, 2, 6};
    //int p1[] = {0, 16, 5, 14, 1, 15, 3, 12, 8, 7, 11, 9, 13, 6, 4, 10, 2};
    //int p2[] = {0, 5, 10, 4, 15, 2, 9, 11, 16, 12, 6, 8, 13, 3, 1, 14, 7};
    //int p3[] = {3, 8, 7, 16, 2, 0, 4, 1, 5, 12, 9, 6, 10, 13, 11, 14, 15};
    //int p4[] = {2, 10, 14, 13, 0, 6, 15, 11, 16, 12, 9, 4, 1, 5, 7, 8, 3};

    return 0;
}
