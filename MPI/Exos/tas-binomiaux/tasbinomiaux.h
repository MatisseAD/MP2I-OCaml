// 1. Déclarations anticipées des structures
struct abrebin_s;

// 2. Définition des alias (typedef)
typedef struct abrebin_s arbrebin;

// 3. Définition des structures avec les alias

// désigne un arbre seul 
// ou un arbre d'un tas
// on confond un tas et son premier arbre
struct abrebin_s 
{ 
    int ordre;
    int valeur;
    int priorite;
    arbrebin *pere; 
    arbrebin *aine; 
    arbrebin *frere;    
};



// 4. Prototypes

// attention: ici la contrainte d'ordre sera: 
// priorité d'un noeud >= à celles de ses fils

arbrebin* init_arbrebin(int v,int p);
// arbre binomial seul d'ordre 0 avec la valeur v et la priorité p

void impression_arbrebin(arbrebin *arbre);
// impression du contenu d'un arbre seul

void impression_tasbin(arbrebin *tas);
// impression du contenu du tas, arbre par arbre, à partir de son premier arbre

void impression_ordre(arbrebin *tas);
// impression des ordres de chaque arbre du tas

arbrebin* fusion_arbrebin(arbrebin *p, arbrebin *f);
// fusion de deux arbres de même ordre: on attache l'arbre f à l'arbre p
// l'un est seul, l'autre est issue du tas

arbrebin* inserer_arbrebin(arbrebin *t, arbrebin *a);
// insere l'arbre seul a dans le tas t

arbrebin* inserer_valeur_prior(arbrebin *t,int v, int p);
// insere le couple (v,p) dans le tas t


int recherche_priormax(arbrebin *tas,int* p);
// renvoie l'élément de priorité maximale
// p sera la priorité calculée de cet élément

// la suite est facultative:

arbrebin* fusion_tas(arbrebin* tas1, arbrebin* tas2);
// fusion de deux tas en un seul
// algo1 : par insertion des arbres de l'un des tas dans l'autre
// facultatif: algo2 : par "addition" des deux tas

arbrebin* suppression_priormax(arbrebin *tas,int pmax);
// suppression d'un élément de priorité maximale pmax