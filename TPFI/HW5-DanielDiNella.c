//1. TYPE SAFETY

/*
    L’esistenza del tipo void* `e il più evidente segno del fatto che in C sia un
    linguaggio non type-safe, nonostante la buona volontà dei compilatori moderni
    che hanno proibito diverse possibilità presenti nel C standard delle origini.
    Scrivere un programma C che dà risultati diversi se compilato su un’architettura
    big-endian (il primo byte è il più significativo, tipico di Motorola, Sun,
    IBM. . . ) oppure su una little-endian (il primo byte è il meno significativo,
    tipico di Intel e Digital. . . ).
    Per chi disponesse di un Mac con processori di famiglia M, potrebbe farmi
    sapere se sono little o big-endian. Io non lo so.
*/

/*
    I Mac con processori di famiglia M funzionano di standard con l'architettura
    little-endian.
*/

//2. ARRAY MUTABILI

/*
    Scrivere una funzione C di complessità Θ(n log n) che elimina i duplicati da un
    vettore (analogo dell’esercizio 1 dell’Homework 1), compattando gli elementi
    a sinistra, e restituendo come risultato il numero degli elementi rimasti.
    Potete ovviamente usare strutture dati per mimare la vostra soluzione Haskell,
    ma quale potrebbe essere il modo migliore per rendere reversibile un
    ordinamento di un array in C?
    Riflettere bene sul prototipo corretto della funzione, in accordo con il galateo
    del C e riflettere anche su quali potrebbero essere (e come definirle)
    eventuali strutture dati ausiliarie.
    L’algoritmo elementare per risolvere questo problema è Θ(n^2), dove n è la
    lunghezza della lista.
*/

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#define MAX_N 100

void swap(int* a, int* b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}

int partition(int arr[], int indexes[], int low, int high) {
    int pivot = arr[indexes[high]];
    int i = low - 1;

    for (int j = low; j < high; j++) {
        if (arr[indexes[j]] < pivot) {
            i++;
            swap(&indexes[i], &indexes[j]);
        }
    }
    swap(&indexes[i + 1], &indexes[high]);
    return (i + 1);
}

void quicksort(int arr[], int indexes[], int low, int high) {
    if (low < high) {
        int pi = partition(arr, indexes, low, high);
        quicksort(arr, indexes, low, pi - 1);
        quicksort(arr, indexes, pi + 1, high);
    }
}

int eliminaDuplicati(int *arr, int n) {
    if (n == 0) return 0;

    int *indexes = (int *)malloc(n * sizeof(int));
    for (int i = 0; i < n; i++) {
        indexes[i] = i;
    }

    quicksort(arr, indexes, 0, n - 1);

    int *unique = (int *)malloc(n * sizeof(int));
    int uniqueCount = 0;

    unique[indexes[0]] = 1;
    uniqueCount++;
    for (int i = 1; i < n; i++) {
        if (arr[indexes[i]] != arr[indexes[i - 1]]) {
            unique[indexes[i]] = 1;
            uniqueCount++;
        } else {
            unique[indexes[i]] = 0;
        }
    }

    int writeIndex = 0;
    for (int i = 0; i < n; i++) {
        if (unique[i]) {
            arr[writeIndex] = arr[i];
            writeIndex++;
        }
    }

    int start = 0;
    for (int i = 1; i < n; i++) {
        if (arr[indexes[i]] != arr[indexes[i - 1]]) {
            quicksort(arr, indexes, start, i - 1);
            start = i;
        }
    }
    quicksort(arr, indexes, start, n - 1);

    free(indexes);
    free(unique);

    return uniqueCount;
}

//3. QUELLO CHE IN HASKELL NON SI PUÒ FARE I: DATI "NON-FUNZIONALI"

/*
    Considerate la seguente funzione ricorsiva che calcola i coefficienti binomiali.

    int cbin(int n, int k){
        if (n==k || n==0) return 1;
        return cbin(n-1,k-1)+cbin(n-1,k);
    }

    Dare prima la definizione di un tipo cBinTree adatto a memorizzare in un
    albero binario i valori dei parametri n e k e il valore calcolato dalla in tale
    chiamata (ogni nodo di un cbinTree contiene quindi tre valori.).
    Scrivere poi una funzione C che genera l’albero delle chiamate ricorsive
    della funzione int cbin(int n, int k), memorizzando in ogni nodo i valori
    dei parametri n e k e il valore calcolato dalla in tale chiamata (vedi Fig. 1).
    Siccome ci sono numerosi sotto-alberi ripetuti nell’albero delle chiamate, è
    interessante costruire il grafo aciclico delle chiamate ricorsive, ma questa volta
    allocando un unico nodo per eventuali chiamate ripetute (con gli stessi valori
    per i parametri n e k), evitando quindi la replicazione di sotto-alberi (vedi
    Fig. 2). Osservate che in questo caso ci sono cammini diversi che finiscono
    nello stesso nodo e osservate anche che non occorre cambiare la definizione del
    tipo cBinTree, n ́e funzioni di stampa o di visita.
    Esempio: In Fig. 2, l’“albero” risultante sullo stesso esempio. Gli archi
    tratteggiati sono quelli che “riportano” su nodi gi`a precedentemente allocati.
*/

typedef struct cBinTree {
    int n;
    int k;
    int value;
    struct cBinTree* left;
    struct cBinTree* right;
    int freed;
    int visited;
} cBinTree;

cBinTree* createNode(int n, int k, int value) {
    cBinTree* newNode = (cBinTree*)malloc(sizeof(cBinTree));
    newNode->n = n;
    newNode->k = k;
    newNode->value = value;
    newNode->left = NULL;
    newNode->right = NULL;
    newNode->freed = 0; 
    newNode->visited = -1;
    return newNode;
}

cBinTree* cBinInvocation(int n, int k) {
    if (n == k || k == 0) {
        return createNode(n, k, 1);
    }

    cBinTree* leftChild = cBinInvocation(n - 1, k - 1);
    cBinTree* rightChild = cBinInvocation(n - 1, k);
    int value = leftChild->value + rightChild->value;

    cBinTree* newNode = createNode(n, k, value);
    newNode->left = leftChild;
    newNode->right = rightChild;

    return newNode;
}

void printBranch(int space, int branches, int visited) {
    for (int i = 0; i < space; i++) {
        printf(" ");
    }

    if (visited >= 1) {
        printf(" *");
    }
    if (branches == 1) {
        printf(" \\");
    } else if (branches == 2) {
        printf(" /");
    }
}

void printTree(cBinTree* root, int space, int branches) {
    if (root == NULL) {
        return;
    }

    space += 5;

    if (root->visited >= 1){
        printf("\n");
        printBranch(space, branches, root->visited);
        printf("cbin(%d, %d) = %d\n", root->n, root->k, root->value);
    } else {
        printTree(root->left, space, 2);
        root->visited += 1;
        printf("\n");
        printBranch(space, branches, root->visited);
        printf("cbin(%d, %d) = %d\n", root->n, root->k, root->value);
    
        printTree(root->right, space, 1);
        root->visited += 1;
    }
}


void freeTree(cBinTree* root) {
    if (root == NULL) {
        return;
    }
    freeTree(root->left);
    freeTree(root->right);
    free(root);
}

cBinTree* cBinInvocationSharing(int n, int k, cBinTree* memo[MAX_N][MAX_N]) {
    if (n == k || k == 0) {
        return createNode(n, k, 1);
    }

    if (memo[n][k] != NULL) {
        return memo[n][k];
    }

    cBinTree* leftChild = cBinInvocationSharing(n - 1, k - 1, memo);
    cBinTree* rightChild = cBinInvocationSharing(n - 1, k, memo);
    int value = leftChild->value + rightChild->value;

    cBinTree* newNode = createNode(n, k, value);
    newNode->left = leftChild;
    newNode->right = rightChild;

    memo[n][k] = newNode;

    return newNode;
}

void sharingFreeTree(cBinTree** rootPtr) {
    if (rootPtr == NULL || *rootPtr == NULL) {
        return;
    }

    cBinTree* root = *rootPtr;

    if (root->freed) {
        return;
    }

    root->freed = 1;

    if (root->left != NULL) {
        sharingFreeTree(&(root->left));
    }
    if (root->right != NULL) {
        sharingFreeTree(&(root->right));
    }

    free(root);
    *rootPtr = NULL;
}

//4. QUELLO CHE IN HASKELL NON SI PUÒ FARE II: CRIVELLO DI EULERO
/*
Scrivere una funzione C che implementi il crivello di Eulero. Non `e ovvio
“saltare” in modo efficiente i numeri gi`a cancellati per trarne vantaggio nelle

successive cancellazioni. La soluzione che vi propongo di implementare con-
siste nell’usare un vettore di coppie di naturali, succ e prec, come una lista

doppiamente concatenata in cui nella posizione i, se i non `e stato cancellato,

succ `e il numero di posizioni che occorre saltare per andare al prossimo nu-
mero non cancellato, mentre prec `e il numero di posizioni che occorre saltare

(all’indietro) per andare al precedente numero non cancellato.
Potete ad esempio definire un tipo Pair che `e una coppia di interi succ e
prec e definiamo un vettore di Pair. Questo vettore va inizializzato con tutti 1
(che significa appunto che tutti i numeri sono ancora potenziali primi).
*/

typedef struct Pair {
    int succ;
    int prec;
} Pair;

Pair* eulerSieve(int n) {
    Pair* pairs = (Pair*)malloc(n * sizeof(Pair));

    for (int i = 0; i < n; i++) {
        pairs[i].succ = 1;
        pairs[i].prec = 1;
    }

    return pairs;
}

void sieveOfEratosthenes(int n) {
    Pair* pairs = eulerSieve(n);
    int primesCount = 0;

    for (int i = 2; i < n; i++) {
        if (pairs[i].succ == 1) {
            primesCount++;
            printf("%d ", i);

            for (int j = i * 2; j < n; j += i) {
                pairs[j].succ = 0;
            }
        }
    }

    printf("\nNumero di primi trovati: %d\n", primesCount);

    free(pairs);
}

//X. TEST

int main() {
    int arr[] = {4, 2, 2, 3, 3, 3, 1, 4, 4, 1, 5};
    int n1 = sizeof(arr) / sizeof(arr[0]);
    int newLength = eliminaDuplicati(arr, n1);
    printf("Array senza duplicati: ");
    for (int i = 0; i < newLength; i++) {
        printf("%d ", arr[i]);
    }
    printf("\nNumero di elementi rimasti: %d\n", newLength);
    printf("---------------------------------\n");

    int n2 = 5, k = 3;
    cBinTree* root1 = cBinInvocation(n2, k);
    printTree(root1,0,0);
    freeTree(root1);
    printf("---------------------------------\n");

    cBinTree* memo[MAX_N][MAX_N] = {NULL};
    cBinTree* root2 = cBinInvocationSharing(n2, k, memo);
    printTree(root2,0,0);
    sharingFreeTree(&root2);
    printf("---------------------------------\n");

    int n3;
    printf("Inserisci un limite superiore per il crivello di Eratostene: ");
    scanf("%d", &n3);
    sieveOfEratosthenes(n3);

    return 0;
}
