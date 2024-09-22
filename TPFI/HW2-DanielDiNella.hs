--1. MERGE SORT (ITERATIVO)

{-
    1.1. Definire una funzione Haskell che segue la seguente idea bottom-up per
    implementare l’algoritmo mergeSort:

    Data una lista xs, creare una lista di liste lunghe 1, ciascuna contenente
    un elemento di xs, poi fondere a due a due le liste ordinate (eventualmente
    lasciando inalterata l’ultima lista quando il numero delle liste è dispari),
    finché non rimane un’unica lista ordinata.
-}

myMerge :: Ord a => [a] -> [a] -> [a]
myMerge [] ys   = ys
myMerge xs []   = xs
myMerge xs@(x:txs) ys@(y:tys)
    | x < y     = x:myMerge txs ys
    | otherwise = y:myMerge tys xs

myMergeSort :: Ord a => [a] -> [a]
myMergeSort [] = []
myMergeSort [x] = [x]
myMergeSort xs = myMerge (myMergeSort ls) (myMergeSort rs)
    where
        (ls, rs) = splitAt (length xs `div` 2) xs

{-
    1.2. Accelerare la prima fase di questo algoritmo per trarre vantaggio da
    input “favorevoli”. La miglioria dovrebbe assicurare un comportamento lineare
    in casi particolarmente fortunati.
-}

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

myMergeSort2 :: Ord a => [a] -> [a]
myMergeSort2 xs
    | isSorted xs = xs
    | otherwise = myMergeSort xs

--2. ALBERI & FUNZIONALI SUGLI ALBERI

{-
    2.1.  Scrivere i funzionali mapBT, mapBT’, foldrBT, foldrBT’, foldlBT, e
    foldlBT’ che generalizzano agli alberi BinTree e BinTree’ gli analoghi 
    funzionali map, foldr e foldl sulle liste. Riflettete accuratamente sui tipi
    che devono avere e su quali siano, di fatto, i principi di ricorsione sugli
    alberi binari.
-}

data BinTree a = Node a (BinTree a) (BinTree a) | Empty
data BinTree' a = Node' (BinTree' a) (BinTree' a) | Leaf a
instance Show a => Show (BinTree a) where
    show Empty = "Empty"
    show (Node x left right) = "Node " ++ show x ++ " (" ++ show left ++ ") (" ++ show right ++ ")"
instance Show a => Show (BinTree' a) where
    show (Leaf x) = "Leaf " ++ show x
    show (Node' left right) = "Node' (" ++ show left ++ ") (" ++ show right ++ ")"

mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT _ Empty = Empty
mapBT f (Node x left right) = Node (f x) (mapBT f left) (mapBT f right)

mapBT' :: (a -> b) -> BinTree' a -> BinTree' b
mapBT' f (Leaf x) = Leaf (f x)
mapBT' f (Node' left right) = Node' (mapBT' f left) (mapBT' f right)

foldrBT :: (a -> b -> b) -> b -> BinTree a -> b
foldrBT _ acc Empty = acc
foldrBT f acc (Node x left right) = foldrBT f (f x (foldrBT f acc right)) left

foldrBT' :: (a -> b -> b) -> b -> BinTree' a -> b
foldrBT' f acc (Leaf x) = f x acc
foldrBT' f acc (Node' left right) = foldrBT' f (foldrBT' f acc right) left

foldlBT :: (b -> a -> b) -> b -> BinTree a -> b
foldlBT _ acc Empty = acc
foldlBT f acc (Node x left right) = foldlBT f (f (foldlBT f acc left) x) right

foldlBT' :: (b -> a -> b) -> b -> BinTree' a -> b
foldlBT' f acc (Leaf x) = f acc x
foldlBT' f acc (Node' left right) = foldlBT' f (foldlBT' f acc left) right

{-
    2.2. Scrivere poi le seguenti funzioni usando foldrBT e foldrBT’ (cercare di ottenere 
    algoritmi lineari nel numero dei nodi):

    (a) numero dei nodi di un albero binario;
-}

numNodes :: BinTree a -> Int
numNodes Empty = 0
numNodes (Node x left right) = numNodes left + numNodes right + 1

numNodes' :: BinTree' a -> Int
numNodes' (Leaf _) = 1
numNodes' (Node' left right) = numNodes' left + numNodes' right + 1

{-
    (b) altezza dell’albero (= lunghezza in numero di archi del più lungo cammino
    radice-foglia);
-}

height :: BinTree a -> Int
height Empty = -1
height (Node _ left right) = 1 + max (height left) (height right)

height' :: BinTree' a -> Int
height' (Leaf _) = 0
height' (Node' left right) = 1 + max (height' left) (height' right)

{-
    (c) massimo indice di sbilanciamento (= massima differenza tra altezza
    sotto-albero destro/sinistro).
-}

maxImbalance :: BinTree a -> Int
maxImbalance Empty = 0
maxImbalance (Node _ left right) = max (abs (height left - height right)) (max (maxImbalance left) (maxImbalance right))

maxImbalance' :: BinTree' a -> Int
maxImbalance' (Leaf _) = 0
maxImbalance' (Node' left right) = max (abs (height' left - height' right)) (max (maxImbalance' left) (maxImbalance' right))

{-
    2.3. Gli alberi a branching illimitato si possono facilmente definire
    in Haskell come segue: data Tree a = R a [Tree a]. Come ai punti
    precedenti, scrivendo i funzionali mapT, foldrT e foldlT.
-}

data Tree a = R a [Tree a]
instance Show a => Show (Tree a) where
    show (R x children) = show x ++ " " ++ show children


mapT :: (a -> b) -> Tree a -> Tree b
mapT f (R x children) = R (f x) (map (mapT f) children)


foldrT :: (a -> b -> b) -> b -> Tree a -> b
foldrT f acc (R x children) = f x (foldr (\child acc' -> foldrT f acc' child) acc children)


foldlT :: (b -> a -> b) -> b -> Tree a -> b
foldlT f acc (R x children) = foldl (\acc' child -> foldlT f acc' child) (f acc x) children

--3. NODI EQUILIBRATI

{-
    Un nodo u di un albero (considerare a piacere i BinTree oppure i Tree dell’esercizio precedente)
    con valori numerici in tutti i nodi è detto equilibrato se la somma delle chiavi nel cammino
    dalla radice a u (esclusa la chiave in u) è esattamente uguale alla somma delle chiavi del
    sotto-albero radicato in u (compresa la chiave in u).
    Scrivere una funzione nodiEquilibrati :: Num a ⇒BinTree a → [a] che preso in input un albero,
    restituisce la lista (eventualmente vuota) contenente tutti i valori nei nodi equilibrati.
    Valutare la complessità della funzione.
-}

nodiEquilibrati :: (Num a, Eq a) => BinTree a -> [a]
nodiEquilibrati tree = nodiEquilibratiAux tree 0

nodiEquilibratiAux :: (Num a, Eq a) => BinTree a -> a -> [a]
nodiEquilibratiAux Empty _ = []
nodiEquilibratiAux (Node val left right) parentSum =
    if parentSum == sumSubtree (Node val left right)
        then val : (nodiEquilibratiAux left (parentSum + val) ++ nodiEquilibratiAux right (parentSum + val))
        else nodiEquilibratiAux left (parentSum + val) ++ nodiEquilibratiAux right (parentSum + val)

sumSubtree :: (Num a, Eq a) => BinTree a -> a
sumSubtree Empty = 0
sumSubtree (Node val left right) = val + sumSubtree left + sumSubtree right

{-
    Poiché l'algoritmo visita ogni nodo dell'albero una sola volta, il tempo necessario per 
    eseguire l'algoritmo è proporzionale al numero totale di nodi nell'albero, quindi è O(n), 
    dove n è il numero totale di nodi. Inoltre, poiché l'algoritmo utilizza la ricorsione, la 
    quantità di memoria richiesta è proporzionale alla massima profondità dell'albero, quindi 
    la complessità spaziale è anch'essa O(n), dove n rappresenta la massima profondità dell'albero.
-}

--4. ALBERI BINARI DI RICERCA

{-
    Scrivere una funzione Haskell listToABR :: Ord a ⇒ [a] → BinTree a che sistema i valori di 
    una lista in un albero binario di ricerca. Determinare la complessità della funzione e 
    chiedersi se si tratta di una complessità ottima rispetto al problema.
-}

listToABR :: Ord a => [a] -> BinTree a
listToABR [] = Empty
listToABR xs = foldr insertNode Empty xs

insertNode :: Ord a => a -> BinTree a -> BinTree a
insertNode x Empty = Node x Empty Empty
insertNode x (Node val left right)
    | x <= val  = Node val (insertNode x left) right
    | otherwise = Node val left (insertNode x right)

{-
    La complessità temporale di questa implementazione è O(n log n), dove n è la lunghezza della 
    lista di input. Questo perché ogni elemento della lista viene inserito nell'albero tramite 
    la funzione insertNode, che ha una complessità di tempo O(log n) in media, considerando che 
    l'albero è bilanciato. Tuttavia, se l'albero non è bilanciato (ad esempio, se gli elementi 
    sono già ordinati), la complessità temporale potrebbe essere O(n^2), rendendo questa 
    implementazione non ottimale in quel caso.
-}

--5. DERIVAZIONI E PROGRAMMI

{-
    La funzione scanr :: (a → b) → b → [a] → [b] può essere facilmente definita componendo map, 
    foldr e tails (chiamata suffissi nell’Homework precedente): 
    scanr f e = map (foldr f e) . tails
    Usare la definizione sopra come specifica per derivare una definizione efficiente (cioè lineare 
    nella lunghezza della lista) facendo manipolazioni algebriche, in analogia con quanto visto 
    per scanl (slide lezione 9).
-}

{-
    Caso base:
    scanr f e []
    map (foldr f e) . tails []
    map (foldr f e)(tails [])
    map (foldr f e)[[]]
    [foldr f e []] = [e]
    scanr f e [] = [e]

    scanr f e (x:xs)
    map(foldr f e) (tails(x:xs))
    map(foldr f e) ((x:xs) : tails(xs))
    foldr f e (x:xs) : foldr f e (tails(xs))
    foldr f e (x:xs) : ... : foldr f e []
    foldr f e (x:xs) : ... : e
    foldr f e (x:xs) : scanr f e xs
    f x (foldr f e xs) : scanr f e xs
-}

scanr2 :: (a -> b -> b) -> b -> [a] -> [b]
scanr2 _ e [] = [e]  
scanr2 f e (x:xs) = f x prev : rest
    where
        rest = scanr2 f e xs
        prev = head rest 