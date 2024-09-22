--1. INSOMNIA

{-
    Scrivere una funzione Haskell che genera la lista infinita di caratteri
    insomnia= "1 sheep 2 sheep 3 sheep 4 sheep ...". Provare a scrivere un 
    “oneliner”, cioè un programma che semplicemente compone opportunamente
    funzioni. Può essere utile la funzione show :: Show a => a => String che
    trasforma un elemento di qualsiasi tipo che implementa la classe Show in
    una stringa, cioè una lista di caratteri.
-}

insomnia :: [String]
insomnia = map (\x -> show x ++ " sheep ") [1..]

--2. TRIANGOLO DI TARTAGLIA

{-
    Definite in Haskell la lista infinita di liste finite tartaglia, tale che
    tartaglia!!n sia l’n-esima riga del triangolo di Tartaglia, e quindi 
    tartaglia!!n!!k sia il coefficiente binomiale (n,k).
-}

tartaglia :: [[Integer]]
tartaglia = iterate nextRow [1]
    where
        nextRow row = zipWith (+) ([0] ++ row) (row ++ [0])

--3. NUMERI FORTUNATI

{-
    I numeri fortunati, introdotti da Stanislaw Ulam, sono definiti come segue:
    1. Dalla sequenza dei numeri naturali (escluso lo zero) tolgo tutti i secondi
    numeri, cio`e i pari.
    2. il secondo numero rimasto `e il 3 e quindi si tolgono tutti i terzi numeri
    tra i sopravvissuti (5, 11, 17, . . . ).
    3. ora si considera il terzo numero rimasto cio`e il 7 e rimuovo tutti i settimi
    numeri (il primo `e il 19) e cos`ı via, fino a ottenere tutti i numeri sopravvissuti
    a tutte le operazioni di “filtraggio”.
    Scrivere una funzione Haskell che genera lo stream dei numeri fortunati.
-}

mcd :: Int -> Int -> Int
mcd a 0 = a
mcd a b = mcd b (a `mod` b)

numeriFortunati :: [Int]
numeriFortunati = filterFortunati [1..] 1
    where
        filterFortunati (x:xs) acc
            | x == 1 = x:filterFortunati xs acc
            | x == 2 = x:filterFortunati xs (acc * x)
            | mcd acc x == 1 = x:filterFortunati xs (acc * x)
            | otherwise = filterFortunati xs acc
        

-- 1D. ITERAZIONE, RICORSIONE PRIMITIVA, CHURCH & ACKERMAN

{-
    Una funzione f :: Nat → a `e definita per ricorsione primitiva dalle funzioni
    h :: Nat → a → a e g :: a se rispetta le equazioni:
    f (n + 1) = h n (f n) e f 0 = g

    1. Scrivere in Haskell il funzionale primRec che definisce la ricorsione primitiva;
-}

data Nat = Zero | Succ Nat deriving (Eq, Show)
fromIntToNat :: Int -> Nat
fromIntToNat n
    | n < 0     = Zero
    | otherwise = fromPositiveInt n
    where
        fromPositiveInt 0 = Zero
        fromPositiveInt m = Succ (fromPositiveInt (m - 1))
fromNatToFloat :: Nat -> Float
fromNatToFloat Zero = 0.0
fromNatToFloat (Succ n) = 1.0 + fromNatToFloat n
addNat :: Nat -> Nat -> Nat
addNat Zero n = n
addNat (Succ m) n = Succ (addNat m n)
mulNat :: Nat -> Nat -> Nat
mulNat Zero _ = Zero
mulNat (Succ m) n = addNat n (mulNat m n)
instance Num Nat where
    fromInteger = fromIntToNat . fromIntegral
    (+) = addNat
    (*) = mulNat
    abs = id
    signum Zero = Zero
    signum _ = Succ Zero
    negate _    = error "Numeri negativi non ammessi"

primRec :: (Nat -> a -> a) -> Nat -> a -> a
primRec h Zero g = g
primRec h (Succ n) g = h n (primRec h n g)

{-
    2. Scrivere in Haskell il funzionale primRec' che definisce la ricorsione primitiva,
    senza fare ricorsione sui naturali, ma usando l’iterazione e le coppie, cioè usando
    il funzionale for visto a lezione.
-}

primRec' :: (Num t, Eq t) => (t -> t -> t) -> t -> t -> t
primRec' h n g = fst (primAux h n g) 
    where
        primAux :: (Num t, Eq t, Num t) => (t -> t -> t) -> t -> t -> (t,t)
        primAux h 0 g = (0,g)
        primAux h n g = (h f1 f2, f1)
            where
                (f1,f2) = primAux h (n-1) g

-- 2D. LISTE INFINITE

{-
    1. Scrivere un one-liner Haskell partsFromAll tale che partsFromAll n allPartitions
    sia proprio la lista di liste che rappresenta le partizioni di n (in ordine ascendente,
    preferibilmente).
-}

partsFromAll :: Int -> [[Int]] -> [[Int]]
partsFromAll n allPartitions = filter ((== n) . sum) $ takeWhile ((<= n) . sum) allPartitions

{-
    2. Scrivere un’equazione ricorsiva che genera allPartitions.
-}

allPartitions :: [[Int]]
allPartitions = concatMap partAux [1..]
    where
        partAux n = go n n
            where
                go _ 0 = [[]]
                go m k = [x:xs | x <- [1..min m k], xs <- go x (k - x)]

{-
    3. Sviluppare qualche idea per rappresentare altre strutture combinatorie
    in modo analogo, tipo: tutti i sottoinsiemi (finiti) dei Naturali, tutte le
    permutazioni (a dominio finito) dei Naturali o altre di vostro gradimento.
-}

subsetsOfN :: Int -> [[Int]]
subsetsOfN n = subsets [1..n]
    where
        subsets :: [Int] -> [[Int]]
        subsets [] = [[]]
        subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

--4D. ALBERO DEI RAZIONALI DI CALKIN-WILF

{-
    L’albero di Calkin-Wilf (vedi figura) fornisce un metodo per costruire una
    biiezione diretta tra numeri naturali e numeri razionali: i nodi dell’albero
    contengono tutte e sole le coppie di razionali ridotte ai minimi termini. Ci
    basta sapere che l’albero di Calkin-Wilf ha una semplice regola di costruzione:
    se (m, n) `e il valore in un nodo, il figlio sinistro contiene la coppia (m, m + n)
    e il figlio destro la coppia (m + n, n). Viene richiesto di:
    
    1. scrivere un’equazione ricorsiva che genera l’albero di Calkin-Wilf;
-}

data CWTree = Empty | Node (Nat, Nat) CWTree CWTree deriving (Show)

cwRec :: CWTree
cwRec = Node (Succ Zero, Succ Zero) (left (Succ Zero, Succ Zero)) (right (Succ Zero, Succ Zero))
    where
        left (m, n) = Node (m, addNat m n) (left (m, addNat m n)) (right (m, addNat m n))
        right (m, n) = Node (addNat m n, n) (left (addNat m n, n)) (right (addNat m n, n))

{-
    2. scrivere la funzione takeNlevels::Int -> BinTree a -> BinTree a che taglia
    un albero (eventualmente infinito) ai primi n livelli; stipulando che
    takeNlevels 0 b torni sempre l’albero vuoto Empty e takeNlevels 1 (Node
    r lft rgt) torni Node r Empty Empty, etc., l’albero in figura si ottiene con
    la chiamata takeNlevels 4 calkinWilf.
-}

takeNlevels :: Int -> CWTree -> CWTree 
takeNlevels 0 _ = Empty
takeNlevels n (Node v lft rgt) = Node v (takeNlevels (n-1) lft) (takeNlevels (n-1) rgt)

{-
    3. Scrivere una funzione visitaLivelli :: BinTree a -> [a] che produce la lista
    dei valori contenuti nei nodi di un albero (finito) nella sequenza ottenuta da
    una visita per livelli. Quindi, la chiamata visitaLivelli (takeNlevels n calkinWilf)
    produrrà i primi 2n − 1 razionali nell’ordine indotto da questa biiezione.
-}

quoziente :: (Nat, Nat) -> Float
quoziente (m, n) = fromNatToFloat m / fromNatToFloat n

visitaLivelli :: CWTree -> [Float]
visitaLivelli tree = bfs [tree]
    where
        bfs [] = []
        bfs (Empty:rest) = bfs rest
        bfs (Node (m, n) left right:rest) = quoziente (m, n) : bfs (rest ++ [left, right])