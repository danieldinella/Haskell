-- 1. RIMOZIONE DUPLICATI

-- 1.1. Dare la definizione di myTakeWhile e myTakeDrop
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ [] = []
myTakeWhile p (x:xs)
    | p x = x : myTakeWhile p xs
    | otherwise = []

myTakeDrop :: (a -> Bool) -> [a] -> [a]
myTakeDrop _ [] = []
myTakeDrop p (x:xs)
    | p x = myTakeDrop p xs
    | otherwise = xs
{-
1.2. Scrivere una funzione ricorsiva myRemoveDupsOrd che
rimuove i duplicati da una lista ordinata xs di lunghezza
n in tempo O(n).
-}
myRemoveDupsOrd :: Eq a => [a] -> [a]
myRemoveDupsOrd [] = []
myRemoveDupsOrd [x] = [x]
myRemoveDupsOrd (x:y:xs)
    | x == y = myRemoveDupsOrd (y:xs)
    | otherwise = x : myRemoveDupsOrd (y:xs)

{- 
1.3. Scrivere una funzione myRemoveDups che rimuove i duplicati
da una qualsiasi lista xs di lunghezza n in tempo O(n log n),
preservando l’ordine originale delle prime occorrenze degli
elementi rimasti.
-}

myRemoveDups :: (Eq a) => [a] -> [a]
myRemoveDups [] = []
myRemoveDups (x:xs) = x : (filter (/=x) $ myRemoveDups xs)

--2. Interdefinibilità di Funzionali

{-
2.1. Definire il funzionale zipWith f xs senza decomporre liste,
ma usando un'espressione che contenga zapp, f ed eventualmente
xs e ys.
-}

zapp :: [a -> b] -> [a] -> [b]
zapp (f:fs) (x:xs) = f x : zapp fs xs
zapp _ _ = []

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f xs ys = zapp (map f xs) ys

{-
2.2.  Abbiamo visto che zipWith è più generale di zip. Tuttavia
si può definire zipWith f xs ys usando zip e un paio di altri
funzionali visti nella Lezione 3.
-}

myZipWith2 :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith2 f xs ys = map (\(x, y) -> f x y) (zip xs ys)

{-
2.3. Definire il funzionale map f xs senza decomporre xs, ma
usando un’espressione che contenga foldr, f e xs. Fare lo stesso
usando foldl.
-}

myMapR :: (a -> b) -> [a] -> [b]
myMapR f xs = foldr (\x acc -> f x : acc) [] xs

myMapL :: (a -> b) -> [a] -> [b]
myMapL f xs = foldl (\acc x -> acc ++ [f x]) [] xs

{-
2.4. Argomentare brvemente sul perché non sia possibile definire
foldl e foldr usando map.

Foldl e foldr riducono una lista a un singolo valore combinando
gli elementi in modo specifico, mentre map trasforma una lista
elemento per elemento. Inoltre, map non conserva lo stato
accumulato o il flusso della riduzione, che è invece caratteristico
di foldl e foldr.
-}

--3. Segmenti e sottoliste

{-
3.1. Scrivere una funzione prefissi :: [a] -> [[a]] che ritorna
tutti i segmenti iniziali di una lista.
-}

prefissi :: [a] -> [[a]]
prefissi [] = [[]]
prefissi xs = [take n xs | n <- [0..length xs]]

{-
3.2. Senza preoccuparsi dell'efficienza, ma usando i funzionali
prefissi, suffissi e altri funzionali dello standard Prelude,
scrivere una funzione segSommaS :: (Num a) => [a] -> a -> [[a]]
che data una lista numerica xs e un valore s restituisce tutti i
segmenti (cioè sottoliste di elementi consecutivi) di xs di somma s.
-}

suffissi :: [a] -> [[a]]
suffissi [] = [[]]
suffissi (x:xs) = (x:xs) : suffissi xs

segSommaS :: (Num a, Eq a) => [a] -> a -> [[a]]
segSommaS xs s = filter (\seg -> sum seg == s) (concatMap prefissi (suffissi xs))

{-
3.3. Scrivere una funzione sublSommaS :: (Num a) => [a] -> a -> [[a]]
che data una lista numerica e un valore s restituisce tutte le sottoliste
(anche di elementi non consecutivi) di somma s.
-}

sublSommaS :: (Num a, Eq a) => [a] -> a -> [[a]]
sublSommaS xs s = filter (\subl -> sum subl == s) (sottoliste xs)
    where
        sottoliste [] = [[]]
        sottoliste (y:ys) = [y:resto | resto <- sottoliste ys] ++ sottoliste ys

--4. Partizioni

{-
4.1. Scrivere una funzione Haskell part :: Int -> Integer che calcola
il numero di partizioni di un certo numero n.
-}

part :: Int -> Integer
part n = pxs !! n
    where
        pxs = map p [0..n]
        p 0 = 1
        p n' = sum [(-1)^(k+1) * p (n' - k * (3*k - 1) `div` 2) + p (n' - k * (3*k + 1) `div` 2) | k <- [1..n']]

{-
4.2. Se invece considero diverse tra loro anche le partizioni che
differiscono solo per l'ordine, quante sono?
-}

part2 :: Int -> Integer
part2 n = (pxs !! n) `div` 2
    where
        pxs = map p [0..n]
        p 0 = 1
        p n' = sum [(-1)^(k+1) * p (n' - k * (3*k - 1) `div` 2) + p (n' - k * (3*k - 1) `div` 2) | k <- [1..n']]

{-
4.3. Scrivere poi una funzione Haskell parts :: Int -> [[Int]], che
calcola la lista delle partizioni di n.
-}

parts :: Int -> [[Int]]
parts n = partitionList n (reverse [1..n])
    where
        partitionList 0 _ = [[]]
        partitionList m [] = []
        partitionList m (x:xs)
            | m < x = partitionList m xs
            | otherwise = map (x:) (partitionList (m - x) (x:xs)) ++ partitionList m xs

{-
4.4. Ma scrivere part usando parts?
-}

part3 :: Int -> Integer
part3 n = fromIntegral (length (parts n))

{-
E la complessità è molto maggiore della part originaria?

La versione originale di part memorizza i risultati intermedi
evitando di ricalcolarli e la sua complessità computazionale è
è O(n²) mentre dovendo calcolare ogni partizione di n prima di
contare quante sono, la complessità diventa O(2^n) quindi risulta
molto più efficiente la prima versione.
-}