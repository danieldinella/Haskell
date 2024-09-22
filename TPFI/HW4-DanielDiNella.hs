{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
import Control.Monad.State
import Data.Char (toLower)
import Control.Monad (replicateM)

-- 1. INPUT/OUTPUT
{-
    Definite un’azione charCount :: IO () che legge un numero
    n da tastiera, poi n stringhe e alla fine stampa il numero
    di stringhe in cui appare ciascuna lettera.
-}

countChars :: [String] -> [(Char, Int)]
countChars strings =
    let lowerStrings = map (map toLower) strings
        chars = concat lowerStrings
        charCounts = [(c, length (filter (== c) chars)) | c <- ['a'..'z']]
    in charCounts

printCounts :: [(Char, Int)] -> IO ()
printCounts charCounts = mapM_ printAux charCounts
    where
        printAux (char, count) = putStrLn (char : ": " ++ show count)

charCount :: IO ()
charCount = do
    putStrLn "Inserire il numero di stringhe:"
    n <- readLn :: IO Int
    putStrLn ("Inserire " ++ show n ++ " stringhe:")
    strings <- replicateM n getLine
    let charCounts = countChars strings
    putStrLn "Numero di occorrenze di ciascuna lettera:"
    printCounts charCounts

--2. NODI EQUILIBRATI CON APPLICATIVI E MONADI
{-
    Risolvere l’esercizio 3 dell’Homework 2 (Nodi Equilibrati) usando applicativi
    e monadi (in analogia con le funzioni che creano un albero o che rietichettano
    i nodi dei un albero visti nella Lezione ), in modo da evitare di dover usare
    (esplicitamente nel codice) parametri e risultati di ritorno ausiliari.
-}


data BinTree a = Empty | Node a (BinTree a) (BinTree a) deriving (Show)

nodiEquilibrati :: (Num a, Eq a, Show a) => BinTree a -> [a]
nodiEquilibrati tree = evalState (nodiEquilibratiM tree) 0

nodiEquilibratiM :: (Num a, Eq a, Show a) => BinTree a -> State a [a]
nodiEquilibratiM Empty = return []
nodiEquilibratiM (Node val left right) = do
    parentSum <- get
    let newSum = parentSum + val
    leftEquilibrati <- withState (const newSum) (nodiEquilibratiM left)
    rightEquilibrati <- withState (const newSum) (nodiEquilibratiM right)
    let subtreeSum = sumSubtree (Node val left right)
    let currentNodeResult = if parentSum == (subtreeSum - val) then [val] else []
    return $ currentNodeResult ++ leftEquilibrati ++ rightEquilibrati

sumSubtree :: (Num a) => BinTree a -> a
sumSubtree Empty = 0
sumSubtree (Node val left right) = val + sumSubtree left + sumSubtree right


--3. MONADI/ECCEZIONI
{-
    Definire il tipo NatBin che rappresenta i numeri naturali come sequenze binarie.
    Potete definirlo come liste (di lunghezza fissata) di 0 e 1, oppure potete dare
    una definizione con data (ad esempio usando 3 costruttori, di cui uno sia la
    costante 0 e gli altri due. . . in ogni caso, immaginare di definire una “parola di
    memoria”, quindi prevedete una lunghezza massima costante).
    Definire un valutatore di espressioni aritmetiche su NatBin, analoghi a quel-
    li visti a lezione, ma considerare tutte le operazioni aritmetiche (+, ×, div, mod
    e -). Estendere il tipo Maybe in modo che il risultato di un’espressione possa
    essere eventualmente un’eccezione diversa a seconda dell’eventuale situazio-
    ne anomala che si `e verificata: divisione per zero, numero negativo oppure
    overflow.
    Potete completare l’esercizio facendo in modo che il tipo NatBin sia un’i-
    stanza delle usuali classi Eq, Ord, Num, Show.
-}

data NatBin = NatBin [Int] deriving (Show)

maxNatBinLength :: Int
maxNatBinLength = 32

maxNatValue :: Integer
maxNatValue = 2^maxNatBinLength - 1

zero :: NatBin
zero = mkNatBin 0

mkNatBin :: Integer -> NatBin
mkNatBin x  
    | x > maxNatValue = error ("Overflow: " ++ show x ++ " is too big.")
    | otherwise = NatBin (padZeros (toBinary x))
    where
        toBinary 0 = [0]
        toBinary x = reverse (binaryList x)
        binaryList 0 = []
        binaryList n = let (q, r) = n `divMod` 2 in fromIntegral r : binaryList q
        padZeros xs = replicate (maxNatBinLength - length xs) 0 ++ xs

instance Eq NatBin where
    (NatBin xs) == (NatBin ys) = xs == ys

instance Ord NatBin where
    compare (NatBin xs) (NatBin ys) = compare xs ys

instance Num NatBin where
    fromInteger = mkNatBin 
    (+) = addNatBin
    (-) = subNatBin
    (*) = mulNatBin
    abs = id
    signum _ = NatBin [1]

addNatBin :: NatBin -> NatBin -> NatBin
addNatBin (NatBin xs) (NatBin[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]) = NatBin xs
addNatBin (NatBin[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]) (NatBin ys) = NatBin ys
addNatBin (NatBin xs) (NatBin ys) = NatBin (reverse (addBits (reverse xs) (reverse ys) 0))
    where
        addBits [] [] 0 = []
        addBits [] [] c = error "Overflow: result is too big."
        addBits [] (y:ys) c = (y + c) `mod` 2 : addBits [] ys ((y + c) `div` 2)
        addBits (x:xs) [] c = (x + c) `mod` 2 : addBits xs [] ((x + c) `div` 2)
        addBits (x:xs) (y:ys) c = (x + y + c) `mod` 2 : addBits xs ys ((x + y + c) `div` 2)

subNatBin :: NatBin -> NatBin -> NatBin
subNatBin (NatBin xs) (NatBin[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]) = NatBin xs
subNatBin (NatBin xs) (NatBin ys) = NatBin (reverse(subBits (reverse xs) (reverse ys) 0))
    where
        subBits [] [] 0 = []
        subBits [] [] _ = error "Negative result"
        subBits (x:xs) [] c = (x - c) `mod` 2 : subBits xs [] (if x - c < 0 then 1 else 0)
        subBits [] (y:ys) _ = error "Negative result"
        subBits (x:xs) (y:ys) c = (x - y - c) `mod` 2 : subBits xs ys (if x - y - c < 0 then 1 else 0)

mulNatBin :: NatBin -> NatBin -> NatBin
mulNatBin _ (NatBin[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]) = zero
mulNatBin (NatBin[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]) _ = zero
mulNatBin (NatBin xs) (NatBin ys) = mulNatBinAux (NatBin xs) (NatBin ys) 31

mulNatBinAux :: NatBin -> NatBin -> Int -> NatBin
mulNatBinAux _ _ (-1) = zero
mulNatBinAux (NatBin xs) (NatBin (y:ys)) offset = addNatBin (mulNatBinAux (NatBin xs) (NatBin ys) (offset - 1)) (mulByDigit (NatBin xs) y offset) 

mulByDigit :: NatBin -> Int -> Int -> NatBin
mulByDigit (NatBin xs) digit offset =
    let result = map (* digit) xs ++ replicate offset 0
        trimmedResult = dropZerosUpToOffset offset result
    in
        if length trimmedResult > maxNatBinLength then
            error "Overflow: result is too big."
        else
            NatBin trimmedResult

dropZerosUpToOffset :: Int -> [Int] -> [Int]
dropZerosUpToOffset 0 xs = xs
dropZerosUpToOffset _ [] = []
dropZerosUpToOffset offset (x:xs)
    | x == 0 && offset > 0 = dropZerosUpToOffset (offset - 1) xs
    | otherwise = x : xs

modNatBin :: NatBin -> NatBin -> NatBin
modNatBin _ (NatBin[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]) = error "Division by zero"
modNatBin (NatBin xs) (NatBin ys) 
    | (NatBin xs) < (NatBin ys) = NatBin xs
    | otherwise = modNatBin (subNatBin (NatBin xs) (NatBin ys)) (NatBin ys)

divNatBin :: NatBin -> NatBin -> (NatBin, NatBin)
divNatBin _ (NatBin[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]) = error "Division by zero"
divNatBin (NatBin xs) (NatBin ys) = divAux (NatBin xs) (NatBin ys) zero
    where
        divAux r d q
            | r < d = (q, r)
            | otherwise = divAux (subNatBin r d) d (addNatBin q (mkNatBin 1))

