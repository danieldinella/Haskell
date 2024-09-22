--mcd di Euclide, versione additiva
mcd x y
    | x Prelude.== y    = x
    | x < y     = mcd(y-x) x
    | otherwise = mcd(x-y) y

--definizione della classe Eq
class Eq a where
    (==), (/=) :: a -> a -> Bool

