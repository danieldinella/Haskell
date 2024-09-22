data MioBool = Vero | Falso

instance Ord MioBool where
    Vero == Vero = Vero
    Falso == Falso = Vero
    _ == _ = Falso
    Falso < True = Vero
    _ < _ = Falso