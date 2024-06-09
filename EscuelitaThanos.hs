-- <--- Primera Parte --->

-----------
--Punto 1--
-----------

data Guantelete = Guantelete {
    material :: String,
    gemas :: [Gema]
}

type Gema = Personaje -> Personaje

data Personaje = Personaje {
    nombre :: String,
    edad :: Int,
    energia :: Float,
    habilidades :: [String],
    planeta :: String
}

type Universo = [Personaje]

chasquidoUniverso :: Guantelete -> Universo -> Universo
chasquidoUniverso unGuantelete unUniverso
  | estaCompleto unGuantelete = reducirALaMitad unUniverso
  | otherwise                 = unUniverso


estaCompleto :: Guantelete -> Bool
estaCompleto unGuantelete = ((== 6). length . gemas $ unGuantelete) && ((== "uru") . material $ unGuantelete)

reducirALaMitad :: Universo -> Universo
reducirALaMitad unUniverso = take (length unUniverso `div` 2) unUniverso

-----------
--Punto 2--
-----------
esAptoParaPendex :: Universo -> Bool
esAptoParaPendex = any esPendex

esPendex :: Personaje -> Bool
esPendex = (< 45) . edad

energiaTotal :: Universo -> Float
energiaTotal = sum . map energia . filter (masDeUnaHabilidad)

masDeUnaHabilidad :: Personaje -> Bool
masDeUnaHabilidad = (> 1) . length . habilidades

-- <--- Primera Parte --->
