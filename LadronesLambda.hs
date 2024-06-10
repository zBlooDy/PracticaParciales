import Data.Char

-----------
--Parte 1--
-----------

data Tesoro = Tesoro {
    anioDescubrimiento :: Int,
    precio :: Int
}

-- A)

type Tipo = Tesoro -> Bool

deLujo :: Tipo
deLujo unTesoro = (precio unTesoro > 1000) || (antiguedad unTesoro > 200)

antiguedad :: Tesoro -> Int
antiguedad unTesoro = 2024 - (anioDescubrimiento unTesoro)

telaSucia :: Tipo
telaSucia unTesoro = precio unTesoro < 50 && (not . deLujo $ unTesoro)

estandar :: Tipo
estandar unTesoro = (not . deLujo $ unTesoro) && (not . telaSucia $ unTesoro)

-- B)

valorTesoro :: Tesoro -> Int
valorTesoro unTesoro = precio unTesoro + (2 * antiguedad unTesoro)

-----------
--Parte 2--
-----------

type Cerradura = String

estaAbierta :: Cerradura -> Bool
estaAbierta = null

type Herramienta = Cerradura -> Cerradura
-- a)
martillo :: Herramienta
martillo = drop 3

-- b)
llaveMaestra :: Herramienta
llaveMaestra = take 0 

-- c)
ganzuaGancho :: Herramienta
ganzuaGancho = filter (sinMayusculas) . drop 1

sinMayusculas :: Char -> Bool
sinMayusculas = not . isUpper

ganzuaRastrillo :: Herramienta
ganzuaRastrillo = filter (sinNumeros) . drop 1

sinNumeros :: Char -> Bool
sinNumeros = not . isDigit

ganzuaRombo :: String -> Herramienta
ganzuaRombo inscripcion = filter (sinInscripcion inscripcion) . drop 1

sinInscripcion :: String -> Char -> Bool
sinInscripcion unaInscripcion letra = not $ elem letra unaInscripcion

-- d)
tensor :: Herramienta
tensor = map toUpper

-- e)
socotroco :: Herramienta -> Herramienta -> Herramienta
socotroco unaHerramienta otraHerramienta = unaHerramienta .  otraHerramienta