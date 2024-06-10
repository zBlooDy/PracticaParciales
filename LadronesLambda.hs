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

-----------
--Parte 3--
-----------

data Ladron = Ladron {
    nombre :: String,
    herramientas :: [Herramienta],
    tesorosRobados :: [Tesoro]
}

data Cofre = Cofre {
    cerradura :: Cerradura,
    tesoro :: Tesoro
}

-- a)
esLegendario :: Ladron -> Bool
esLegendario unLadron = (experiencia unLadron > 100) && sonTodosDeLujo (tesorosRobados unLadron)

experiencia :: Ladron -> Int
experiencia = sum . map valorTesoro . tesorosRobados

sonTodosDeLujo :: [Tesoro] -> Bool
sonTodosDeLujo = all deLujo

-- b)

robarCofre :: Ladron -> Cofre -> Ladron
robarCofre unLadron unCofre = usarHerramientasParaRobar (herramientas unLadron) unCofre unLadron

usarHerramientasParaRobar :: [Herramienta] -> Cofre -> Ladron -> Ladron
usarHerramientasParaRobar [] _ unLadron = unLadron
usarHerramientasParaRobar herramientas unCofre unLadron
  | estaAbierta (cerradura unCofre) = agregarTesoro (tesoro unCofre) . herramientasNoUtilizadas (herramientas) $ unLadron
  | otherwise                       = usarHerramientasParaRobar (drop 1 herramientas) cofreDebilitado unLadron
  where
    cofreDebilitado = usarHerramienta (head herramientas) unCofre


usarHerramienta :: Herramienta -> Cofre -> Cofre
usarHerramienta unaHerramienta unCofre = unCofre {cerradura = unaHerramienta (cerradura unCofre)}

agregarTesoro :: Tesoro -> Ladron -> Ladron
agregarTesoro unTesoro = mapTesoros (unTesoro :)

mapTesoros :: ([Tesoro] -> [Tesoro]) -> Ladron -> Ladron
mapTesoros f unLadron = unLadron {tesorosRobados = f $ tesorosRobados unLadron}

herramientasNoUtilizadas :: [Herramienta] -> Ladron -> Ladron
herramientasNoUtilizadas herramientasSinUsar unLadron = unLadron {herramientas = herramientasSinUsar}


-- c)

atraco :: Ladron -> [Cofre] -> Ladron
atraco = foldl robarCofre 

