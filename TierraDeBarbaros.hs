import Data.Char (toUpper, isUpper)
import Text.Show.Functions

data Barbaro = Barbaro {
    nombre :: String,
    fuerza :: Int,
    habilidades :: [Habilidad],
    objetos :: [Objeto]
} deriving (Show)

type Habilidad = String

type Objeto = Barbaro -> Barbaro

-- <- Mapeos ->
mapFuerza :: (Int -> Int) -> Barbaro -> Barbaro
mapFuerza f unBarbaro = unBarbaro {fuerza = f $ fuerza unBarbaro}

mapHabilidad :: ([Habilidad] -> [Habilidad]) -> Barbaro -> Barbaro
mapHabilidad f unBarbaro = unBarbaro {habilidades = f $ habilidades unBarbaro}
-----------
--Punto 1--
-----------
-- 1)
espada :: Int -> Objeto 
espada peso = aumentarFuerza (2 * peso) 

aumentarFuerza :: Int -> Barbaro -> Barbaro
aumentarFuerza unaFuerza = mapFuerza (+ unaFuerza)

-- 2)
amuletosMisticos :: Habilidad -> Objeto
amuletosMisticos unaHabilidad = mapHabilidad (unaHabilidad :) 

-- 3)
varitasDefectuosas :: Objeto
varitasDefectuosas unBarbaro = unBarbaro {habilidades = ["Hacer magia"]}

-- 4)
ardilla :: Objeto
ardilla = id 

-- 5)
cuerda :: Objeto -> Objeto -> Objeto
cuerda unObjeto otroObjeto = unObjeto . otroObjeto

-----------
--Punto 2--
-----------

megafono :: Objeto
megafono = mapHabilidad (mayusculas . concatenarPlus )

mayusculas :: [String] -> [String]
mayusculas  = map (map toUpper)

concatenarPlus :: [String] -> [String]
concatenarPlus lista = [concat lista] 

megafonoBarbarico :: Objeto
megafonoBarbarico = cuerda ardilla megafono

-----------
--Punto 3--
-----------
type Evento = Barbaro -> Bool
type Aventura = [Evento]

-- 1)

tieneHabilidad :: String -> Barbaro -> Bool
tieneHabilidad habilidad = elem habilidad . habilidades

invasionDeSuciosDuendes :: Evento
invasionDeSuciosDuendes  = tieneHabilidad "Escribir Poesia Atroz"  

-- 2)

cremalleraDelTiempo :: Evento
cremalleraDelTiempo (Barbaro "Faffy" _ _ _) = False
cremalleraDelTiempo (Barbaro "Astro" _ _ _) = False
cremalleraDelTiempo _                       = True

-- 3)

saqueo :: Evento
saqueo unBarbaro = tieneHabilidad "Robar" unBarbaro && esFuerte unBarbaro

esFuerte :: Barbaro -> Bool
esFuerte = (>80) . fuerza


gritoDeGuerra :: Evento
gritoDeGuerra unBarbaro = poderDeGrito unBarbaro >= cantidadLetrasHabilidades unBarbaro

cantidadLetrasHabilidades :: Barbaro -> Int
cantidadLetrasHabilidades = sum . map length . habilidades

poderDeGrito :: Barbaro -> Int
poderDeGrito = (4*) . length . objetos

caligrafia :: Evento
caligrafia = all caligrafiaPerfecta . habilidades

caligrafiaPerfecta :: Habilidad -> Bool
caligrafiaPerfecta unaHabilidad = isUpper (head unaHabilidad) && cantidadVocales unaHabilidad > 3

esVocal :: Char -> Bool
esVocal letra = elem letra "aeiouAEIOU"

cantidadVocales :: String -> Int
cantidadVocales [] = 0
cantidadVocales (x:xs) 
  | esVocal x = 1 + cantidadVocales xs
  | otherwise = 0 + cantidadVocales xs

dave = Barbaro "Dave" 100 ["tejer","escribirPoesia"] [ardilla, varitasDefectuosas]


ritualFechorias :: Barbaro -> Aventura -> Bool
ritualFechorias unBarbaro = any (\evento -> evento unBarbaro) 


sobrevivientes :: [Barbaro] -> Aventura -> [Barbaro]
sobrevivientes listaBarbaros unaAventura = filter (sobrevivenA unaAventura) listaBarbaros


sobrevivenA :: Aventura -> Barbaro -> Bool
sobrevivenA unaAventura unBarbaro = all (\evento -> evento unBarbaro) unaAventura

