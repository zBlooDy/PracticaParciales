-- DATAS ======================================================
import Text.Show.Functions
import Data.List(genericLength)
import Data.Char(isUpper)
import Data.ByteString (unpack)

data Plomero = Plomero {
    nombre :: String,
    cajaDeHerramientas :: [Herramienta],
    historialReparaciones :: [Reparacion],
    cantidadDinero :: Float
} deriving (Show)

data Herramienta = Herramienta {
    denominacion :: String,
    materialEmpuniadura :: Material,
    precio :: Float
} deriving (Show,Eq)

data Material = Hierro | Madera | Goma | Plastico deriving (Show,Eq)


-- PUNTO 1 ======================================================

llaveInglesa = Herramienta {
    denominacion = "Llave inglesa",
    precio = 200,
    materialEmpuniadura = Hierro
}


martillo = Herramienta {
    denominacion = "Martillo",
    precio = 20,
    materialEmpuniadura = Madera
}

llaveFrancesa = Herramienta {
    denominacion = "Llave francesa",
    precio = 1,
    materialEmpuniadura = Hierro
}

mario = Plomero {
    nombre = "Mario",
    cajaDeHerramientas = [llaveInglesa, martillo],
    historialReparaciones = [],
    cantidadDinero = 1200
}

wario = Plomero {
    nombre = "Wario",
    cajaDeHerramientas = map (Herramienta "Llave francesa" Hierro) [1..],
    historialReparaciones = [],
    cantidadDinero = 0.5
}

-- PUNTO 2 ======================================================

tieneHerramienta :: Herramienta -> Plomero -> Bool
tieneHerramienta unaHerramienta unPlomero = elem unaHerramienta $ cajaDeHerramientas unPlomero

esMalvado :: Plomero -> Bool
esMalvado unPlomero = ("Wa" ==).take 2 $ nombre unPlomero

puedeComprar :: Plomero -> Herramienta -> Bool
puedeComprar unPlomero unaHerramienta = cantidadDinero unPlomero >= precio unaHerramienta

-- PUNTO 3 ======================================================

esBuenaHerramienta :: Herramienta -> Bool
esBuenaHerramienta (Herramienta _ Hierro precio ) = precio >= 10000
esBuenaHerramienta (Herramienta "Martillo" Madera _ ) = True
esBuenaHerramienta (Herramienta "Martillo" Goma _) = True
esBuenaHerramienta _ = False

-- PUNTO 4 ======================================================

comprarHerramienta :: Plomero -> Herramienta -> Plomero
comprarHerramienta unPlomero unaHerramienta 
    | puedeComprar unPlomero unaHerramienta = quitaDinero (precio unaHerramienta) . agregaHerramienta unaHerramienta $ unPlomero
    | otherwise                             = unPlomero


agregaHerramienta :: Herramienta -> Plomero -> Plomero
agregaHerramienta unaHerramienta  = mapHerramienta (unaHerramienta :) 

mapHerramienta :: ([Herramienta] -> [Herramienta]) -> Plomero -> Plomero
mapHerramienta f unPlomero = unPlomero {cajaDeHerramientas = f $ cajaDeHerramientas unPlomero}

quitaDinero :: Float -> Plomero -> Plomero
quitaDinero unDinero  = mapDinero (subtract unDinero) 

mapDinero :: (Float -> Float) -> Plomero -> Plomero
mapDinero f unPlomero = unPlomero {cantidadDinero = f $ cantidadDinero unPlomero}

-- PUNTO 5 ======================================================

data Reparacion = Reparacion {
    descripcion :: String,
    requerimiento :: Plomero -> Bool   
} deriving (Show)

--a) 
filtracionAgua :: Reparacion
filtracionAgua = Reparacion {
    descripcion = "Filtracion de agua",
    requerimiento = tieneHerramienta llaveInglesa 
}

--b)
esDificil :: Reparacion -> Bool
esDificil (Reparacion descripcion _) = length descripcion > 100 && todasMayusculas descripcion
 
todasMayusculas = all isUpper 

--c) 
presupuestoReparacion :: Reparacion -> Float
presupuestoReparacion (Reparacion descripcion _) = (*3). genericLength $ descripcion

-- PUNTO 6 ======================================================

hacerUnaReparacion :: Plomero -> Reparacion -> Plomero
hacerUnaReparacion unPlomero unaReparacion
    | puedeReparar unaReparacion unPlomero = agregaReparacion unaReparacion . cambiaHerramientas unaReparacion . aumentarDinero (presupuestoReparacion unaReparacion) $ unPlomero 
    | otherwise = aumentarDinero 100 unPlomero


puedeReparar :: Reparacion -> Plomero -> Bool
puedeReparar unaReparacion unPlomero = requerimiento unaReparacion unPlomero || esMalvado unPlomero && tieneHerramienta martillo unPlomero

aumentarDinero :: Float -> Plomero -> Plomero
aumentarDinero unDinero = mapDinero (+ unDinero) 

agregaReparacion :: Reparacion -> Plomero -> Plomero
agregaReparacion unaReparacion unPlomero = unPlomero {historialReparaciones = unaReparacion : historialReparaciones unPlomero}

cambiaHerramientas :: Reparacion -> Plomero -> Plomero
cambiaHerramientas unaReparacion unPlomero 
    | esMalvado unPlomero     = agregaHerramienta (Herramienta "Destornillador" Plastico 0) unPlomero
    | esDificil unaReparacion = mapHerramienta (filter $ not.esBuenaHerramienta) unPlomero
    | otherwise               = mapHerramienta tail' unPlomero


tail' :: [a] -> [a]
tail' [] = []
tail' (x:xs) = xs

-- PUNTO 7 ======================================================

jornadaDeTrabajo :: [Reparacion] -> Plomero -> Plomero
jornadaDeTrabajo listaReparaciones unPlomero = foldl hacerUnaReparacion unPlomero listaReparaciones


-- PUNTO 8 ======================================================

maximumBy :: Ord b => (a -> b) -> [a] -> a
maximumBy = foldl1 . maxBy

maxBy :: Ord b => (a -> b) -> a -> a -> a
maxBy f x y
  | f x > f y = x
  | otherwise = y

empleadoMas :: (Ord a) => (Plomero -> a) -> [Reparacion] -> [Plomero] -> Plomero
empleadoMas criterio listaReparaciones =  maximumBy (criterio . jornadaDeTrabajo listaReparaciones)

--a)
empleadoMasReparador :: [Reparacion] -> [Plomero] -> Plomero
empleadoMasReparador  = empleadoMas (length . historialReparaciones)

--b)
empleadoMasAdinerado :: [Reparacion] -> [Plomero] -> Plomero
empleadoMasAdinerado = empleadoMas cantidadDinero

--c)

empleadoMasInversor :: [Reparacion] -> [Plomero] -> Plomero
empleadoMasInversor = empleadoMas (sum . map precio . cajaDeHerramientas)