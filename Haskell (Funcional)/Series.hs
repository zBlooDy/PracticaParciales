import Text.Show.Functions

data Serie = Serie {
    nombreSerie :: String,
    actores :: [Actor],
    presupuesto :: Int,
    cantidadTemporadas :: Int,
    ratingPromedio :: Float,
    cancelada :: Bool
} deriving(Show)

data Actor = Actor {
    nombreActor :: String,
    sueldoAnual :: Int,
    restricciones :: [Restriccion]
}deriving(Show)

type Restriccion = Actor -> Bool

-------------
-- Punto 1 --
-------------

estaEnRojo :: Serie -> Bool
estaEnRojo unaSerie = presupuesto unaSerie > sumaSueldos (actores unaSerie)

sumaSueldos :: [Actor] -> Int
sumaSueldos listaDeActores = sum (map sueldoAnual listaDeActores)

esProblematica :: Serie -> Bool
esProblematica unaSerie = (>3) .  actoresConXRestricciones 2 $ actores unaSerie 

actoresConXRestricciones :: Int -> [Actor] -> Int
actoresConXRestricciones cantidad = length . filter ((>= cantidad) . length . restricciones)

-------------
-- Punto 2 --
-------------
type Produccion = Serie -> Serie

conFavoritismos :: [Actor] -> Produccion
conFavoritismos listaFavoritos = agregaActores listaFavoritos . sacarActores 2 

sacarActores :: Int -> Serie -> Serie
sacarActores cantidad = mapActores (drop cantidad)

agregaActores :: [Actor] -> Serie -> Serie
agregaActores listaActores = mapActores (listaActores ++)

mapActores :: ([Actor] -> [Actor]) -> Serie -> Serie
mapActores f unaSerie = unaSerie {actores = f $ actores unaSerie}

johnnydepp :: Actor
johnnydepp = Actor{
    nombreActor = "Johnny Depp",
    sueldoAnual = 20000000,
    restricciones = []
}

helenabonham :: Actor
helenabonham = Actor {
    nombreActor = "Helena Bonham",
    sueldoAnual = 15000000,
    restricciones = []
}

timBurton :: Produccion
timBurton = conFavoritismos [johnnydepp, helenabonham]

gatoPardeitor :: Produccion
gatoPardeitor unaSerie = unaSerie 

estireitor :: Produccion
estireitor = duplicarTemporadas

duplicarTemporadas :: Serie -> Serie
duplicarTemporadas = mapTemporadas (*2)

mapTemporadas :: (Int -> Int) -> Serie -> Serie
mapTemporadas f unaSerie = unaSerie {cantidadTemporadas = f $ cantidadTemporadas unaSerie}


desespereitor :: [Produccion] -> Produccion
desespereitor listaProducciones unaSerie = foldl (\x f -> f x) unaSerie listaProducciones

canceleitor :: Float -> Produccion
canceleitor cifra unaSerie 
    | estaEnRojo unaSerie || ratingPromedio unaSerie < cifra  = unaSerie {cancelada = True}
    | otherwise                                               = unaSerie

-------------
-- Punto 3 --
-------------

bienestarSerie :: Serie -> Int
bienestarSerie (Serie _ _ _ _ _ True) = 0

bienestarTotalSerie :: Serie -> Int
bienestarTotalSerie unaSerie = bienestarSegunTemporadas unaSerie + bienestarSegunActores unaSerie 

bienestarSegunTemporadas :: Serie -> Int
bienestarSegunTemporadas (Serie _ _ cantidadTemporadas _ _ _) 
    | (>4)  cantidadTemporadas  = 5
    | otherwise                 = (10 - cantidadTemporadas) * 2

bienestarSegunActores :: Serie -> Int
bienestarSegunActores (Serie _ actores _ _ _ _)
    | (<10) . length  $ actores = 3
    | otherwise                 = 10 - actoresConXRestricciones 2 actores  

-------------
-- Punto 4 --
-------------

aplicaSeriesEfectivas :: [Serie] -> [Produccion] -> [Serie]
aplicaSeriesEfectivas listaSeries listaProductores = map (produccionMasEfectiva listaProductores) listaSeries 


produccionMasEfectiva :: [Produccion] -> Serie -> Serie
produccionMasEfectiva [produccion] unaSerie = produccion unaSerie
produccionMasEfectiva (produccion1:produccion2:producciones) unaSerie
    | bienestarTotalSerie (produccion1 unaSerie) <= bienestarTotalSerie (produccion2 unaSerie) = produccionMasEfectiva (produccion2:producciones) unaSerie
    | otherwise = produccion1 unaSerie


-------------
-- Punto 5 --
-------------

--  a) Si, se puede aplicar ya que el productor gatopardeitor no realiza ninguna operacion relacionada a los actores de una Serie

--  b) Si, se puede aplicar conFavoritismos. En mi ejemplo agrego los favoritos al principio, en cambio si se agregarian al final de la lista seria un problema ya que se va a quedar buscando el final de la lista para concatenarlos


-------------
-- Punto 6 --
-------------
esControvertida :: Serie -> Bool
esControvertida unaSerie = sueldosMenorAMayor (actores unaSerie)

sueldosMenorAMayor :: [Actor] -> Bool
sueldosMenorAMayor (x:y:xs) = sueldoAnual x < sueldoAnual y && sueldosMenorAMayor (y:xs)

-------------
-- Punto 7 --
-------------

funcionLoca x y z= filter (even . x) . map (length.y)  $ z

-- La funcion x va a recibir un entero, porque recibe una lista de enteros y va a devolver un valor el cual debe poder verificarse que sea Par
-- La funcion y tiene que recibe un valor del tipo de la lista y tiene que devolver una lista, por el hecho de que length trabaja con listas
-- El parametro z va a ser una lista de cualquier cosa
-- El resultado de la funcion va a ser una lista de enteros ya que lo ultimo que hace es filtrar por los pares.

-- funcionLoca :: (Integral b) => (Int -> b) -> ( a -> [b] ) -> [a] -> [Int]