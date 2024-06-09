-- <- Mapeos ->

mapReconocimiento :: (Int -> Int) -> Fremen -> Fremen
mapReconocimiento f unFremen = unFremen {reconocimiento = f $ reconocimiento unFremen}

mapTolerancia :: (Float -> Float) -> Fremen -> Fremen
mapTolerancia f unFremen = unFremen {nivelTolerancia = f $ nivelTolerancia unFremen}

mapTitulo :: ([Titulo] -> [Titulo]) -> Fremen -> Fremen
mapTitulo f unFremen = unFremen {titulos = f $ titulos unFremen}
-----------
--Punto 1--
-----------

data Fremen = Fremen {
    nombre :: String,
    nivelTolerancia :: Float,
    titulos :: [Titulo],
    reconocimiento :: Int
} deriving (Eq)

type Titulo = String

type Tribu = [Fremen]

-- a)

agregarReconocimiento :: Int -> Fremen -> Fremen
agregarReconocimiento unReconocimiento = mapReconocimiento (+ unReconocimiento)

-- b)

algunCandidato :: Tribu -> Bool
algunCandidato = any esCandidato

esCandidato :: Fremen -> Bool
esCandidato unFremen = tieneTitulo "Domador" unFremen && nivelTolerancia unFremen > 100

tieneTitulo :: Titulo -> Fremen -> Bool
tieneTitulo unTitulo = elem unTitulo . titulos

-- c)

hallarElegido :: Tribu -> Fremen
hallarElegido unaTribu = maximoSegun reconocimiento candidatos
    where
        candidatos = filter (esCandidato) unaTribu


maximoSegun :: (Fremen -> Int) -> Tribu -> Fremen
maximoSegun f = foldl1 (maximo f)

maximo ::  (Fremen -> Int) -> Fremen -> Fremen -> Fremen 
maximo f a b
  | f a > f b = a
  | otherwise = b

-----------
--Punto 2--
-----------

data Gusano = Gusano {
    longitud :: Float,
    nivelHidratacion :: Int,
    descripcion :: String
}

reproducir :: Gusano -> Gusano -> Gusano
reproducir unGusano otroGusano = Gusano {
    longitud = 0.1 * mayorLongitud unGusano otroGusano,
    nivelHidratacion = 0,
    descripcion = descripcion unGusano ++ " - " ++ descripcion otroGusano  
    }

mayorLongitud :: Gusano -> Gusano -> Float
mayorLongitud unGusano otroGusano = max (longitud unGusano) (longitud otroGusano)


listaDeCrias :: [Gusano] -> [Gusano] -> [Gusano] 
listaDeCrias gusanos1 gusanos2 = zipWith reproducir gusanos1 gusanos2

listaDeCrias' :: [Gusano] -> [Gusano] -> [Gusano] 
listaDeCrias' gusanos1 gusanos2 = map (uncurry reproducir) (zip gusanos1 gusanos2)

-- zip me deja las dos listas en una lista de a pares ordenados
-- uncurry le aplica a un par ordenado, la funcion que le mando
-- entonces transformo la lista con el map, aplicandole a cada elemento (par ordenado) uncurry reproducir

-----------
--Punto 3--
-----------

type Mision = Gusano -> Fremen -> Fremen

domarGusano :: Mision
domarGusano unGusano unFremen
  | puedeDomarlo = aumentarTolerancia 100 . ganarTitulo ("Domador") $ unFremen
  | otherwise    = bajarTolerancia (0.1 * nivelTolerancia unFremen) unFremen
  where
    puedeDomarlo = (nivelTolerancia unFremen >= longitud unGusano)

aumentarTolerancia :: Float -> Fremen -> Fremen
aumentarTolerancia unaTolerancia = mapTolerancia (unaTolerancia +)

bajarTolerancia :: Float -> Fremen -> Fremen
bajarTolerancia unaTolerancia = mapTolerancia (subtract unaTolerancia)

ganarTitulo :: Titulo -> Fremen -> Fremen
ganarTitulo unTitulo = mapTitulo (unTitulo :)


destruirGusano :: Mision
destruirGusano unGusano unFremen
  | puedeDestruirlo = aumentarTolerancia 100 . agregarReconocimiento 1 $ unFremen
  | otherwise       = bajarTolerancia (0.2 * nivelTolerancia unFremen) unFremen
  where
    puedeDestruirlo = tieneTitulo "Domador" unFremen && nivelTolerancia unFremen < (longitud unGusano / 2)


realizarMisionColectiva :: Gusano -> Tribu -> Mision -> Tribu
realizarMisionColectiva unGusano unaTribu unaMision = map (unaMision unGusano) unaTribu

cambiaElegido :: Gusano -> Tribu -> Mision -> Bool
cambiaElegido unGusano unaTribu unaMision = hallarElegido unaTribu /= (hallarElegido (realizarMisionColectiva unGusano unaTribu unaMision))