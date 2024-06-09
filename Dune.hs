-- <- Mapeos ->

mapReconocimiento :: (Int -> Int) -> Fremen -> Fremen
mapReconocimiento f unFremen = unFremen {reconocimiento = f $ reconocimiento unFremen}

-----------
--Punto 1--
-----------

data Fremen = Fremen {
    nombre :: String,
    nivelTolerancia :: Int,
    titulos :: [Titulo],
    reconocimiento :: Int
} 

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

