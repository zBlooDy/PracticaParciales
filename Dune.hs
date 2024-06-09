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

