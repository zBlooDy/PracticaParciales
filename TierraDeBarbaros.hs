
data Barbaro = Barbaro {
    nombre :: String,
    fuerza :: Int,
    habilidades :: [Habilidad],
    objetos :: [Objeto]
}

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