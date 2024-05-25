
-----------
--Parte A--
-----------

-- 1)
data Personaje = Personaje {
    nombre :: String,
    poder :: Int,
    enemigosDerrotados :: [Derrota]
} deriving (Show)

data Derrota = Derrota {
    nombreOponente :: String,
    anio :: Int
}deriving (Show)




-- 2)
entrenamiento :: [Personaje] -> [Personaje]
entrenamiento unGrupo = map (aumentaPoder.length $ unGrupo) unGrupo

aumentaPoder :: Int -> Personaje -> Personaje
aumentaPoder unPoder = mapPoder (unPoder *)

mapPoder :: (Int -> Int) -> Personaje -> Personaje
mapPoder f unPersonaje = unPersonaje {poder = f $ poder unPersonaje}

-- 3)
rivalesDignos :: [Personaje] -> [Personaje]
rivalesDignos unGrupo = filter esRivalParaThanos (entrenamiento unGrupo)

esRivalParaThanos :: Personaje -> Bool
esRivalParaThanos unPersonaje = poder unPersonaje > 500 && any ((== "Hijo De Thanos") . nombreOponente) (enemigosDerrotados unPersonaje)

-- 4)

guerraCivil :: Int -> [Personaje] -> [Personaje] -> [Personaje]
guerraCivil unAnio = zipWith (peleaDePersonajes unAnio)

peleaDePersonajes :: Int -> Personaje -> Personaje -> Personaje
peleaDePersonajes anioPelea unPersonaje otroPersonaje
   | poder unPersonaje > poder otroPersonaje = agregarDerrota (Derrota (nombre otroPersonaje) anioPelea) unPersonaje
   | otherwise                               = agregarDerrota (Derrota (nombre unPersonaje) anioPelea) otroPersonaje


agregarDerrota :: Derrota -> Personaje -> Personaje
agregarDerrota unaDerrota = mapDerrota(unaDerrota :)

mapDerrota :: ([Derrota] -> [Derrota]) -> Personaje -> Personaje
mapDerrota f unPersonaje = unPersonaje {enemigosDerrotados = f $ enemigosDerrotados unPersonaje}