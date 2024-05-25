import Text.Show.Functions
import Data.Char (intToDigit)

-----------
--Parte A--
-----------

-- 1)
data Personaje = Personaje {
    nombre :: String,
    poder :: Int,
    enemigosDerrotados :: [Derrota],
    equipamientos :: [Equipamiento]
} deriving (Show)

data Derrota = Derrota {
    nombreOponente :: String,
    anio :: Int
}deriving (Show)




-- 2)
entrenamiento :: [Personaje] -> [Personaje]
entrenamiento unGrupo = map (multiplicaPoder.length $ unGrupo) unGrupo

multiplicaPoder :: Int -> Personaje -> Personaje
multiplicaPoder unPoder = mapPoder (unPoder *)

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


-----------
--Parte B--
-----------
-- 1)
type Equipamiento = Personaje -> Personaje 


-- 2)
escudo :: Equipamiento
escudo unPersonaje 
   | (< 5).length.enemigosDerrotados $ unPersonaje = aumentaPoder 50 unPersonaje
   | otherwise                                     = quitaPoder 100 unPersonaje

aumentaPoder :: Int -> Personaje -> Personaje
aumentaPoder unPoder = mapPoder (unPoder +)

quitaPoder :: Int -> Personaje -> Personaje
quitaPoder unPoder = mapPoder (subtract unPoder)

trajeMecanizado :: Int -> Equipamiento
trajeMecanizado version unPersonaje = unPersonaje {nombre = "Iron " ++ nombre unPersonaje ++ " V" ++ [intToDigit version]}


-- 3)

-- a)
stormBreaker :: Equipamiento
stormBreaker unPersonaje
   | nombre unPersonaje == "Thor" = agregaSufijo "dios del trueno" . limpiaHistorialDerrotas $ unPersonaje
   | otherwise                    = unPersonaje

agregaSufijo :: String -> Personaje -> Personaje
agregaSufijo sufijo unPersonaje = unPersonaje {nombre = nombre unPersonaje ++ sufijo}

limpiaHistorialDerrotas :: Personaje -> Personaje 
limpiaHistorialDerrotas unPersonaje = unPersonaje {enemigosDerrotados = []}


-- gemaDelAlma :: Equipamiento
-- gemaDelAlma unPersonaje
--     | nombre unPersonaje == "Thanos" = agregarDerrotasExtras unPersonaje


agregarDerrotasExtras = iterate (+1) 2018


-- c)
-- guanteleteInfinito :: [Equipamiento] -> Equipamiento
-- guianteleteInfinito listaEquipamientos unPersonaje = foldl (\x f -> f x) unPersonaje (filter esGemaDelInfinito listaEquipamientos)

