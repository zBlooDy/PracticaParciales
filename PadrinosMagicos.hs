-----------
--Parte A--
-----------

-- 1)

data Chico = Chico {
    nombre :: String,
    edad :: Int,
    habilidades :: [String],
    deseos :: [Deseo]
}

type Deseo = Chico -> Chico


mapHabilidades :: ([String] -> [String]) -> Chico -> Chico
mapHabilidades f unChico = unChico {habilidades = f $ habilidades unChico}

mapEdad :: (Int -> Int) -> Chico -> Chico
mapEdad f unChico = unChico {edad = f $ edad unChico}

-- a)

aprenderHabilidades :: [String] -> Deseo
aprenderHabilidades habilidades = mapHabilidades (habilidades ++)

-- b)

serGrosoEnNeedForSpeed :: Deseo
serGrosoEnNeedForSpeed = mapHabilidades (infinitasVersiones ++)

infinitasVersiones :: [String]
infinitasVersiones = map (needForSpeeds) [1..]

needForSpeeds :: Int -> String
needForSpeeds unNumero = "jugar need for speed " ++ show unNumero

-- c)
serMayor :: Deseo
serMayor = mapEdad (const 18)

-- 2)

-- a)
type Padrino = Chico -> Chico

wanda :: Padrino
wanda = madurarAnios 1 . cumplirPrimerDeseo

cumplirPrimerDeseo :: Chico -> Chico
cumplirPrimerDeseo unChico = head (deseos unChico) $ unChico

madurarAnios :: Int -> Chico -> Chico
madurarAnios anios = mapEdad (+ anios)

-- b)
cosmo :: Padrino
cosmo unChico = desmadurar (edad unChico `div` 2) unChico

desmadurar :: Int -> Chico -> Chico
desmadurar unaEdad = mapEdad (subtract unaEdad)

-- c)
muffinMagico :: Padrino
muffinMagico unChico = foldl (\chico deseo -> deseo chico) unChico (deseos unChico)


