-----------
--Parte A--
-----------

-- 1)

data Chico = Chico {
    nombre :: String,
    edad :: Int,
    habilidades :: [Habilidad],
    deseos :: [Deseo]
}

type Habilidad = String
type Deseo = Chico -> Chico


mapHabilidades :: ([Habilidad] -> [Habilidad]) -> Chico -> Chico
mapHabilidades f unChico = unChico {habilidades = f $ habilidades unChico}

mapEdad :: (Int -> Int) -> Chico -> Chico
mapEdad f unChico = unChico {edad = f $ edad unChico}

-- a)

aprenderHabilidades :: [Habilidad] -> Deseo
aprenderHabilidades habilidades = mapHabilidades (habilidades ++)

-- b)

serGrosoEnNeedForSpeed :: Deseo
serGrosoEnNeedForSpeed = mapHabilidades (infinitasVersiones ++)

infinitasVersiones :: [Habilidad]
infinitasVersiones = map (needForSpeeds) [1..]

needForSpeeds :: Int -> Habilidad
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


-----------
--Parte B--
-----------

type Condicion = Chico -> Bool
-- 1)

-- a)
tieneHabilidad :: Habilidad -> Condicion
tieneHabilidad unaHabilidad = elem unaHabilidad . habilidades

-- b)
esSuperMaduro :: Condicion
esSuperMaduro unChico = esMayor unChico && (tieneHabilidad  "manejar" unChico)

esMayor :: Chico -> Bool
esMayor = (> 18) . edad

-- 2)

data Chica = Chica {
    nombreChica :: String,
    condicion :: Condicion
}

-- a)

quienConquistaA :: Chica -> [Chico] -> Chico
quienConquistaA unaChica losPretendientes 
  | null candidatos = last losPretendientes
  | otherwise       = head candidatos
  where
    candidatos = filter (condicion unaChica) losPretendientes

-- b)

martu :: Chica
martu = Chica "Martina" (tieneHabilidad "cocinar")

-- Ejemplo consulta :
-- quienConquistaA martu [timmy, mati, ale]

-----------
--Parte C--
-----------

infractoresDeDaRules :: [Chico] -> [String]
infractoresDeDaRules = map nombre . filter (tieneDeseoProhibido)

tieneDeseoProhibido :: Chico -> Bool
tieneDeseoProhibido = any esProhibida . take 5 . habilidades . muffinMagico

esProhibida :: Habilidad -> Bool
esProhibida unaHabilidad = elem unaHabilidad habilidadesProhibidas

habilidadesProhibidas :: [Habilidad]
habilidadesProhibidas = ["enamorar" , "matar" , "dominar el mundo"]

-----------
--Parte D--
-----------

-- Composicion se utiliza en casi todas las funciones
-- Orden superior se utiliza en las genericas como mapEdad, mapHabilidades
-- Aplicacion parcial en esMayor x ejemplo
-- Listas infinitas en jugarNeedForSpeeds