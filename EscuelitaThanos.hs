-- <--- Primera Parte --->

-- <- Mapeos ->

mapEnergia :: (Float -> Float) -> Personaje -> Personaje
mapEnergia f unPersonaje = unPersonaje {energia = f $ energia unPersonaje}

mapHabilidad :: ([Habilidad] -> [Habilidad]) -> Personaje -> Personaje
mapHabilidad f unPersonaje = unPersonaje {habilidades = f $ habilidades unPersonaje}
-----------
--Punto 1--
-----------

data Guantelete = Guantelete {
    material :: String,
    gemas :: [Gema]
}

type Gema = Personaje -> Personaje

data Personaje = Personaje {
    nombre :: String,
    edad :: Int,
    energia :: Float,
    habilidades :: [Habilidad],
    planeta :: String
}

type Habilidad = String

type Universo = [Personaje]

chasquidoUniverso :: Guantelete -> Universo -> Universo
chasquidoUniverso unGuantelete unUniverso
  | estaCompleto unGuantelete = reducirALaMitad unUniverso
  | otherwise                 = unUniverso


estaCompleto :: Guantelete -> Bool
estaCompleto unGuantelete = ((== 6). length . gemas $ unGuantelete) && ((== "uru") . material $ unGuantelete)

reducirALaMitad :: Universo -> Universo
reducirALaMitad unUniverso = take (length unUniverso `div` 2) unUniverso

-----------
--Punto 2--
-----------
esAptoParaPendex :: Universo -> Bool
esAptoParaPendex = any esPendex

esPendex :: Personaje -> Bool
esPendex = (< 45) . edad

energiaTotal :: Universo -> Float
energiaTotal = sum . map energia . filter (masDeUnaHabilidad)

masDeUnaHabilidad :: Personaje -> Bool
masDeUnaHabilidad = (> 1) . length . habilidades

-- <--- Segunda Parte --->

-----------
--Punto 3--
-----------

-- <La mente>
mente :: Float -> Gema
mente unValor = debilitarEnergia (unValor)

debilitarEnergia :: Float -> Personaje -> Personaje
debilitarEnergia unValor = mapEnergia (subtract unValor)

-- <El alma>
alma :: Habilidad -> Gema
alma unaHabilidad = debilitarEnergia 10 . eliminarHabilidad unaHabilidad

eliminarHabilidad :: Habilidad -> Personaje -> Personaje
eliminarHabilidad unaHabilidad = mapHabilidad (filter (/= unaHabilidad))

-- <El espacio>
espacio :: String -> Gema
espacio unPlaneta = debilitarEnergia 20 . transportarAlPlaneta unPlaneta

transportarAlPlaneta :: String -> Personaje -> Personaje
transportarAlPlaneta nuevoPlaneta unPersonaje = unPersonaje {planeta = nuevoPlaneta}

-- <El poder>

poder :: Gema
poder = anularEnergia . quitarHabilidadesSegun 2

anularEnergia :: Personaje -> Personaje
anularEnergia = mapEnergia (const 0)

quitarHabilidadesSegun :: Int -> Personaje -> Personaje
quitarHabilidadesSegun unValor unPersonaje
  | cantidadHabilidades <= unValor = mapHabilidad (const []) unPersonaje
  | otherwise                      = unPersonaje
  where
    cantidadHabilidades = length . habilidades $ unPersonaje

-- <El tiempo>
tiempo :: Gema
tiempo = debilitarEnergia 50 . reducirEdad

reducirEdad :: Personaje -> Personaje
reducirEdad unPersonaje = unPersonaje {edad = edadSegunGemaTiempo}
    where
        edadSegunGemaTiempo = max 18 (edad unPersonaje `div` 2)

-- <La gema loca>

gemaLoca :: Gema -> Gema
gemaLoca unaGema = unaGema . unaGema 

-----------
--Punto 4--
-----------

guanteleteFlojo :: Guantelete
guanteleteFlojo = (Guantelete "Goma" [tiempo, alma "usar Mjolnir", gemaLoca (alma "programacion en Haskell")])

-----------
--Punto 5--
-----------

utilizar :: [Gema] -> Personaje -> Personaje
utilizar listaGemas unEnemigo = foldl (\enemigo gema -> gema enemigo) unEnemigo listaGemas

-----------
--Punto 6--
-----------

gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa (Guantelete _ [gema]) _ = gema
gemaMasPoderosa (Guantelete material (gema1:gema2:gemas)) unPersonaje
    | energia (gema1 unPersonaje) < energia (gema2 unPersonaje) = gemaMasPoderosa (Guantelete material (gema1:gemas)) unPersonaje
    | otherwise                                                 = gemaMasPoderosa (Guantelete material (gema2:gemas)) unPersonaje