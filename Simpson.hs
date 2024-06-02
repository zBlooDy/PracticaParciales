data Simpson = Simpson {
    nombre :: String, 
    dinero :: Int,
    felicidad :: Int
} deriving (Eq)

------------
-- Mapeos --
------------

mapFelicidad :: (Int -> Int) -> Simpson -> Simpson
mapFelicidad f unSimpson = unSimpson {felicidad = f $ felicidad unSimpson}

mapDinero :: (Int -> Int) -> Simpson -> Simpson
mapDinero f unSimpson = unSimpson {dinero = f $ dinero unSimpson}
-----------
--Punto 1--
-----------

type Actividad = Simpson -> Simpson

-- <- Ir a la escuela ->
irALaEscuela :: Actividad
irALaEscuela unSimpson 
  | nombre unSimpson == "Lisa" = aumentarFelicidad 20 unSimpson
  | otherwise                  = disminuirFelicidad 20 unSimpson

aumentarFelicidad :: Int -> Simpson -> Simpson
aumentarFelicidad unaFelicidad = mapFelicidad (unaFelicidad +)

disminuirFelicidad :: Int -> Simpson -> Simpson
disminuirFelicidad unaFelicidad = mapFelicidad (max 0 . subtract unaFelicidad)

-- <- Comer donas ->
comerDonas :: Int -> Actividad
comerDonas cantidadDonas = disminuirDinero (10 * cantidadDonas) . aumentarFelicidad (10 * cantidadDonas)

disminuirDinero :: Int -> Simpson -> Simpson
disminuirDinero unDinero = mapDinero (subtract unDinero)

-- <- Ir a trabajar ->

irATrabajar :: String -> Actividad
irATrabajar unTrabajo = aumentarDinero (length unTrabajo)

aumentarDinero :: Int -> Simpson -> Simpson
aumentarDinero unDinero = mapDinero (unDinero +)

irATrabajarComoDirector :: Actividad
irATrabajarComoDirector = irATrabajar ("Escuela Elemental") . disminuirFelicidad 20

