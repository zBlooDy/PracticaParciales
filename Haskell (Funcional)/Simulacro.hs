-- Escribí tu código acá
data Auto = Auto {
  marca :: String,
  modelo :: String,
  desgaste :: (Ruedas, Chasis),
  velocidadMax :: Float,
  tiempoDeCarrera :: Float
}

type Ruedas = Float
type Chasis = Float

-----------
--Punto 1--
-----------

autoFerrari :: Auto
autoFerrari = Auto {
  marca = "Ferrari",
  modelo = "F50",
  desgaste = (0,0),
  velocidadMax = 65,
  tiempoDeCarrera = 0
}

autoLamborghini :: Auto
autoLamborghini = Auto {
  marca = "Lamborghini",
  modelo = "Diablo",
  desgaste = (4,7),
  velocidadMax = 73,
  tiempoDeCarrera = 0
}

autoFiat :: Auto
autoFiat = Auto {
  marca = "Fiat",
  modelo = "600",
  desgaste = (27,33),
  velocidadMax = 44,
  tiempoDeCarrera = 0
}

-----------
--Punto 2--
-----------

estaEnBuenEstado :: Auto -> Bool
estaEnBuenEstado unAuto = desgasteChasis unAuto < 40 && desgasteRuedas unAuto < 60

desgasteChasis :: Auto -> Float
desgasteChasis = snd . desgaste

desgasteRuedas :: Auto -> Float
desgasteRuedas = fst . desgaste

noDaParaMas :: Auto -> Bool
noDaParaMas unAuto = desgasteChasis unAuto > 80 || desgasteRuedas unAuto > 80

-----------
--Punto 3--
-----------

reparacionAuto :: Auto -> Auto
reparacionAuto unAuto = bajarDesgasteChasis (0.85 * desgasteChasis unAuto) . anularDesgasteRuedas $ unAuto

bajarDesgasteChasis :: Float -> Auto -> Auto
bajarDesgasteChasis unDesgaste = mapDesgaste (modificarChasis (subtract unDesgaste))

modificarChasis :: (Float -> Float) -> (Ruedas, Chasis) -> (Ruedas, Chasis)
modificarChasis f (ruedas, chasis) = (ruedas, f $ chasis)

modificarRuedas :: (Float -> Float) -> (Ruedas, Chasis) -> (Ruedas, Chasis)
modificarRuedas f (ruedas, chasis) = (f $ ruedas, chasis)

mapDesgaste :: ((Ruedas, Chasis) -> (Ruedas, Chasis)) -> Auto -> Auto
mapDesgaste f unAuto = unAuto {desgaste = f $ desgaste unAuto}

anularDesgasteRuedas :: Auto -> Auto
anularDesgasteRuedas unAuto = unAuto {desgaste = (0, desgasteChasis unAuto)}

-----------
--Punto 4--
-----------


type Tramo = Auto -> Auto

type Tiempo = Float
-- A) 
curva :: Float -> Float -> Tramo
curva angulo longitud unAuto = desgastarRuedas (desgasteCurva angulo longitud) . sumarTiempo (tiempoCurva longitud unAuto) $ unAuto

desgasteCurva :: Float -> Float -> Float
desgasteCurva angulo longitud = 3 * longitud / angulo

tiempoCurva :: Float -> Auto -> Float
tiempoCurva longitud (Auto _ _ _ velocidadMax _) = longitud / (velocidadMax / 2)

desgastarRuedas :: Float -> Auto -> Auto
desgastarRuedas unDesgaste = mapDesgaste (modificarRuedas (+ unDesgaste))

sumarTiempo :: Tiempo -> Auto -> Auto
sumarTiempo unTiempo = mapTiempo (unTiempo +)

mapTiempo :: (Tiempo -> Tiempo) -> Auto -> Auto
mapTiempo f unAuto = unAuto {tiempoDeCarrera =  f $ tiempoDeCarrera unAuto}


curvaPeligrosa :: Tramo
curvaPeligrosa = curva 60 300

curvaTranca :: Tramo
curvaTranca = curva 110 550

-- B)

tramoRecto :: Float -> Tramo
tramoRecto longitud unAuto = subirDesgasteChasis (0.1 * longitud) . sumarTiempo (tiempoRecto longitud unAuto) $ unAuto

subirDesgasteChasis :: Float -> Auto -> Auto
subirDesgasteChasis unDesgaste = mapDesgaste(modificarChasis(+ unDesgaste))

tiempoRecto :: Float -> Auto -> Float
tiempoRecto longitud (Auto _ _ _ velocidadMax _) = longitud / velocidadMax

tramoRectoClassic :: Tramo
tramoRectoClassic = tramoRecto 750

tramito :: Tramo
tramito = tramoRecto 280

-- C)
tiempoDeUnTramo :: Tramo -> Auto -> Float
tiempoDeUnTramo unTramo unAuto = tiempoDeCarrera (unTramo unAuto) - tiempoDeCarrera unAuto

boxes :: Tramo -> Tramo
boxes unTramo unAuto 
  | estaEnBuenEstado unAuto = unTramo unAuto
  | otherwise               = sumarTiempo (tiempoDeUnTramo unTramo unAuto) . reparacionAuto $ unAuto

-- D)
tramoMojado :: Tramo -> Tramo
tramoMojado unTramo unAuto = sumarTiempo ((tiempoDeUnTramo unTramo unAuto)  / 2) unAuto

-- E)
tramoConRipio :: Tramo -> Tramo
tramoConRipio unTramo unAuto = sumarTiempo (2 * tiempoDeUnTramo unTramo unAuto) . unTramo . unTramo $ unAuto 

-- F) 
tramoConObstruccion :: Tramo -> Float -> Tramo
tramoConObstruccion unTramo metrosOcupados = unTramo . desgastarRuedas (2 * metrosOcupados)

-----------
--Punto 5--
-----------

pasarPorTramo :: Tramo -> Auto -> Auto
pasarPorTramo unTramo unAuto
  | noDaParaMas unAuto = unAuto
  | otherwise          = unTramo unAuto

-----------
--Punto 6--
-----------

type Pista = [Tramo]
-- A)

superPista :: Pista
superPista = [tramoRectoClassic, curvaTranca, tramoMojado tramito , tramito, tramoConObstruccion (curva 80 400) 2, curva 115 650, tramoRecto 970, curvaPeligrosa, tramoConRipio tramito, boxes (tramoRecto 800)]

-- B) 
peganLaVuelta :: Pista -> [Auto] -> [Auto]
peganLaVuelta unaPista = filter (not . noDaParaMas) . map (aplicarTramos unaPista)

aplicarTramos :: Pista -> Auto -> Auto
aplicarTramos unaPista unAuto = foldl (\auto tramo -> pasarPorTramo tramo auto) unAuto unaPista

-----------
--Punto 7--
-----------
-- A)

type Carrera = (Pista, Vueltas)
type Vueltas = Int

-- B)

tourBuenosAires :: Carrera
tourBuenosAires = (superPista, 20)

-- C)

jugarUnaCarrerra :: Carrera -> [Auto] -> [Auto]
jugarUnaCarrerra (pista, vueltas) = take vueltas . vanCorriendoVueltas pista


vanCorriendoVueltas :: Pista -> [Auto] -> [Auto]
vanCorriendoVueltas unaPista = iterate (peganLaVuelta unaPista)

