
data Perrito = Perrito {
    raza :: String,
    juguetesFavoritos :: [Juguete],
    tiempoEnGuarderia :: Int,
    energia :: Int
}

type Juguete = String

data Guarderia = Guarderia {
    nombre :: String,
    rutina :: [Actividad]
}

type Actividad = (Ejercicio, Tiempo)

type Tiempo = Int
type Ejercicio = Perrito -> Perrito

tiempoActividad :: Actividad -> Tiempo
tiempoActividad = snd

ejercicioDeActividad :: Actividad -> Ejercicio
ejercicioDeActividad = fst
-- <- Mapeos ->

mapEnergia :: (Int -> Int) -> Perrito -> Perrito
mapEnergia f unPerrito = unPerrito {energia = f $ energia unPerrito}

mapJuguete :: ([Juguete] -> [Juguete]) -> Perrito -> Perrito
mapJuguete f unPerrito = unPerrito {juguetesFavoritos = f $ juguetesFavoritos unPerrito}

-----------
--Parte A--
-----------

-- <- Jugar ->
jugar :: Ejercicio
jugar = cansarPerrito 10

cansarPerrito :: Int -> Perrito -> Perrito
cansarPerrito unaEnergia = mapEnergia (subtract unaEnergia)

-- <- Ladrar ->
ladrar :: Int -> Ejercicio 
ladrar cantidadLadridos = aumentarEnergia (mitadLadridos)
    where
        mitadLadridos = cantidadLadridos `div` 2

aumentarEnergia :: Int -> Perrito -> Perrito
aumentarEnergia unaEnergia = mapEnergia (+ unaEnergia)

-- <- Regalar ->
regalar :: Juguete -> Ejercicio
regalar unJuguete = agregarJuguete (unJuguete)

agregarJuguete :: Juguete -> Perrito -> Perrito
agregarJuguete unJuguete = mapJuguete (unJuguete :)

-- <- Dia de Spa ->
diaDeSpa :: Ejercicio
diaDeSpa unPerrito
  | permaneceMuchoTiempo || esExtravagante unPerrito = regalar ("Peine de goma") . completarEnergia $ unPerrito
  | otherwise                                                  = unPerrito
  where
    permaneceMuchoTiempo = (>= 50) . tiempoEnGuarderia $ unPerrito

completarEnergia :: Perrito -> Perrito
completarEnergia = mapEnergia (const 100)

esExtravagante :: Perrito -> Bool
esExtravagante unPerrito = elem (raza unPerrito) razasExtravagantes

razasExtravagantes :: [String]
razasExtravagantes = ["Dalmata" , "Pomerania"]

-- <- Dia de campo ->
diaDeCampo :: Ejercicio
diaDeCampo = pierdePrimerJuguete . jugar

pierdePrimerJuguete :: Perrito -> Perrito
pierdePrimerJuguete = mapJuguete (drop 1)

zara :: Perrito
zara = Perrito "Dalmata" ["Pelota", "Mantita"] 90 80

guarderiaPdePerritos :: Guarderia
guarderiaPdePerritos = Guarderia "Guarderia P de Perritos" [(jugar , 10) , (ladrar 18, 20) , (regalar "Pelota" , 0) , (diaDeSpa , 120) , (diaDeCampo , 720)]

-----------
--Parte B--
-----------

puedeEstarEnGuarderia :: Perrito -> Guarderia -> Bool
puedeEstarEnGuarderia unPerrito unaGuarderia = tiempoEnGuarderia unPerrito > tiempoRutina unaGuarderia

tiempoRutina :: Guarderia -> Tiempo
tiempoRutina = sum . map tiempoActividad . rutina


esResponsable :: Perrito -> Bool
esResponsable = tieneMasDeTresJuguetes . diaDeCampo

tieneMasDeTresJuguetes :: Perrito -> Bool
tieneMasDeTresJuguetes = (> 3) . length . juguetesFavoritos


realizarRutinaDe :: Guarderia -> Perrito -> Perrito
realizarRutinaDe unaGuarderia unPerrito
  | puedeEstarEnGuarderia unPerrito unaGuarderia = foldr ($) unPerrito (ejerciciosGuarderia)
  | otherwise                              = unPerrito
  where
    ejerciciosGuarderia = map ejercicioDeActividad (rutina unaGuarderia)


perrosCansados :: Guarderia -> [Perrito] -> [Perrito]
perrosCansados unaGuarderia = filter (estaCansado . realizarRutinaDe unaGuarderia)

estaCansado :: Perrito -> Bool
estaCansado = (< 5) . energia


