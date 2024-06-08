data Ninja = Ninja {
    nombre :: String,
    herramientas :: [Herramienta],
    jutsus :: Jutsu,
    rango :: Int
}

type Herramienta = (String, Int)

nombreHerramienta :: Herramienta -> String
nombreHerramienta = fst

cantidadDisponible :: Herramienta -> Int
cantidadDisponible = snd

mapHerramienta :: ([Herramienta] -> [Herramienta]) -> Ninja -> Ninja
mapHerramienta f unNinja = unNinja {herramientas = f $ herramientas unNinja}

mapRango :: (Int -> Int) -> Ninja -> Ninja
mapRango f unNinja = unNinja {rango = max 0 . f $ rango unNinja}

-----------
--Parte A--
-----------
-- a)

obtenerHerramienta :: Herramienta -> Ninja -> Ninja
obtenerHerramienta unaHerramienta unNinja
  | puedeObtener unaHerramienta unNinja = agregarHerramienta unaHerramienta unNinja
  | otherwise                           = agregarHerramienta (sinExceder unaHerramienta) unNinja


puedeObtener :: Herramienta -> Ninja -> Bool
puedeObtener unaHerramienta unNinja = (sumaDeHerramientas unNinja + cantidadDisponible unaHerramienta) <= 100

sumaDeHerramientas :: Ninja -> Int
sumaDeHerramientas = sum . map cantidadDisponible . herramientas

agregarHerramienta :: Herramienta -> Ninja -> Ninja
agregarHerramienta unaHerramienta = mapHerramienta (unaHerramienta :)

sinExceder :: Herramienta -> Herramienta
sinExceder (nombre, cantidad) = (nombre, min 100 cantidad)

-- b)

usarHerramienta :: Herramienta -> Ninja -> Ninja
usarHerramienta unaHerramienta = mapHerramienta (quitarHerramienta unaHerramienta)

quitarHerramienta :: Herramienta -> [Herramienta] -> [Herramienta]
quitarHerramienta unaHerramienta = filter (/= unaHerramienta) 


-----------
--Parte B--
-----------

data Mision = Mision {
    cantidadDeNinjas :: Int,
    rangoRecomendado :: Int,
    ninjasEnemigos :: [Ninja],
    recompensa :: Herramienta
}

type Equipo = [Ninja]

-- a)

esDesafiante :: Mision -> Equipo -> Bool
esDesafiante unaMision unEquipo = algunNinjaNovato unEquipo (rangoRecomendado unaMision) && cantidadEnemigos unaMision >= 2

algunNinjaNovato :: Equipo -> Int -> Bool
algunNinjaNovato unEquipo unRango = any ((< unRango) . rango) unEquipo

cantidadEnemigos :: Mision -> Int
cantidadEnemigos = length . ninjasEnemigos

-- b)

esCopada :: Mision -> Bool
esCopada unaMision = elem (recompensa unaMision) recompensasCopadas

recompensasCopadas :: [Herramienta]
recompensasCopadas = [("Bomba de Humo", 3), ("Shuriken" , 5), ("Kunai", 14)]


-- c)
esFactible :: Mision -> Equipo -> Bool
esFactible unaMision unEquipo = (not $ esDesafiante unaMision unEquipo) && sonCapaces unEquipo unaMision

sonCapaces :: Equipo -> Mision -> Bool
sonCapaces unEquipo unaMision = ninjasSuficientes unEquipo (cantidadDeNinjas unaMision) || (sumarTodasLasHerramientas unEquipo > 500)

sumarTodasLasHerramientas :: Equipo -> Int
sumarTodasLasHerramientas = sum . map sumaDeHerramientas

ninjasSuficientes :: Equipo -> Int -> Bool
ninjasSuficientes unEquipo cantidad = length unEquipo >= cantidad

-- d)

fallarMision :: Mision -> Equipo -> Equipo
fallarMision unaMision = sufrenVerguenza 2 . quedanLosMasAptos unaMision 

quedanLosMasAptos :: Mision -> Equipo -> Equipo
quedanLosMasAptos (Mision _ rangoRecomendado _ _)  = filter ((>= rangoRecomendado) . rango)

sufrenVerguenza :: Int -> Equipo -> Equipo
sufrenVerguenza unRango = map (reducirRango unRango)

reducirRango :: Int -> Ninja -> Ninja
reducirRango unRango = mapRango (subtract unRango)

-- e)

cumplirMision :: Mision -> Equipo -> Equipo
cumplirMision unaMision = promocionarNinjas . gananRecompensa (recompensa unaMision)

promocionarNinjas :: Equipo -> Equipo
promocionarNinjas = map (aumentaRango 1)

aumentaRango :: Int -> Ninja -> Ninja
aumentaRango unRango = mapRango (+ unRango)

gananRecompensa :: Herramienta -> Equipo -> Equipo
gananRecompensa unaHerramienta = map (agregarHerramienta unaHerramienta)

-- f)

type Jutsu = Mision -> Mision

clonesDeSombra :: Int -> Jutsu
clonesDeSombra clones = mapNinjasNecesarios (subtract clones)

mapNinjasNecesarios :: (Int -> Int) -> Mision -> Mision
mapNinjasNecesarios f unaMision = unaMision {cantidadDeNinjas = max 1 . f $ cantidadDeNinjas unaMision}

-- g)

fuerzaDeUnCentenar :: Jutsu
fuerzaDeUnCentenar = mapEnemigos enemigosFlojos

enemigosFlojos :: [Ninja] -> [Ninja]
enemigosFlojos = filter ((< 500) . rango)

mapEnemigos :: ([Ninja] -> [Ninja]) -> Mision -> Mision
mapEnemigos f unaMision = unaMision {ninjasEnemigos = f $ ninjasEnemigos unaMision}


ejecutarMision :: Mision -> Equipo -> Equipo
ejecutarMision unaMision unEquipo = hacerMision unEquipo . aplicarJutsus unaMision $ unEquipo

aplicarJutsus :: Mision -> Equipo -> Mision
aplicarJutsus unaMision unEquipo = foldl (\mision jutsu -> jutsu mision) unaMision (jutsusEquipo unEquipo)


jutsusEquipo :: Equipo -> [Jutsu]
jutsusEquipo unEquipo = map jutsus unEquipo

hacerMision :: Equipo -> Mision -> Equipo
hacerMision unEquipo unaMision 
  | esCopada unaMision || esFactible unaMision unEquipo = cumplirMision unaMision unEquipo
  | otherwise                                           = fallarMision unaMision unEquipo