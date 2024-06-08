data Ninja = Ninja {
    nombre :: String,
    herramientas :: [Herramienta],
    jutsus :: Int,
    rango :: Int
}

type Herramienta = (String, Int)

nombreHerramienta :: Herramienta -> String
nombreHerramienta = fst

cantidadDisponible :: Herramienta -> Int
cantidadDisponible = snd

mapHerramienta :: ([Herramienta] -> [Herramienta]) -> Ninja -> Ninja
mapHerramienta f unNinja = unNinja {herramientas = f $ herramientas unNinja}

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