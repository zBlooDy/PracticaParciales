import Text.Show.Functions
-----------
--Punto 1--
-----------

data Chofer = Chofer {
    nombre :: String,
    kilometraje :: Int,
    viajes :: [Viaje],
    condicion :: Condicion
} deriving (Show)

type Condicion = Viaje -> Bool

data Viaje = Viaje {
    fecha :: Fecha,
    cliente :: Cliente,
    costo :: Int
} deriving (Show)

type Fecha = (Int, Int, Int)

data Cliente = Cliente {
    nombreCliente :: String,
    localidad :: String
} deriving (Show)

-----------
--Punto 2--
-----------

ninguna :: Condicion
ninguna _ = True

viajesCaros :: Condicion
viajesCaros = (> 200) . costo

clienteConNLetras :: Int -> Condicion
clienteConNLetras cantidad = (> cantidad) . length . nombreCliente . cliente

noViveEn :: String -> Condicion
noViveEn zona = (/= zona) . localidad . cliente

-----------
--Punto 3--
-----------

lucas :: Cliente
lucas = Cliente {
    nombreCliente = "Lucas",
    localidad = "Victoria"
}

daniel :: Chofer
daniel = Chofer {
    nombre = "Daniel",
    kilometraje = 23500,
    viajes = [Viaje (20,04,2017) lucas 150],
    condicion = noViveEn "Olivos"
}

alejandra :: Chofer
alejandra = Chofer {
    nombre = "Alejandra",
    kilometraje = 180000,
    viajes = [],
    condicion = ninguna
}

-----------
--Punto 4--
-----------

puedeTomarViaje :: Viaje -> Chofer -> Bool
puedeTomarViaje unViaje unChofer = condicion unChofer unViaje

-----------
--Punto 5--
-----------

liquidacionChofer :: Chofer -> Int
liquidacionChofer = sum . map costo . viajes

-----------
--Punto 6--
-----------

realizarUnViaje :: Viaje -> [Chofer] -> Chofer
realizarUnViaje unViaje = efectuarViaje unViaje . choferConMenosViajes . choferesQuePuedenRealizar unViaje

choferesQuePuedenRealizar :: Viaje -> [Chofer] -> [Chofer]
choferesQuePuedenRealizar unViaje = filter (puedeTomarViaje unViaje)

choferConMenosViajes :: [Chofer] -> Chofer
choferConMenosViajes = foldl1 menosViajes

menosViajes :: Chofer -> Chofer -> Chofer
menosViajes unChofer otroChofer 
  | cantidadViajes unChofer > cantidadViajes otroChofer = otroChofer
  | otherwise                                             = unChofer
  where
    cantidadViajes = length . viajes

efectuarViaje :: Viaje -> Chofer -> Chofer
efectuarViaje unViaje = mapViajes (unViaje :)

mapViajes :: ([Viaje] -> [Viaje]) -> Chofer -> Chofer
mapViajes f unChofer = unChofer {viajes = f $ viajes unChofer}

-----------
--Punto 7--
-----------

-- A)

nitoInfy :: Chofer
nitoInfy = Chofer {
    nombre = "Nito Infy",
    kilometraje = 70000,
    viajes = repetirViaje (Viaje (11,03,2017) lucas 50),
    condicion = clienteConNLetras 3
}

repetirViaje :: Viaje -> [Viaje]
repetirViaje unViaje = unViaje : repetirViaje unViaje

-- B) No, no se puede calcular la liquidacion del chofer, debido que esta funcion utiliza "sum" la cual necesita recorrer toda la lista, lo cual se quedaria procesando y nunca arrojaria un resultado

-- C) Si, esto ocurre por la evaluacion diferida con la que trabaja Haskell. "No se evalua lo que no hace falta", para tomar el viaje solo necesita chequear que cumpla la condicion, sin intervenir los infinitos viajes que tiene realizados.

-----------
--Punto 8--
-----------
gongNeng :: b -> (b -> Bool ) -> (a -> b) -> [a] -> b
gongNeng arg1 arg2 arg3 = max arg1 . head . filter arg2 . map arg3 