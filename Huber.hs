-----------
--Punto 1--
-----------

data Chofer = Chofer {
    nombre :: String,
    kilometraje :: Int,
    viajes :: [Viaje],
    condicion :: Condicion
}

type Condicion = Viaje -> Bool

data Viaje = Viaje {
    fecha :: Fecha,
    cliente :: Cliente,
    costo :: Int
}

type Fecha = (Int, Int, Int)

data Cliente = Cliente {
    nombreCliente :: String,
    localidad :: String
}

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