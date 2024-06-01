-----------
--Punto 1--
-----------

data Pais = Pais {
  ingresoPerCapita :: Float,
  sectorPublico :: Float,
  sectorPrivado :: Float,
  recursosNaturales :: [Recurso],
  deuda :: Float
} deriving (Show)

type Recurso = String

-- <- Mapeos ->

mapDeuda :: (Float -> Float) -> Pais -> Pais
mapDeuda f unPais = unPais {deuda = f $ deuda unPais}

mapPublico :: (Float -> Float) -> Pais -> Pais
mapPublico f unPais = unPais {sectorPublico = f $ sectorPublico unPais}

mapIngresoPerCapita :: (Float -> Float) -> Pais -> Pais
mapIngresoPerCapita f unPais = unPais {ingresoPerCapita = f $ ingresoPerCapita unPais}

mapRecurso :: ([Recurso] -> [Recurso]) -> Pais -> Pais
mapRecurso f unPais = unPais {recursosNaturales = f $ recursosNaturales unPais}

namibida :: Pais
namibida = Pais {
    ingresoPerCapita = 4140,
    sectorPublico = 400000,
    sectorPrivado = 650000,
    recursosNaturales = ["Mineria", "Ecoturismo"],
    deuda = 50000000
}

-----------
--Punto 2--
-----------

type Estrategia = Pais -> Pais

prestarMillones :: Float -> Estrategia
prestarMillones cantidadMillones = aumentarDeuda (1.5 * cantidadMillones)

aumentarDeuda :: Float -> Pais -> Pais
aumentarDeuda unaDeuda = mapDeuda (unaDeuda +)

reducirPuestosPublicos :: Float -> Estrategia
reducirPuestosPublicos cantidadPuestos = reducirIngresoPerCapita cantidadPuestos . echarPublicos cantidadPuestos

echarPublicos :: Float -> Pais -> Pais
echarPublicos cantidad = mapPublico (subtract cantidad)

reducirIngresoPerCapita :: Float -> Pais -> Pais
reducirIngresoPerCapita cantidadPuestos unPais 
  | cantidadPuestos > 100 = mapIngresoPerCapita (subtract (0.2 * ingresoPerCapita unPais)) unPais
  | otherwise             = mapIngresoPerCapita (subtract (0.15 * ingresoPerCapita unPais)) unPais


entregarEmpresa :: Recurso -> Estrategia
entregarEmpresa unRecurso = reducirDeuda 2000000 . quitarRecurso unRecurso

reducirDeuda :: Float -> Pais -> Pais
reducirDeuda unaDeuda = mapDeuda (subtract unaDeuda)

quitarRecurso :: Recurso -> Pais -> Pais
quitarRecurso unRecurso = mapRecurso (filter (/= unRecurso))

establecerBlindaje :: Estrategia
establecerBlindaje unPais = prestarMillones ((calcularPBI unPais) / 2) . reducirPuestosPublicos 500 $ unPais

calcularPBI :: Pais -> Float
calcularPBI (Pais ingresoPerCapita sectorPublico sectorPrivado _ _) = ingresoPerCapita * (sectorPublico + sectorPrivado) 

