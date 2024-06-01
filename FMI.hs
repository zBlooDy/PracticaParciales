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

-----------
--Punto 3--
-----------

type Receta = [Estrategia]

recetaInventada :: Receta
recetaInventada = [prestarMillones (200000000) , entregarEmpresa "Mineria"]

-- b) Se puede aplicar como foldl (\pais estrategia -> estrategia pais) unPais recetaInventada

-----------
--Punto 4--
-----------

cualesZafan :: [Pais] -> [Pais]
cualesZafan = filter tienePetroleo

tienePetroleo :: Pais -> Bool
tienePetroleo = elem "Petroleo" . recursosNaturales

-- Aparece el concepto de orden superior ya que filter recibe como parametro una funcion. Esa funcion tiene composicion

totalDeuda :: [Pais] -> Float
totalDeuda = sum . map deuda 

-- Aparece el concepto de composicion de funciones, hace el codigo mucho mas expresivo

-----------
--Punto 5--
-----------

recetasOrdenadas :: Pais -> [Receta] -> Bool
recetasOrdenadas _ [receta] = True
recetasOrdenadas unPais (receta1:receta2:recetas) = (calcularPBI $ aplicarReceta unPais receta1) <= (calcularPBI $ aplicarReceta unPais receta2) && recetasOrdenadas unPais (receta2:recetas)


aplicarReceta :: Pais -> Receta -> Pais
aplicarReceta = foldl (\pais receta -> receta pais)