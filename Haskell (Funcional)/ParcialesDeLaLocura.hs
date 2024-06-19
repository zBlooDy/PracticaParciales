data Investigador = Investigador {
    nombre :: String,
    cordura :: Int,
    items :: [Item],
    sucesosEvitados :: [String]
} deriving (Show, Eq)

data Item = Item {
    nombreItem :: String,
    valor :: Int
} deriving (Show, Eq)

maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b
  | f a > f b = a
  | otherwise = b

deltaSegun ponderacion transformacion valor =  abs ((ponderacion . transformacion) valor - ponderacion valor)

-- <- Mapeos ->
mapCordura :: (Int -> Int) -> Investigador -> Investigador
mapCordura f unInvestigador = unInvestigador {cordura = max 0 . f $ cordura unInvestigador}

mapItem :: ([Item] -> [Item]) -> Investigador -> Investigador
mapItem f unInvestigador = unInvestigador {items = f $ items unInvestigador}

mapSucesosEvitado :: ([String] -> [String]) -> Investigador -> Investigador
mapSucesosEvitado f unInvestigador = unInvestigador {sucesosEvitados = f $ sucesosEvitados unInvestigador}

-----------
--Punto 1--
-----------

enloquezca :: Int -> Investigador -> Investigador
enloquezca cantidadPuntos = mapCordura (subtract cantidadPuntos)

hallarItem :: Item -> Investigador -> Investigador
hallarItem unItem = enloquezca (valor unItem) . agregarItem unItem

agregarItem :: Item -> Investigador -> Investigador
agregarItem unItem = mapItem (unItem :)


-----------
--Punto 2--
-----------

algunoTiene :: String -> [Investigador] -> Bool
algunoTiene nombreDeItem  = any (tieneItem nombreDeItem)

tieneItem :: String -> Investigador -> Bool
tieneItem nombreDeItem = any ((== nombreDeItem) . nombreItem) . items

-----------
--Punto 3--
-----------

liderDeUnGrupo :: [Investigador] -> Investigador
liderDeUnGrupo = maximoSegun potencial

potencial :: Investigador -> Int
potencial unInvestigador
  | not . estaLoco $ unInvestigador = (cordura unInvestigador) * (experiencia unInvestigador) + (valor . maximoItem $ unInvestigador)  
  | otherwise                       = 0
    where 
        maximoItem unInvestigador = maximoSegun valor (items unInvestigador)

estaLoco :: Investigador -> Bool
estaLoco = (== 0) . cordura

experiencia :: Investigador -> Int
experiencia unInvestigador = 1 + ((*3) . length . sucesosEvitados $ unInvestigador)

-----------
--Punto 4--
-----------

-- a)
corduraTotal :: Int -> [Investigador] -> Int
corduraTotal cantidadPuntos = sum . map (deltaSegun cordura (enloquezca cantidadPuntos))

-- b)
deltaPotencial :: [Investigador] -> Int
deltaPotencial = head.map (deltaSegun potencial enloquecerPorCompleto)

enloquecerPorCompleto :: Investigador -> Investigador
enloquecerPorCompleto unInvestigador = enloquezca (cordura unInvestigador) unInvestigador

--c)
-- En el caso de la funcion de corduraTotal, para una lista infinita de investigadores no arrojaria un resultado, se quedaria procesando y sumando todos esos valores infinitos
-- En el caso de deltaPotencial, por la evaluacion diferida de haskell, no le interesa toda la lista sino el primer elemento, asi que podria devolver un resultado.

-----------
--Punto 5--
-----------

data Suceso = Suceso {
    descripcion :: String,
    formaDeEvitar :: EvitarSuceso,
    consecuencias :: [Consecuencia]
}

type EvitarSuceso = [Investigador] -> Bool

type Consecuencia = [Investigador] -> [Investigador]

despertarAntiguo :: Suceso
despertarAntiguo = Suceso {
    descripcion = "Despertar de un Antiguo",
    formaDeEvitar = any (tieneItem "Necronomicon"),
    consecuencias = [todosEnloquecen 10, drop 1]
}

todosEnloquecen :: Int -> [Investigador] -> [Investigador]
todosEnloquecen cantidadPuntos = map (enloquezca cantidadPuntos)

ritualInnsmotuh :: Suceso
ritualInnsmotuh = Suceso {
    descripcion = "Ritual en Innsmouth",
    formaDeEvitar = (> 100) . potencial . liderDeUnGrupo,
    consecuencias = [aplicarAlPrimero (hallarItem dagaMaldita), todosEnloquecen 2, enfrentar despertarAntiguo]
}

aplicarAlPrimero :: (Investigador -> Investigador) -> [Investigador] -> [Investigador]
aplicarAlPrimero f (x:xs) = f x : xs

dagaMaldita :: Item
dagaMaldita = Item "Daga Maldita" 3

-----------
--Punto 6--
-----------

enfrentar :: Suceso -> [Investigador] -> [Investigador]
enfrentar unSuceso = cumplenLoNecesario unSuceso . todosEnloquecen 1

cumplenLoNecesario :: Suceso -> [Investigador] -> [Investigador]
cumplenLoNecesario unSuceso grupoInvestigadores 
  | puedenEvitar unSuceso grupoInvestigadores = incorporanSucesoEvitado unSuceso grupoInvestigadores
  | otherwise                                 = sufrenConsecuenciasDe unSuceso grupoInvestigadores
  where
    puedenEvitar = formaDeEvitar


incorporanSucesoEvitado :: Suceso -> [Investigador] -> [Investigador]
incorporanSucesoEvitado unSuceso = map (agregarSucesoEvitado $ descripcion unSuceso)

agregarSucesoEvitado :: String -> Investigador -> Investigador
agregarSucesoEvitado unaDescripcion = mapSucesosEvitado (unaDescripcion :)

sufrenConsecuenciasDe :: Suceso -> [Investigador] -> [Investigador]
sufrenConsecuenciasDe unSuceso grupoInvestigadores = foldl (\investigadores consecuencia -> consecuencia investigadores) grupoInvestigadores (consecuencias unSuceso)

-----------
--Punto 7--
-----------

