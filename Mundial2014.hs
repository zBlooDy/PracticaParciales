data Jugador = Jugador {
    nombre :: String,
    edad :: Int,
    promedioDeGol :: Float,
    habilidad :: Int,
    cansancio :: Float
} deriving (Show)

data Equipo = Equipo {
    nombreEquipo :: String,
    grupo :: Char,
    jugadores :: [Jugador]
}deriving (Show)


quickSort _ [] = [] 
quickSort criterio (x:xs) = (quickSort criterio . filter (not . criterio x)) xs ++ [x] ++  (quickSort criterio . filter (criterio x)) xs 

-----------
--Punto 1--
-----------


figurasDeEquipo :: Equipo -> [Jugador]
figurasDeEquipo = filter esFigura . jugadores

esFigura :: Jugador -> Bool
esFigura unJugador = habilidad unJugador > 75 && promedioDeGol unJugador > 0 

-----------
--Punto 2--
-----------

tieneFarandulero :: Equipo -> Bool
tieneFarandulero = any esFarandulero . jugadores


esFarandulero :: Jugador -> Bool
esFarandulero unJugador = elem (nombre unJugador) jugadoresFaranduleros

jugadoresFaranduleros :: [String]
jugadoresFaranduleros = ["Maxi Lopez", "Icardi", "Aguero", "Caniggia", "Demichelis"]

-----------
--Punto 3--
-----------

figuritasDificiles :: [Equipo] -> Char -> [[Jugador]]
figuritasDificiles equipos unGrupo = map jugadoresDificiles (equiposSegunGrupo unGrupo equipos)

equiposSegunGrupo :: Char -> [Equipo] -> [Equipo]
equiposSegunGrupo unGrupo = filter ((== unGrupo) . grupo)

jugadoresDificiles :: Equipo -> [Jugador]
jugadoresDificiles = filter esDificil . jugadores

esDificil :: Jugador -> Bool
esDificil unJugador = esFigura unJugador && esJoven unJugador && (not.esFarandulero $ unJugador)

esJoven :: Jugador -> Bool
esJoven = (< 27) . edad

-----------
--Punto 4--
-----------

jugaronUnMundial :: Equipo -> Equipo
jugaronUnMundial = mapJugadores (map jugoPartido)

mapJugadores :: ([Jugador] -> [Jugador]) -> Equipo -> Equipo
mapJugadores f unEquipo = unEquipo {jugadores = f $ jugadores unEquipo}

jugoPartido :: Jugador -> Jugador
jugoPartido unJugador 
    | esDificil unJugador = mapCansancio (const 50) unJugador
    | esJoven unJugador   = aumentarCansancio (0.1 * cansancioJugador) unJugador
    | esFigura unJugador  = aumentarCansancio 20 unJugador
    | otherwise           = aumentarCansancio (2 * cansancioJugador) unJugador
    where
        cansancioJugador = cansancio unJugador 


aumentarCansancio :: Float -> Jugador -> Jugador
aumentarCansancio unCansancio = mapCansancio (unCansancio +)


mapCansancio :: (Float -> Float) -> Jugador -> Jugador
mapCansancio f unJugador = unJugador {cansancio = f $ cansancio unJugador}


-----------
--Punto 5--
-----------

empezoElMundial :: Equipo -> Equipo -> Equipo
empezoElMundial unEquipo otroEquipo 
  | promedioGol unEquipo > promedioGol otroEquipo = jugaronUnMundial unEquipo
  | otherwise                                     = jugaronUnMundial otroEquipo


promedioGol :: Equipo -> Float
promedioGol = sum . map promedioDeGol . jugadoresMenosCansados

jugadoresMenosCansados :: Equipo -> [Jugador]
jugadoresMenosCansados unEquipo = take 11 (quickSort menosCansado (jugadores unEquipo))

menosCansado :: Jugador -> Jugador -> Bool
menosCansado unJugador otroJugador = cansancio unJugador < cansancio otroJugador