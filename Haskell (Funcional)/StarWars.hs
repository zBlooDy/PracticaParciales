data Nave = Nave {
    nombre :: String,
    durabilidad :: Int,
    escudo :: Int,
    ataque :: Int,
    poder :: [Poder]
}

type Poder = Nave -> Nave

-----------
-- Mapeos--
-----------

mapAtaque :: (Int -> Int) -> Nave -> Nave
mapAtaque f unaNave = unaNave {ataque = max 0 . f $ ataque unaNave}


mapEscudo :: (Int -> Int) -> Nave -> Nave
mapEscudo f unaNave = unaNave {escudo = max 0 . f $ escudo unaNave}

mapDurabilidad :: (Int -> Int) -> Nave -> Nave
mapDurabilidad f unaNave = unaNave {durabilidad = max 0 . f $ durabilidad unaNave}
-----------
--Punto 1--
-----------

tieFighter :: Nave
tieFighter = Nave {
    nombre = "TIE Fighter",
    durabilidad = 200,
    escudo = 100,
    ataque = 50,
    poder = [movimientoTurbo]
}

movimientoTurbo :: Poder
movimientoTurbo = incrementaAtaque 25

incrementaAtaque :: Int -> Nave -> Nave
incrementaAtaque unAtaque = mapAtaque (unAtaque +)

reduceAtaque :: Int -> Nave -> Nave
reduceAtaque unAtaque = mapAtaque (subtract unAtaque)

xWing :: Nave
xWing = Nave {
    nombre = "X Wing",
    durabilidad = 300,
    escudo = 150,
    ataque = 100,
    poder = [reparacionEmergencia]
}

reparacionEmergencia :: Poder
reparacionEmergencia = incrementaDurabilidad 50 . reduceAtaque 30

incrementaDurabilidad :: Int -> Nave -> Nave
incrementaDurabilidad unaDurabilidad = mapDurabilidad (unaDurabilidad +)

reduceDurabilidad :: Int -> Nave -> Nave
reduceDurabilidad unaDurabilidad = mapDurabilidad (subtract unaDurabilidad)


naveDarthVader :: Nave
naveDarthVader = Nave {
    nombre = "Nave de Darth Vader",
    durabilidad = 500,
    escudo = 300,
    ataque = 200,
    poder = [movimientoSuperTurbo]
}

movimientoSuperTurbo :: Poder
movimientoSuperTurbo = incrementaAtaque 75 . reduceDurabilidad 45

milleniumFalcon :: Nave
milleniumFalcon = Nave {
    nombre = "Millennium Falcon",
    durabilidad = 1000,
    escudo = 500,
    ataque = 50,
    poder = [reparacionEmergencia, incrementaEscudos 100]
}

incrementaEscudos :: Int -> Poder
incrementaEscudos unEscudo = mapEscudo (unEscudo +)


-----------
--Punto 2--
-----------

durabilidadFlota :: [Nave] -> Int
durabilidadFlota = sum . map durabilidad

-----------
--Punto 3--
-----------

sufreUnAtaque :: Nave -> Nave -> Nave
sufreUnAtaque oponente atacada  = ataqueEntreNaves (aplicaPoderes atacada) (aplicaPoderes oponente)


aplicaPoderes :: Nave -> Nave
aplicaPoderes unaNave = foldl (\nave poder -> poder nave) unaNave (poder unaNave)

ataqueEntreNaves :: Nave -> Nave -> Nave
ataqueEntreNaves atacada oponente 
 | escudo atacada > ataque oponente = atacada
 | otherwise                        = reduceDurabilidad (danioRecibido atacada oponente) atacada



danioRecibido :: Nave -> Nave -> Int
danioRecibido naveAtacada naveOponente = ataque naveOponente - escudo naveAtacada

-----------
--Punto 4--
-----------

estaFueraDeCombate :: Nave -> Bool
estaFueraDeCombate = (== 0) . durabilidad 

-----------
--Punto 5--
-----------

type Estrategia = Nave -> Bool

esNaveDebil :: Estrategia
esNaveDebil = (< 200) . escudo

esNaveConPeligrosidad :: Int -> Estrategia
esNaveConPeligrosidad unValor = (> unValor) . ataque

esNaveQueQuedariaFuera :: Nave -> Estrategia
esNaveQueQuedariaFuera naveAtacante = estaFueraDeCombate . sufreUnAtaque naveAtacante


misionSorpresa :: Nave -> Estrategia -> [Nave] -> [Nave]
misionSorpresa unaNave estrategia = map (atacarSegunEstrategia unaNave estrategia) 


atacarSegunEstrategia :: Nave -> Estrategia -> Nave -> Nave
atacarSegunEstrategia atacante estrategia naveDeLaFlota 
  | estrategia naveDeLaFlota = sufreUnAtaque naveDeLaFlota atacante
  | otherwise                = naveDeLaFlota

-----------
--Punto 6--
-----------


cualEstrategiaConviene :: Nave -> [Nave] -> Estrategia -> Estrategia -> [Nave]
cualEstrategiaConviene unaNave flotaEnemiga unaEstrategia otraEstragia = flotaConMenorDebilidadTrasMision (misionSorpresa unaNave unaEstrategia flotaEnemiga) (misionSorpresa unaNave otraEstragia flotaEnemiga)


flotaConMenorDebilidadTrasMision :: [Nave] -> [Nave] -> [Nave]
flotaConMenorDebilidadTrasMision flota1 flota2
  | durabilidadFlota flota1 > durabilidadFlota flota2 = flota2
  | otherwise                                         = flota1 


-----------
--Punto 7--
-----------

flotaInfinita :: Nave -> [Nave]
flotaInfinita = repeat

--  No se podria obtener la durabilidad, ya que, se necesita sumar TODAS las durabilidades y como la lista es infinita nunca termina

-- Cuando se aplica una mision se va a mostrar la lista infinita de la flota, con las naves que sufrieron ataques y las que no