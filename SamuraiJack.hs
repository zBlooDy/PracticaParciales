data Elemento = Elemento { 
    tipo :: String,
    ataque :: Personaje -> Personaje,
    defensa :: Personaje -> Personaje 
}

data Personaje = Personaje { 
    nombre :: String,
    salud :: Float,
    elementos :: [Elemento],
    anioPresente :: Int 
}


-----------
--Punto 1--
-----------
mandarAlAnio :: Int -> Personaje -> Personaje
mandarAlAnio unAnio unPersonaje = unPersonaje {anioPresente = unAnio}

meditar ::  Personaje -> Personaje
meditar unPersonaje  = mapSalud (salud unPersonaje /2 +) unPersonaje

mapSalud :: (Float -> Float) -> Personaje -> Personaje
mapSalud f unPersonaje = unPersonaje {salud = f $ salud unPersonaje}


causarDanio :: Float -> Personaje -> Personaje
causarDanio unaSalud = mapSalud (max 0 . subtract unaSalud)

-----------
--Punto 2--
-----------

-- a)
esMalvado :: Personaje -> Bool
esMalvado unPersonaje = any ((== "Malvado") . tipo) $ elementos unPersonaje

-- b)
danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce unPersonaje unElemento = salud unPersonaje  -  salud (ataque unElemento unPersonaje)

-- Aclaracion de consigna: Te pregunta si lo mata con algun elemento

-- c)
enemigosMortales :: Personaje -> [Personaje] -> [Personaje]
enemigosMortales unPersonaje  = filter (loMatanConUnElemento unPersonaje) 

loMatanConUnElemento :: Personaje -> Personaje -> Bool
loMatanConUnElemento unPersonaje unEnemigo = any ((==0). danioQueProduce unPersonaje) $ elementos unEnemigo


-----------
--Punto 3--
-----------
concentracion :: Int -> Elemento
concentracion veces = Elemento {
    tipo = "Magia",
    ataque = id,
    defensa = foldl1 (.) (replicate veces meditar)
}


esbirro :: Elemento 
esbirro = Elemento {
    tipo = "Magia",
    ataque = causarDanio 1,
    defensa = id
}

esbirrosMalvados :: Int -> [Elemento]
esbirrosMalvados cantidad = replicate cantidad esbirro

jack :: Personaje
jack = Personaje {
    nombre = "Jack",
    salud = 300,
    elementos = [concentracion 3, katanaMagica],
    anioPresente = 200
}

katanaMagica :: Elemento
katanaMagica = Elemento {
    tipo = "Magia",
    ataque = causarDanio 1000,
    defensa = id
}

aku :: Int -> Float -> Personaje 
aku unAnio unaSalud = Personaje {
    nombre = "Aku",
    salud = unaSalud,
    elementos = [concentracion 4, portalFuturo unAnio] ++ esbirrosMalvados (100*unAnio),
    anioPresente = unAnio
}

portalFuturo :: Int -> Elemento
portalFuturo anioParaAku = Elemento {
    tipo = "Magia",
    ataque = mandarAlAnio (anioParaAku + 2800),
    defensa = aku (anioParaAku + 2800) . salud 
}

-----------
--Punto 4--
-----------

luchar :: Personaje -> Personaje -> (Personaje, Personaje)
luchar atacante defensor 
   | loMatanConUnElemento atacante defensor = (defensor, atacante)
   | otherwise                              = luchar proximoAtacante proximoDefensor
   where
    proximoAtacante = aplicarElementos ataque defensor (elementos atacante)
    proximoDefensor = aplicarElementos defensa atacante (elementos atacante)


aplicarElementos :: (Elemento -> Personaje -> Personaje) -> Personaje -> [Elemento] -> Personaje
aplicarElementos f unPersonaje elementos = foldl (\x f -> f x) unPersonaje (map f elementos)


-----------
--Punto 5--
-----------

--Inferir el tipo de la siguiente función:
f x y z
    | y 0 == z = map (fst.x z)
    | otherwise = map (snd.x (y 0))

-- f :: (Eq t1, Num t2) =>
--      (t1 -> a1 -> (a2, a2)) -> (t2 -> t1) -> t1 -> [a1] -> [a2]   ?????