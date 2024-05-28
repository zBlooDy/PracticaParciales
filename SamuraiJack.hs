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

meditar :: Float -> Personaje -> Personaje
meditar unValor  = mapSalud (+ unValor/2)

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

