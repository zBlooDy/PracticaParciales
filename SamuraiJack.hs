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
