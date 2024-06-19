-----------
--Punto 1--
-----------

-- a)
data Peleador = Peleador {
    puntosVida :: Int,
    resistencia :: Int,
    ataques :: [Ataque]
}

type Ataque = Peleador -> Peleador

-- <- Mapeos ->

mapVida :: (Int -> Int) -> Peleador -> Peleador
mapVida f unPeleador = unPeleador {puntosVida = f $ puntosVida unPeleador}

mapAtaque ::([Ataque] -> [Ataque]) -> Peleador -> Peleador
mapAtaque f unPeleador = unPeleador {ataques = f $ ataques unPeleador}

-- b)

estaMuerto :: Peleador -> Bool
estaMuerto = (< 1) . puntosVida

esHabil :: Peleador -> Bool
esHabil = (> 10) . length . ataques

-- c)

-- i)
golpe :: Int -> Ataque
golpe intensidad unOponente = bajarVida (impactoGolpe) unOponente
    where
        impactoGolpe = intensidad `div` (resistencia unOponente)

bajarVida :: Int -> Peleador -> Peleador
bajarVida unaVida = mapVida (subtract unaVida)

sumarVida :: Int -> Peleador -> Peleador
sumarVida unaVida = mapVida (+ unaVida)

-- ii)
toqueDeLaMuerte :: Ataque
toqueDeLaMuerte = mapVida (const 0)

-- iii)

type Parte = String

patada :: Parte -> Ataque
patada "Pecho" unOponente
  | not . estaMuerto $ unOponente = bajarVida 10 unOponente
  | otherwise                     = sumarVida 1 unOponente
patada "Carita" unOponente = bajarVida (puntosVida unOponente `div` 2) unOponente
patada "Nuca" unOponente   = perderAtaques 1 unOponente 
patada _ unOponente        = unOponente


perderAtaques :: Int -> Peleador -> Peleador
perderAtaques cantidad = mapAtaque (drop cantidad)


-- d)

bruceLee :: Peleador
bruceLee = Peleador 200 25 [toqueDeLaMuerte, golpe 500, ataqueBruce]

ataqueBruce :: Ataque
ataqueBruce = patada "Carita" . patada "Carita" . patada "Carita"


-----------
--Punto 2--
-----------

type Enemigo = Peleador

mejorAtaque :: Peleador -> Enemigo -> Ataque
mejorAtaque unPeleador unEnemigo = foldl1 (mayor unEnemigo) (ataques unPeleador)


mayor :: Enemigo -> Ataque -> Ataque -> Ataque
mayor unEnemigo ataque1 ataque2
  | puntosVida (ataque1 unEnemigo) < puntosVida (ataque2 unEnemigo) = ataque2
  | otherwise                                                       = ataque1

-----------
--Punto 3--
-----------

-- a)
esTerrible :: Ataque -> [Enemigo] -> Bool
esTerrible unAtaque = mataronMuchos . atacarEnemigos unAtaque

atacarEnemigos :: Ataque -> [Enemigo] -> [Enemigo]
atacarEnemigos unAtaque = map (\enemigo -> unAtaque enemigo)

mataronMuchos :: [Enemigo] -> Bool
mataronMuchos enemigos = (length . filter estaMuerto $ enemigos) > (length enemigos `div` 2)

-- b)
esPeligroso :: Peleador -> [Enemigo] -> Bool
esPeligroso unPeleador enemigos = all (flip esTerrible enemigosHabiles) (ataques unPeleador)
    where
        enemigosHabiles = filter esHabil enemigos

-- c)
invencible :: Peleador -> [Enemigo] -> Bool
invencible unPeleador enemigos = (puntosVida (foldl (aplicarMejorAtaque) unPeleador enemigos)) == (puntosVida unPeleador)

aplicarMejorAtaque :: Peleador -> Enemigo -> Peleador
aplicarMejorAtaque unPeleador unEnemigo = (mejorAtaque unPeleador unEnemigo) $ unPeleador