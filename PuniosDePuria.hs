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