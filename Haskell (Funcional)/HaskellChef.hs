data Participante = Participante {
    nombre :: String,
    trucos :: [Truco],
    platoEspecial :: Plato
}

data Plato = Plato {
    dificultad :: Int,
    componentes :: [Componente]
}

type Truco = Plato -> Plato
type Componente = (Ingrediente, Peso)
type Ingrediente = String
type Peso = Float



-- <-Mapeos ->

ingredienteComponente :: Componente -> Ingrediente
ingredienteComponente unComponente = fst unComponente

pesoComponente :: Componente -> Peso
pesoComponente unComponente = snd unComponente

mapComponente :: ([Componente] -> [Componente]) -> Plato -> Plato
mapComponente f unPlato = unPlato {componentes = f $ componentes unPlato}

-----------
--Parte A--
-----------

-- <- Trucos ->
endulzar :: Float -> Truco
endulzar gramos = agregarComponente ("Azucar", gramos)

agregarComponente :: Componente -> Plato -> Plato
agregarComponente unComponente = mapComponente (unComponente :)

salar :: Float -> Truco
salar gramos = agregarComponente ("Sal", gramos)

darSabor :: Float -> Float -> Truco
darSabor gramosSal gramosAzucar = salar gramosSal . endulzar gramosAzucar

duplicarPorcion :: Truco
duplicarPorcion = mapComponente (map duplicarComponente)

duplicarComponente :: Componente -> Componente
duplicarComponente (ingrediente , peso) = (ingrediente , 2 * peso)

simplificar :: Truco
simplificar unPlato 
  | esComplejo unPlato = simplificarPlato unPlato
  | otherwise         = unPlato

esComplejo :: Plato -> Bool
esComplejo unPlato = (dificultad unPlato > 7) && ((> 5) . length . componentes $ unPlato) 

simplificarPlato :: Plato -> Plato
simplificarPlato = cambiarDificultad 5 . mapComponente (filter ((>10) . pesoComponente))

cambiarDificultad :: Int -> Plato -> Plato
cambiarDificultad nuevaDificultad unPlato = unPlato {dificultad = nuevaDificultad}

-- <- Tipos de platos ->

esVegano :: Plato -> Bool
esVegano = not . any (productosVeganos) . componentes

productosVeganos :: Componente -> Bool
productosVeganos unComponente = elem (ingredienteComponente unComponente) ["Huevo" , "Carne" , "Leche" , "Yoghurt"]

esSinTacc :: Plato -> Bool
esSinTacc  = not . tieneIngrediente "Harina"
    
tieneIngrediente :: Ingrediente -> Plato -> Bool
tieneIngrediente ingrediente unPlato = elem ingrediente (ingredientesDe unPlato)

ingredientesDe :: Plato -> [Ingrediente]
ingredientesDe = map ingredienteComponente . componentes


noAptoHipertension :: Plato -> Bool
noAptoHipertension unPlato = tieneIngrediente "Sal" unPlato && esMayorCantidadDe "Sal" unPlato 2 

esMayorCantidadDe :: Ingrediente -> Plato -> Float -> Bool
esMayorCantidadDe unIngrediente unPlato unValor = any (serIngredienteMayor unIngrediente unValor) (componentes unPlato)
 
serIngredienteMayor :: Ingrediente -> Float -> Componente -> Bool
serIngredienteMayor unIngrediente unValor (ingrediente, peso) = (unIngrediente == ingrediente) && (peso > unValor)


-----------
--Parte B--
-----------

pepeRonccino :: Participante
pepeRonccino = Participante {
    nombre = "Pepe Ronccino",
    trucos = [darSabor 2 5, simplificar, duplicarPorcion],
    platoEspecial = platoDePepe
}

platoDePepe :: Plato
platoDePepe = Plato {
    dificultad = 10,
    componentes = repeat ("Sal", 10)
}

-----------
--Parte C--
-----------

cocinar :: Participante -> Plato -> Plato
cocinar unParticipante unPlato = foldl (\plato truco -> truco plato) unPlato (trucos unParticipante)

esMejorQue :: Plato -> Plato -> Bool
esMejorQue unPlato otroPlato = (dificultad unPlato > dificultad otroPlato) && (sumaPesos unPlato < sumaPesos otroPlato)

sumaPesos :: Plato -> Float
sumaPesos = sum . map pesoComponente . componentes

participanteEstrella :: [Participante] -> Participante
participanteEstrella  = foldl1 (estrellaEntre)

estrellaEntre :: Participante -> Participante -> Participante
estrellaEntre unParticipante otroParcipante 
  | esMejorQue (terminaCocinar unParticipante) (terminaCocinar otroParcipante) = unParticipante
  | otherwise                                                                = otroParcipante

terminaCocinar :: Participante -> Plato
terminaCocinar unParticipante = cocinar unParticipante (platoEspecial unParticipante)

-----------
--Parte D--
-----------

platinum :: Plato
platinum = Plato {
    dificultad = 10,
    componentes = componentesInfinitos
}

componentesInfinitos :: [Componente]
componentesInfinitos = zip (map (ingredientesInfinitos) [1..]) (iterate (+1) 1)

ingredientesInfinitos :: Float -> Ingrediente
ingredientesInfinitos unValor = "Ingrediente " ++ show unValor 

-- Se pueden aplicar todos los trucos, salvo esComplejo ya que necesitas la longitud de la lista, la cual es infinita y nunca puede determinar esa longitud
-- En caso de que no cumpla la dificultad, por 
-- No, no se puede saber si es mejor porque sumaPesos utiliza sum, que necesita recorrer toda la lista