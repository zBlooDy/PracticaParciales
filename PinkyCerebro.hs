-----------
--Punto 1--
-----------


data Animal = Animal {
    iq :: Int,
    especie :: String,
    capacidades :: [Habilidad]
}

type Habilidad = String
-- <- Mapeos ->

mapIQ :: (Int -> Int) -> Animal -> Animal
mapIQ f unAnimal = unAnimal {iq = f $ iq unAnimal}

mapCapacidad :: ([Habilidad] -> [Habilidad]) -> Animal -> Animal
mapCapacidad f unAnimal = unAnimal {capacidades = f $ capacidades unAnimal}

-----------
--Punto 2--
-----------

type Transformacion = Animal -> Animal

inteligenciaSuperior :: Int -> Transformacion
inteligenciaSuperior aumento = mapIQ (aumento +)

pinkificar :: Transformacion
pinkificar unAnimal = unAnimal {capacidades = []}

superpoderes :: Transformacion
superpoderes unAnimal
  | especie unAnimal == "Elefante"                   = mapCapacidad ("No tenerle miedo a los ratones" :) unAnimal
  | especie unAnimal == "Raton" && iq unAnimal > 100 = mapCapacidad ("Hablar" :) unAnimal
  | otherwise                                        = unAnimal


-----------
--Punto 3--
-----------

type Criterio = Animal -> Bool

antropomorfico :: Criterio
antropomorfico unAnimal = elem "Hablar" (capacidades unAnimal) &&  iq unAnimal > 60

noTanCuerdo :: Criterio
noTanCuerdo = (>2) . length . filter pinkiesco . capacidades

pinkiesco :: Habilidad -> Bool
pinkiesco unaHabilidad = (take 6 unaHabilidad == "hacer ") && esPinkiesca (drop 6 unaHabilidad)


esPinkiesca :: Habilidad -> Bool
esPinkiesca unaHabilidad = length unaHabilidad <= 4 && any esVocal unaHabilidad

esVocal :: Char -> Bool
esVocal letra = letra `elem` "aeiouAEIOU"

-----------
--Punto 4--
-----------

type Experimento = ([Transformacion], Criterio)

experimentoExitoso :: Experimento -> Animal -> Bool
experimentoExitoso unExperimento unAnimal = snd unExperimento $ aplicarTransformaciones unAnimal (fst unExperimento)

aplicarTransformaciones :: Animal -> [Transformacion] -> Animal
aplicarTransformaciones = foldl (\animal transformacion -> transformacion animal)  

papuRaton :: Animal
papuRaton = Animal {
    iq = 17,
    especie = "Raton",
    capacidades =["destruenglonir el mundo", "hacer planes desalmados"]
}

-- Consulta: experimentoExitoso ([pinkificar, inteligenciaSuperior 10, superpoderes], antropomorfico) papuRaton

