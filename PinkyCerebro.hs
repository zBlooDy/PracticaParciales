-----------
--Punto 1--
-----------


data Animal = Animal {
    nombre :: String,
    iq :: Int,
    especie :: String,
    capacidades :: [String]
}

-- <- Mapeos ->

mapIQ :: (Int -> Int) -> Animal -> Animal
mapIQ f unAnimal = unAnimal {iq = f $ iq unAnimal}

mapCapacidad :: ([String] -> [String]) -> Animal -> Animal
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
  | especie unAnimal == "Raton" && iq unAnimal > 100 = mapCapacidad("Hablar" :) unAnimal
  | otherwise                                        = unAnimal

  

