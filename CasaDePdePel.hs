data Ladron = Ladron {
    nombre      :: String,
    habilidades :: [Habilidad],
    armas       :: [Arma]
}

data Rehen = Rehen {
    nombreRehen  :: String,
    nivelComplot :: Int,
    nivelMiedo   :: Int,
    plan         :: Plan
}

type Habilidad = String

type Plan = Rehen -> Ladron -> Ladron

type Arma = Int -> Rehen -> Rehen

-- <- Mapeos ->

mapComplot :: (Int -> Int) -> Rehen -> Rehen
mapComplot f unRehen = unRehen {nivelComplot = f $ nivelComplot unRehen}

mapMiedo :: (Int -> Int) -> Rehen -> Rehen
mapMiedo f unRehen = unRehen {nivelMiedo = f $ nivelMiedo unRehen}

mapArmas :: ([Arma] -> [Arma]) -> Rehen -> Rehen
mapArmas f unRehen = unRehen {armas = f $ armas unRehen}


aumentarMiedo :: Int -> Rehen -> Rehen
aumentarMiedo cantidad = mapMiedo (cantidad +)

aumentarComplot :: Int -> Rehen -> Rehen
aumentarComplot cantidad = mapComplot (cantidad +)

reducirComplot :: Int -> Rehen -> Rehen
reducirComplot cantidad = mapMiedo (subtract cantidad)

-------------
-- Punto 1 --
-------------

tokio :: Ladron
tokio = Ladron "Tokio" ["trabajo psicologico" , "entrar en moto"] [pistola 9, pistola 9, ametralladora 30]

profesor :: Ladron
profesor = Ladron "Profesor" ["disfrazarse de linyera" , "disfrazarse de payaso" , "estar siempre un paso adelante"] []

pablo :: Rehen
pablo = Rehen "Pablo" 40 30 esconderse

arturito :: Rehen
arturito = Rehen "Arturito" 70 50 (atacarLadron pablo . esconderse)

pistola :: Arma
pistola calibre unRehen = aumentarMiedo (3 * (length . nombreRehen $ unRehen)) . reducirComplot (5 * calibre) $ unRehen

ametralladora ::  Arma
ametralladora balas = aumentarMiedo (balas) . mapComplot (flip div 2)

-------------
-- Punto 2 --
-------------

esInteligente :: Ladron -> Bool
esInteligente unLadron = (esLadron unLadron profesor) || ((>2) . length . habilidades $ unLadron)

esLadron :: Ladron -> Ladron -> Bool
esLadron unLadron otroLadron = nombre unLadron == nombre otroLadron

-------------
-- Punto 3 --
-------------

conseguirArma :: Arma -> Ladron -> Ladron
conseguirArma unArma = mapArmas (unArma :)

-------------
-- Punto 4 --
-------------

disparos :: Ladron -> Rehen -> Rehen
disparos unLadron unRehen = armaMasIntimidadora unRehen (armas unLadron) $ unRehen

armaMasIntimidadora :: Rehen -> [Arma] -> Arma
armaMasIntimidadora unRehen  = maximoSegun (\arma -> nivelMiedo (arma unRehen))

maximoSegun :: (Rehen -> Int) -> [Arma] -> Arma
maximoSegun f = foldl1 (maximo f)

maximo :: (Rehen -> Int) -> Arma -> Arma -> Arma
maximo f unArma otroArma
  | f unArma > f otroArma = unArma
  | otherwise             = otroArma

hacerseElMalo :: Ladron -> Rehen -> Rehen
hacerseElMalo unLadron unRehen
  | esLadron unLadron berlin = aumentarMiedo (cantidadLetrasDeHabilidades) unRehen
  | esLadron unLadron rio    = aumentarComplot 20 unRehen
  | otherwise                = aumentarMiedo 10 unRehen
  where
    cantidadLetrasDeHabilidades = sum . map length . habilidades $ unLadron

-------------
-- Punto 5 --
-------------

calmarLasAguas :: Ladron -> [Rehen] -> [Rehen]
calmarLasAguas unLadron = filter ((> 60) . nivelComplot) . map disparos unLadron

-------------
-- Punto 6 --
-------------

puedeEscapar :: Ladron -> Bool
puedeEscapar = any esDisfrazarse . habilidades

esDisfrazarse :: Habilidad -> Bool
esDisfrazarse unaHabilidad = take 14 unaHabilidad == "disfrazarse de"

-------------
-- Punto 7 --
-------------

laCosaPintaMal :: [Ladron] -> [Rehen] -> Bool
laCosaPintaMal unosLadrones unosRehenes = nivelPromComplot unosRehenes > (nivelPromMiedo unosRehenes * (cantidadTotalArmas unosLadrones))

nivelPromComplot :: [Rehen] -> Int
nivelPromComplot = promedioSegun nivelComplot

nivelPromMiedo :: [Rehen] -> Int
nivelPromMiedo = promedioSegun nivelMiedo

promedioSegun :: (Rehen -> Int) -> [Rehen] -> Int
promedioSegun f xs = sum (map f xs) div length xs

cantidadTotalArmas :: [Ladron] -> Int
cantidadTotalArmas = sum . map (length . armas) 

-------------
-- Punto 8 --
-------------

rebelarse :: [Rehen] -> Ladron -> Ladron
rebelarse grupoRehenes unLadron = foldl (\ladron rehen = (plan rehen) $ ladron) unLadron grupoRehenes

atacarLadron :: Plan
atacarLadron rehenInvolucrado = perderArmas (div (length . nombre $ rehenInvolucrado) 10)

perderArmas :: Int -> Ladron -> Ladron
perderArmas cantidad = mapArmas (drop cantidad)

esconderse :: Plan
esconderse rehenInvolucrado unLadron = perderArmas (div (length . habilidades $ unLadron) 3)

-------------
-- Punto 9 --
-------------

planValencia :: [Rehen] -> [Ladron] -> Int
planValencia grupoRehenes = (* 1000000) . cantidadTotalArmas . map (rebelarse grupoRehenes . conseguirArma (ametralladora 45))

--------------
-- Punto 10 --
--------------

-- No, ya que no se podria sacar el valor de la cantidad total de armas, porque esta funcion utiliza un sum y la lista es infinita

--------------
-- Punto 11 --
--------------

-- En el caso de que el plan sea esconderse no se puede, sino si.