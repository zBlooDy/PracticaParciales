-----------
--Parte 1--
-----------

data Tesoro = Tesoro {
    anioDescubrimiento :: Int,
    precio :: Int
}

-- A)

deLujo :: Tesoro -> Bool
deLujo unTesoro = (precio unTesoro > 1000) || (antiguedad unTesoro > 200)

antiguedad :: Tesoro -> Int
antiguedad unTesoro = 2024 - (anioDescubrimiento unTesoro)

telaSucia :: Tesoro -> Bool
telaSucia unTesoro = precio unTesoro < 50 && (not . deLujo $ unTesoro)

estandar :: Tesoro -> Bool
estandar unTesoro = (not . deLujo $ unTesoro) && (not . telaSucia $ unTesoro)

-- B)

valorTesoro :: Tesoro -> Int
valorTesoro unTesoro = precio unTesoro + (2 * antiguedad unTesoro)

