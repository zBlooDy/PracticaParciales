


--Ejercicio 2
--Definir la función mejor/3, que recibe dos funciones y un número, y devuelve el resultado de la función que dé un valor más alto.
--Main> mejor cuadrado triple 1 
-- > 3 
-- (pues triple 1 = 3 > 1 = cuadrado 1) 

cuadrado :: Int -> Int
cuadrado num = num * num

triple :: Int -> Int
triple num = 3 * num

mejor :: (Int -> Int) -> (Int -> Int) -> Int -> Int
mejor f g numero  = max (f numero) (g numero)