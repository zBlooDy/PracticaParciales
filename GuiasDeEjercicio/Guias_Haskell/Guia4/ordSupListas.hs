import Data.List

--Ejercicio 1 
--Definir la función esMultiploDeAlguno/2, que recibe un número y una lista y devuelve True si el número es múltiplo de alguno de los números de la lista. 



esMultiplo :: Int -> Int -> Bool
esMultiplo num1  = (0==) . mod num1

esMultiploDeAlguno :: Int -> [Int] -> Bool
esMultiploDeAlguno numero  = any (esMultiplo numero)


--Ejercicio 2
--Armar una función promedios/1, que dada una lista de listas me devuelve la lista de los promedios de cada lista-elemento. P.ej. 
--Main> promedios [[8,6],[7,9,4],[6,2,4],[9,6]] 
--[7,6.67,4,7.5] 


calculaPromedio :: [Int] -> Float
calculaPromedio lista = (fromIntegral.sum) lista / (fromIntegral.length) lista

promedios :: [[Int]] -> [Float]
promedios listaDeNotas = map calculaPromedio listaDeNotas
--                              (a->b) -> [a] -> [b]

--Ejercicio 3
--Armar una función promediosSinAplazos que dada una lista de listas me devuelve la lista de los promedios de cada lista-elemento, excluyendo los que sean menores a 4 que no se cuentan

promediosSinAplazos :: [[Int]] -> [Float]
promediosSinAplazos listaDeNotas = filter (>=4) $ promedios listaDeNotas
--                                     (a -> Bool) -> [a] -> [a]


--Ejercicio 4
--Definir la función mejoresNotas, que dada la información de un curso devuelve la lista con la mejor nota de cada alumno. Usar maximum

mejoresNotas :: [[Int]] -> [Int]
mejoresNotas listaDeNotas = map maximum listaDeNotas

--Ejercicio 5
--Definir la función aprobó/1, que dada la lista de las notas de un alumno devuelve True si el alumno aprobó. Se dice que un alumno aprobó si todas sus notas son 6 o más

aprobo :: [Int] -> Bool
aprobo listaDeNotas = ((>= 6). minimum) listaDeNotas

--Ejercicio 6
--Definir la función aprobaron/1, que dada la información de un curso devuelve la información de los alumnos que aprobaron

aprobaron :: [[Int]] -> [[Int]]
aprobaron listaDeNotas = filter aprobo listaDeNotas

-- filter usa aprobo, que es [Int] -> Bool
-- filter :: (a -> Bool) -> [a] -> [a]. En mi caso a seria [Int]
-- filter :: ([Int] -> Bool) -> [[Int]] -> [[Int]]
--                 aprobo         lista     retorno
-- Entonces tenemos que el tipo de lista tendría que ser [[Int]], y el tipo que va a devolver tendría que ser también [[Int]]:

--Ejercicio 7
--Definir la función divisores/1, que recibe un número y devuelve la lista de divisores

divisores :: Int -> [Int]
divisores numero = filter (esMultiplo numero) [1 .. numero]

--Ejercicio 8
--Definir la función exists/2, que dadas una función booleana y una lista devuelve True si la función da True para algún elemento de la lista

exists :: (a->Bool) -> [a] -> Bool
exists fbool lista = any fbool lista

--Ejercicio 9
--Definir la función hayAlgunNegativo/2, que dada una lista de números y un (…algo…) devuelve True si hay algún nro. negativo. 
--Main> hayAlgunNegativo [2,-3,9] (…algo…) 
--True 

hayAlgunNegativo :: [Int] -> Bool
hayAlgunNegativo listaDeNumeros  = any (<0) listaDeNumeros

--Ejercicio 10
--Definir la función aplicarFunciones/2, que dadas una lista de funciones y un valor cualquiera, devuelve la lista del resultado de aplicar las funciones al valor.
--Main> aplicarFunciones[(*4),(+3),abs] (-8) 
--[-32,-5,8] 

aplicarFunciones :: [a -> b] -> a -> [b]



aplicarFunciones listaDeFunciones valor = map ($ valor) listaDeFunciones
--                                         La funcion $ evalua en una funcion de la lista que lee, el valor, luego de eso map se encarga de hacer la lista
--Ejercicio 11
--Definir la función sumaF/2, que dadas una lista de funciones y un número, devuelve la suma del resultado de aplicar las funciones al número. P.ej.

sumaF :: Num b => [a -> b] -> a -> b
sumaF listaDeFunciones numero = sum $ aplicarFunciones listaDeFunciones numero

--Ejercicio 12
--Escribir una función subirHabilidad/2 que reciba un número (que se supone positivo sin validar) y una lista de números, y le suba la habilidad a cada jugador cuidando que ninguno se pase de 12. P.ej. 
--Main> subirHabilidad 2 [3,6,9,10,11,12] 
--[5,8,11,12,12,12] 

subirHabilidad :: Int -> [Int] -> [Int]
subirHabilidad numero listaDeHabilidades = map (min 12 . (+)numero) listaDeHabilidades





--Ejercicio 14
--takeWhile toma x elementos de una lista mientras se cumpla una funcion
-- takeWhile :: (a -> Bool) -> [a] -> [a]

--Ejercicio 15
--Usar takeWhile/2 para definir las siguientes funciones: 
--primerosPares/1, que recibe una lista de números y devuelve la sublista hasta el primer no par exclusive

primerosPares :: [Int] -> [Int]
primerosPares listaNumeros = takeWhile even listaNumeros

--primerosDivisores/2, que recibe una lista de números y un número n, y devuelve la sublista hasta el primer número que no es divisor de n exclusive. 

primerosDivisores :: Int -> [Int] -> [Int]
primerosDivisores numero listaNumeros = takeWhile (esMultiplo numero) listaNumeros

--primerosNoDivisores/2, que recibe una lista de números y un número n, y devuelve la sublista hasta el primer número que sí es divisor de n exclusive.

primerosNoDivisores :: Int -> [Int] -> [Int]
primerosNoDivisores numero listaNumeros = takeWhile (not.esMultiplo numero) listaNumeros


--Ejercicio 16
--Definir la función: huboMesMejorDe/3, que dadas las listas de ingresos y egresos y un número, devuelve True si el resultado de algún mes es mayor que el número. P.ej. 
--Main> huboMesMejorDe [1..12] [12,11..1] 10 
--True 
--Porque en diciembre el resultado fue 12-1=11 > 10. Evaluar si el resultado neto (ingreso - egreso) es > numero

huboMesMejorDe :: [Int] -> [Int] -> Int -> Bool
huboMesMejorDe listaIngresos listaEgresos numero = any (> numero) $ zipWith (-) listaIngresos listaEgresos


--Ejercicio 17
--a) Definir la función crecimientoAnual/1,que recibe como parámetro la edad de la persona, y devuelve cuánto tiene que crecer en un año. 
crecimientoAnual :: Int -> Int
crecimientoAnual edad 
    | edad >= 1 && edad <= 10 = 24 - (edad * 2)
    | edad <= 15              = 4
    | edad <= 17              = 2
    | edad <= 19              = 1
    | otherwise               = 0

--b) Definir la función crecimientoEntreEdades/2, que recibe como parámetros dos edades y devuelve cuánto tiene que crecer una persona entre esas dos edades.

crecimientoEntreEdades :: Int -> Int -> Int
crecimientoEntreEdades edad1 edad2 
    | edad1 < edad2 = crecimientoAnual edad1 + crecimientoEntreEdades (edad1+1) edad2
    | otherwise     = 0


edades = [0..100]

crecimientoEntreEdades' edad1 edad2 = sum (map crecimientoAnual [edad1 .. edad2-1])

--c) Armar una función alturasEnUnAnio/2, que dada una edad y una lista de alturas de personas, devuelva la altura de esas personas un año después.
alturasEnUnAnio :: Int -> [Int] -> [Int]
alturasEnUnAnio edad listaDeAlturas = map ((+) . crecimientoAnual $ edad) listaDeAlturas

--d)Definir la función alturaEnEdades/3, que recibe la altura y la edad de una persona y una lista de edades, y devuelve la lista de la altura que va a tener esa persona en cada una de las edades. P.ej. 
--Main> alturaEnEdades 120 8 [12,15,18] 
--[142,154,164] 

alturaEnEdades :: Int -> Int -> [Int] -> [Int]
alturaEnEdades altura edad listaEdades = map ((altura +). crecimientoEntreEdades edad) listaEdades


--Ejercicio 18
--Se tiene información de las lluvias en un determinado mes por Ej: se conoce la siguiente información: 

lluviasEnero = [0,2,5,1,34,2,0,21,0,0,0,5,9,18,4,0]
--[2,5,1,34,2,0,21,0,0,0,5,9,18,4,0]
--[2,5,1,34,2] ++ lluviaAux([0,21,0,0,0,5,9,18,4,0])
--

--a) Definir la función rachasLluvia/1, que devuelve una lista de las listas de los días seguidos que llovió. P.ej. se espera que la consulta 
--Main> rachasLluvia lluviasEnero 
--[[2,5,1,34,2],[21],[5,9,18,4]].

prueba = [1,2,3,4,5,6]

--No anda
rachasLluvia  = groupBy (\x y -> x /= 0 && y /= 0) 

-- lluviaAux [] = []
-- lluviaAux lluviasMes 
--     | head lluviasMes == 0 = lluviaAux (tail lluviasMes)
--     | otherwise =   ((takeWhile (/= 0) lluviasMes)) ++ lluviaAux (dropWhile (/= 0) lluviasMes) 


--Respuesta Copilot
-- rachasLluvia' [] = []
-- rachasLluvia' (x:xs)
--     | x == 0 = rachasLluvia xs
--     | otherwise = (x : takeWhile (/= 0) xs) : rachasLluvia (dropWhile (/= 0) xs)

--Ejercicio 19
--Definir una función que sume una lista de números. 
--Nota: Resolverlo utilizando foldl/foldr. 

sumaLista :: [Int] -> Int
sumaLista listaNumeros = foldl (+) 0 listaNumeros

--Ejercicio 20
--Definir una función que resuelva la productoria de una lista de números. 
--Nota: Resolverlo utilizando foldl/foldr. 

productoriaLista :: [Int] -> Int
productoriaLista listaNumeros = foldl (*) 1 listaNumeros

--Ejercicio 21
--Definir la función dispersion, que recibe una lista de números y devuelve la dispersión de los valores, o sea máximo - mínimo. 

