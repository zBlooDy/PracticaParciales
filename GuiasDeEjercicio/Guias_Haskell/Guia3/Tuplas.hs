
--Ejercicio 1
--Definir las funciones fst3, snd3, trd3, que dada una tupla de 3 elementos devuelva el elemento correspondiente, p.ej. 
--Main> snd3 (4,5,6) 
--5
fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a

snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

trd3 :: (a,b,c) -> c
trd3 (a,b,c) = c


--Ejercicio 2
--Definir la función aplicar, que recibe como argumento una tupla de 2 elementos con funciones y un entero, me devuelve como resultado una tupla con el resultado de aplicar el elemento a cada una de la funciones, ej: 
--Main> aplicar (doble,triple) 8 
--(16,24) 
--Main> aplicar ((3+),(2*)) 8 
--(11,16)

aplicar :: (Int -> Int , Int -> Int) -> Int -> (Int, Int)
aplicar (f,g) numero = (f numero, g numero)

--Ejercicio 3
--Definir la función cuentaBizarra, que recibe un par y: si el primer elemento es mayor al segundo devuelve la suma, si el segundo le lleva más de 10 al primero devuelve la resta 2do – 1ro, y si el segundo es más grande que el 1ro pero no llega a llevarle 10, devuelve el producto. Ej: 
--Main> cuentaBizarra (5,8)
--40

cuentaBizarra :: (Int, Int) -> Int
cuentaBizarra (a,b) 
    | a > b     = a + b
    | b-a >= 10 = b - a
    | otherwise = a * b

--Ejercicio 4
--Representamos las notas que se sacó un alumno en dos parciales mediante un par (nota1,nota2), p.ej. un patito en el 1ro y un 7 en el 2do se representan mediante el par (2,7). 

--a) Definir la función esNotaBochazo, recibe un número y devuelve True si no llega a 6, False en caso contrario. No vale usar guardas. 

esNotaBochazo :: Int -> Bool
esNotaBochazo  = (<6) 

--b) Definir la función aprobo, recibe un par e indica si una persona que se sacó esas notas aprueba. Usar esNotaBochazo. 

aprobo :: (Int, Int) -> Bool
aprobo (nota1, nota2) = (not.esNotaBochazo)nota1 && (not.esNotaBochazo)nota2

--c) Definir la función promociono, que indica si promocionó, para eso tiene las dos notas tienen que sumar al menos 15 y además haberse sacado al menos 7 en cada parcial. 

promociono :: (Int, Int) -> Bool
promociono (nota1, nota2) = nota1+nota2 >= 15 && nota1 >= 7 && nota2 >= 7

--d) Escribir una consulta que dado un par indica si aprobó el primer parcial, usando esNotaBochazo y composición. La consulta tiene que tener esta forma (p.ej. para el par de notas (5,8)) 
--Main> (... algo ...) (5,8) 

aproboPrimerParcial :: (Int, Int) -> Bool
aproboPrimerParcial (nota1, nota2) = not.esNotaBochazo $ nota1

--Ejercicio 5
--Siguiendo con el dominio del ejercicio anterior, tenemos ahora dos parciales con dos recuperatorios, lo representamos mediante un par de pares ((parc1,parc2),(recup1,recup2)). 
--Si una persona no rindió un recuperatorio, entonces ponemos un "-1" en el lugar correspondiente. 

--a) Definir la función notasFinales que recibe un par de pares y devuelve el par que corresponde a las notas finales del alumno para el 1er y el 2do parcial. P.ej. 
--Main> notasFinales ((2,7),(6,-1)) 
--(6,7) 

notasFinales :: ((Int, Int), (Int,Int)) -> (Int, Int)
notasFinales ((parc1,parc2),(recup1,recup2)) = (max parc1 recup1 , max parc2 recup2)

--b)Escribir la consulta que indica si un alumno cuyas notas son ((2,7),(6,-1)) recursa o no. O sea, la respuesta debe ser True si recursa, y False si no recursa. Usar las funciones definidas en este punto y el anterior, y composición. La consulta debe tener esta forma:
--Main> (... algo ...) ((2,7),(6,-1))


recursaAlumno :: ((Int, Int), (Int,Int)) -> Bool
recursaAlumno = not.aprobo.notasFinales

--Ejercico 6
--Definir la función esMayorDeEdad, que dada una tupla de 2 elementos (persona, edad) me devuelva True si es mayor de 21 años y False en caso contrario. Por Ej:.


esMayorDeEdad :: (String, Int) -> Bool
esMayorDeEdad ( _ , edad) = edad >= 21

--Ejercicio 7
--Definir la función calcular, que recibe una tupla de 2 elementos, si el primer elemento es par lo duplica, sino lo deja como está y con el segundo elemento en caso de ser impar le suma 1 y si no deja esté último como esta. 

duplicaPares :: Int -> Int
duplicaPares numero 
    | even numero = 2* numero
    | otherwise = numero

siguienteImpares :: Int -> Int
siguienteImpares numero
    | odd numero = numero + 1
    | otherwise = numero

calcular :: (Int, Int) -> (Int, Int)
calcular (numero1 , numero2) = (duplicaPares numero1, siguienteImpares numero2)