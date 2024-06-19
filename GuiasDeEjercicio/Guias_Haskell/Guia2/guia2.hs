-- <====================== APLICACION PARCIAL ===================>


-- Definir una función siguiente, que al invocarla con un número cualquiera me devuelve el resultado de sumar a ese número el 1. 
--Main> siguiente 3
--4

siguiente :: Num a => a -> a
siguiente numero = (+1) numero

--Definir la función mitad que al invocarla con un número cualquiera me devuelve la mitad de dicho número, ej: 
--Main> mitad 5
--2.5

mitad :: Fractional a => a -> a
mitad valor = (/2) valor


--Definir una función inversa, que invocando a la función con un número cualquiera me devuelva su inversa. 
--Main> inversa 4
--0.25
--Main> inversa 0.5
--2.0

inversa :: Fractional a => a -> a
inversa numero = (/numero) 1

--Definir una función triple, que invocando a la función con un número cualquiera me devuelva el triple del mismo.
--Main> triple 5 
--15
triple :: Num a => a -> a
triple numero = (3*) numero

--Definir una función esNumeroPositivo, que invocando a la función con un número cualquiera me devuelva true si el número es positivo y false en caso contrario. 
--Main> esNumeroPositivo (-5)
--False
--Main> esNumeroPositivo 0.99
--True 

esNumeroPositivo :: (Ord a, Num a) => a -> Bool
esNumeroPositivo numero = (>=0) numero



-- <====================== COMPOSICION FUNCIONES ===================>

--Resolver la función del ejercicio 5 de la guía anterior esBisiesto/1, utilizando aplicación parcial y composición.
esMultiploDe :: Integral a => a -> a -> Bool
esMultiploDe numero1 numero2 = ((== 0) . mod numero2) numero1

esBisiesto :: Integral a => a -> Bool
esBisiesto anio  = (esMultiploDe 400 anio  || esMultiploDe 4 anio)  && (esMultiploDe 100 anio) 

--Resolver la función inversaRaizCuadrada/1, que da un número n devolver la inversa su raíz cuadrada. 
--Main> inversaRaizCuadrada 4 
--0.5 
--Nota: Resolverlo utilizando la función inversa Ej. 2.3, sqrt y composición

inversaRaizCuadrada :: Floating c => c -> c
inversaRaizCuadrada numero = (inversa.sqrt) numero

--Definir una función incrementMCuadradoN, que invocándola con 2 números m y n, incrementa un valor m al cuadrado de n por Ej: 
--Main> incrementMCuadradoN 3 2 
--11 
--Incrementa 2 al cuadrado de 3, da como resultado 11. Nota: Resolverlo utilizando aplicación parcial y composición. 
cuadrado :: Num a => a -> a
cuadrado numero = numero * numero

incrementMCuadradoN :: Num a => a -> a -> a
incrementMCuadradoN m n =  ((+n) . cuadrado) m



--Definir una función esResultadoPar/2, que invocándola con número n y otro m, devuelve true si el resultado de elevar n a m es par. 
--Main> esResultadoPar 2 5 
--True 
--Main> esResultadoPar 3 2
--False 
--Nota Obvia: Resolverlo utilizando aplicación parcial y composición.

esResultadoPar :: Integral a => a -> a -> Bool
esResultadoPar n m = (even . (n^)) m


