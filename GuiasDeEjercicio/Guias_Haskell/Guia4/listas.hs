--Ejercicio 1
-- Definir una función que sume una lista de números. 
--Nota: Investigar sum 

sumaLista :: (Foldable t, Num a) => t a -> a
sumaLista  = sum 

--Ejercicio 2:
-- f cardiaca cada 10 minutos

frecuenciaCardiaca :: [Int]
frecuenciaCardiaca = [80, 100, 120, 128, 130, 123, 125] 

--a) Definir la función promedioFrecuenciaCardiaca, que devuelve el promedio de la frecuencia cardíaca. 
 
promedioFrecuenciaCardiaca :: Double
promedioFrecuenciaCardiaca  = (fromIntegral.sum) frecuenciaCardiaca / (fromIntegral.length) frecuenciaCardiaca

--b)Definir la función frecuenciaCardiacaMinuto/1, que recibe m que es el minuto en el cual quiero conocer la frecuencia cardíaca, m puede ser a los 10, 20, 30 ,40,..hasta 60. 


valorMinuto :: Int -> Int
valorMinuto minuto = div minuto 10

frecuenciaCardiacaMinuto :: Int -> Int
frecuenciaCardiacaMinuto  = (frecuenciaCardiaca !!) . valorMinuto



--c)Definir la función frecuenciasHastaMomento/1, devuelve el total de frecuencias que se obtuvieron hasta el minuto m. 

frecuenciasHastaMomento  = flip take frecuenciaCardiaca . (1+) . valorMinuto 


--Ejercicio 3:
--Definir la función esCapicua/1, si data una lista de listas, me devuelve si la concatenación de las sublistas es una lista capicua.

--              ((concat lista==). reverse.concat) lista
esCapicua :: [String] -> Bool
esCapicua lista = concat lista == (reverse.concat) lista


--Ejercicio 4
--Se tiene información detallada de la duración en minutos de las llamadas que se llevaron a cabo en un período determinado, discriminadas en horario normal y horario reducido. 
--duracionLlamadas = 
--(("horarioReducido",[20,10,25,15]),(“horarioNormal”,[10,5,8,2,9,10])). 


horarioReducido :: [Int]
horarioReducido = [20,10,25,15]

horarioNormal :: [Int]
horarioNormal = [10,5,8,2,9,10]

--a) Definir la función cuandoHabloMasMinutos, devuelve en que horario se habló más cantidad de minutos, en el de tarifa normal o en el reducido. 

