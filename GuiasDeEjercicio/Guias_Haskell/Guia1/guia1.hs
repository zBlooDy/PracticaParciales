laBotellaHola = 1000

double :: Num a => a -> a
double numero = 2 * numero

esMultiploDeTres :: Integral a => a -> Bool
esMultiploDeTres numero = (mod numero 3) == 0

esMultiploDe :: Integral a => a -> a -> Bool
esMultiploDe numero1 numero2 = (mod numero2 numero1) == 0

cubo :: Num a => a -> a
cubo numero = (numero * numero * numero)

area :: Num a => a -> a -> a
area base altura = (base * altura)

esBisiesto :: Integral a => a -> Bool
esBisiesto anio  = (esMultiploDe 400 anio) == True || ((esMultiploDe 4 anio) == True && (esMultiploDe 100 anio) == False)

celsiusToFahr :: Fractional a => a -> a
celsiusToFahr temperaturaCelsius = (32 + (9/5) * temperaturaCelsius)

fahrToCelsius :: Fractional a => a -> a
fahrToCelsius temperaturaFahr = (temperaturaFahr - 32) * 5/9

haceFrioF :: (Ord a, Fractional a) => a -> Bool
haceFrioF temperaturaFahr = (fahrToCelsius temperaturaFahr) < 8

mcm :: Integral a => a -> a -> a
mcm numero1 numero2 = div (numero1 * numero2) (mcd numero1 numero2)

mcd :: Integral a => a -> a -> a
mcd numero1 numero2 = gcd numero1 numero2

dispersion :: (Num a, Ord a) => a -> a -> a -> a
dispersion dia1 dia2 dia3 = (max dia1 (max dia2 dia3)) - (min dia1 (min dia2 dia3))

diasParejos :: (Num a, Ord a) => a -> a -> a -> Bool
diasParejos dia1 dia2 dia3 = ((dispersion dia1 dia2 dia3) < 30)

diasLocos :: (Num a, Ord a) => a -> a -> a -> Bool
diasLocos dia1 dia2 dia3 = ((dispersion dia1 dia2 dia3) > 100)

diasNormales :: (Num a, Ord a) => a -> a -> a -> Bool
diasNormales dia1 dia2 dia3 = (((diasParejos dia1 dia2 dia3) == False) && ((diasLocos dia1 dia2 dia3) == False))

pesoPino :: (Ord a, Num a) => a -> a
pesoPino altura
    | altura <= 300 = 3 * altura
    | altura > 300 = 2 * altura

esPesoUtil :: (Ord a, Num a) => a -> Bool
esPesoUtil peso = (peso >= 400 && peso <= 1000)

sirvePino :: (Ord a, Num a) => a -> Bool
sirvePino altura = esPesoUtil (pesoPino altura)


