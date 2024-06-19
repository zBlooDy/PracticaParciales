--SENCILLAS: SE LLAMAN ELEMENTO Y TIENEN
--Nombre, Simbolo quimico, Numero atomico

--COMPUESTAS: Se llaman compuestos y tienen una serie de componentes
--Un componente es un par formado por sustancia y cant de moleculas (sustancia, cantidadMoleculas). La sustancia del componente puede ser elemento o compuesto

--Compuestos tienen:
--Nombre y simbolo o formula quimica (la cual no nos interesa saber en todo momento)

--TODAS LAS SUTANCIAS TIENEN UN GRUPO O ESPECIE Y SON:
-- Metal / No Metal / Halogeno / Gas noble

data Componente = Componente {
    sustancia :: TipoSustancia,
    cantidadMoleculas :: Int
} deriving (Eq, Show)


data TipoSustancia = 
    Elemento {
        nombre :: String,
        simbolo :: String,
        numeroAtomico :: Int,
        grupo :: Especie
    } 
    | Compuesto {
        nombre :: String,
        grupo :: Especie,
        componentes :: [Componente]
    } deriving (Eq, Show)

data Especie = Metal | NoMetal | Halogeno | GasNoble deriving(Eq, Show)



--1)Modelar las siguientes sustancias:
--El hidrógeno y el oxígeno
--El agua, sustancia compuesta por 2 hidrógenos y 1 un oxígeno.

hidrogeno :: TipoSustancia
hidrogeno =  Elemento {
    nombre = "Hidrogeno",
    simbolo = "H",
    numeroAtomico = 1,
    grupo = NoMetal
}

oxigeno :: TipoSustancia
oxigeno =  Elemento {
    nombre = "Oxigeno",
    simbolo = "O",
    numeroAtomico = 16,
    grupo = NoMetal
}

fluor :: TipoSustancia
fluor =  Elemento {
    nombre = "Fluor",
    simbolo = "F",
    numeroAtomico = 17,
    grupo = Halogeno
}

mercurio :: TipoSustancia
mercurio =  Elemento {
    nombre = "Mercurio",
    simbolo = "Hg", 
    numeroAtomico = 12,
    grupo = Metal
}

agua :: TipoSustancia
agua =  Compuesto {
    nombre = "Agua",
    grupo = NoMetal,
    componentes = [Componente hidrogeno 2, Componente oxigeno 1]
} 


--2) Saber si conduce bien una sustancia.
--Poder saber si una sustancia conduce bien según un criterio. Los criterios actuales son electricidad y calor, pero podría haber más. Los metales conducen bien cualquier criterio, sean compuestos o elementos. Los elementos que sean gases nobles conducen bien la electricidad, y los compuestos halógenos conducen bien el calor. Para el resto, no son buenos conductores.
--El criterio actual es electricidad y calor

data Conductores = Electricidad | Calor deriving(Eq, Show)

conduceBien :: Conductores -> TipoSustancia -> Bool
conduceBien _ (Elemento _ _ _ Metal)  = True
conduceBien _ (Compuesto _ Metal _)   = True
conduceBien Electricidad (Elemento _ _ _ GasNoble) = True
conduceBien Calor (Compuesto _ Halogeno _) = True
conduceBien _ _ = False



--3) Obtener el nombre de unión de un nombre. Esto se logra añadiendo “uro” al final del nombre, pero solo si el nombre termina en consonante. Si termina en vocal, se busca hasta la última consonante y luego sí, se le concatena “uro”. Por ejemplo, el nombre de unión del Fluor es “fluoruro”, mientras que el nombre de unión del mercurio es “mercururo

esVocal letra = elem letra "aeiouAEIOU"


unionNombreSustancia :: TipoSustancia -> String
unionNombreSustancia sustancia
    | (not.esVocal.last.nombre) sustancia = nombre sustancia ++ "uro"
    | otherwise                         = (nombreParaConcatenar.nombre) sustancia ++ "uro"




nombreParaConcatenar :: String -> String
nombreParaConcatenar nombre = reverse.dropWhile esVocal $ reverse nombre


--4) Combinar 2 nombres. Al nombre de unión del primero lo concatenamos con el segundo, agregando un “ de “ entre medio. Por ejemplo, si combino “cloro” y “sodio” debería obtener “cloruro de sodio"


combinarNombres :: TipoSustancia -> TipoSustancia -> String
combinarNombres sustancia1 sustancia2 = unionNombreSustancia sustancia1 ++ " de " ++ nombre sustancia2

--5)Mezclar una serie de componentes entre sí. El resultado de dicha mezcla será un compuesto. Sus componentes serán los componentes mezclados. El nombre se forma de combinar los nombres de la sustancia de cada componente. La especie será, arbitrariamente, un no metal.



nombreComponente :: Componente -> String
nombreComponente  = nombre.sustancia  

creaListaNombres  = map (nombre.sustancia) 

mezclarComponentes :: [Componente] -> TipoSustancia
mezclarComponentes listaComponentesMezcla =  Compuesto {
    nombre = foldl1 (++) (creaListaNombres listaComponentesMezcla),
    grupo = NoMetal,
    componentes = listaComponentesMezcla
}

-- 6) Obtener la fórmula de una sustancia:
-- para los elementos es su símbolo químico
-- para los compuestos es la concatenación de las representaciones de sus componentes y se pone entre paréntesis

-- La representación de un componente depende de la cantidad de moléculas:
-- -> si tiene una, entonces solo es la fórmula de su sustancia
-- -> si tiene más, entonces es la fórmula de su sustancia y la cantidad
-- Por ejemplo, la fórmula del agua debería ser (H2O). ¡Recuerden que una sustancia compuesta puede estar compuesta por otras sustancias compuestas!

formulaSustancia :: TipoSustancia -> String
formulaSustancia (Elemento _ simbolo _ _) = simbolo

--Falta terminar