persona(bart).
persona(larry).
persona(otto).
persona(marge).

%los magios son functores alMando(nombre, antiguedad), novato(nombre) y elElegido(nombre).
persona(alMando(burns,29)).
persona(alMando(clark,20)).
persona(novato(lenny)).
persona(novato(carl)).
persona(elElegido(homero)).

hijo(homero,abbe).
hijo(bart,homero).
hijo(larry,burns).

salvo(carl,lenny).
salvo(homero,larry).
salvo(otto,burns).

%Los beneficios son funtores confort(descripcion), confort(descripcion, caracteristica), 
% dispersion(descripcion), economico(descripcion, monto).
gozaBeneficio(carl, confort(sillon)).
gozaBeneficio(lenny, confort(sillon)).
gozaBeneficio(lenny, confort(estacionamiento, techado)).
gozaBeneficio(carl, confort(estacionamiento, libre)).
gozaBeneficio(clark, confort(viajeSinTráfico)).
gozaBeneficio(clark, dispersion(fiestas)).
gozaBeneficio(burns, dispersion(fiestas)).
gozaBeneficio(lenny, economico(descuento, 500)).

% %%%%%%%%%%%%%
% %% Punto 1 %%
% %%%%%%%%%%%%%

aspiranteMagio(Persona) :-
    persona(Persona),
    descendenteMagio(Persona).

aspiranteMagio(Persona) :-
    persona(Persona),
    salvo(Persona, OtraPersona),
    esMagio(OtraPersona).

descendenteMagio(Persona) :-
    hijo(Persona, Padre),
    esMagio(Padre).

esMagio(Persona) :- persona(alMando(Persona, _)).
esMagio(Persona) :- persona(novato(Persona)).
esMagio(Persona) :- persona(elElegido(Persona)).
    
% %%%%%%%%%%%%%
% %% Punto 2 %%
% %%%%%%%%%%%%%

puedeDarOrdenes(Persona, OtraPersona) :-
    persona(alMando(Persona, _)),
    persona(novato(OtraPersona)).

puedeDarOrdenes(Persona, OtraPersona) :-
    persona(alMando(Persona, Nivel1)),
    persona(alMando(OtraPersona, Nivel2)),
    Nivel1 > Nivel2.

puedeDarOrdenes(Persona, OtraPersona) :-
    persona(elElegido(Persona)),
    esMagio(OtraPersona).

% %%%%%%%%%%%%%
% %% Punto 3 %%
% %%%%%%%%%%%%%

sienteEnvidia(Persona, OtrasPersonas) :-
    persona(Persona),
    findall(OtraPersona, tieneEnvidia(Persona, OtraPersona), OtrasPersonas).

tieneEnvidia(Persona, OtraPersona) :-
    aspiranteMagio(Persona),
    esMagio(OtraPersona).

tieneEnvidia(Persona, OtraPersona) :-
    persona(Persona),
    not(aspiranteMagio(Persona)),
    aspiranteMagio(OtraPersona).

tieneEnvidia(Persona, OtraPersona) :-
    persona(novato(Persona)),
    persona(alMando(OtraPersona, _)).

% %%%%%%%%%%%%%
% %% Punto 4 %%
% %%%%%%%%%%%%%

masEnvidioso(Persona) :-
    cantidadDePersonasEnvidiadas(Persona, Cantidad),
    forall(cantidadDePersonasEnvidiadas(_, OtraCantidad), Cantidad >= OtraCantidad).

cantidadDePersonasEnvidiadas(Persona, Cantidad) :-
    sienteEnvidia(Persona, OtrasPersonas),
    length(OtrasPersonas, Cantidad).

% %%%%%%%%%%%%%
% %% Punto 5 %%
% %%%%%%%%%%%%%

soloLoGoza(Persona, Beneficio) :-
    gozaBeneficio(Persona, Beneficio),
    forall(gozaBeneficio(Persona, OtroBeneficio) , Beneficio \= OtroBeneficio).

% %%%%%%%%%%%%%
% %% Punto 6 %%
% %%%%%%%%%%%%%

% tipoDeBeneficioMasAprovechado(Beneficio) :-

% Pareceria que hay que usar funciones raras con listas

% La idea seria juntar las listas de cada beneficio, en una lista, y contar cual es la de mayor longitud

% Justificar:
% ¿Dónde se aprovecho el uso del polimorfismo? ¿Con qué objetivo?

% El polimorfismo se aprovecho en esMagio para ver los distintos tipos de casos en las que una Persona podria ser magio (novato, alMando, elElegido)