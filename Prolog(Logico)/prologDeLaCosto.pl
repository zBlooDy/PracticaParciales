% No aseguro que este bien, es un parcial largo y estaba medio cansado cuando lo hice :(


% puestoDeComida(Comida, Precio)
puestoDeComida(hamburgesa, 2000).
puestoDeComida(panchitosConPapa, 1500).
puestoDeComida(lomitosCompletos, 2500).
puestoDeComida(caramelos, 0).

% atraccion(Nombre, Tipo())

% tranquila(ParaQuien)
atraccion(autitosChocadores, tranquila(familia)).
atraccion(casaEmbrujada, tranquila(familia)).
atraccion(laberinto, tranquila(familia)).
atraccion(tobogan, tranquila(chicos)).
atraccion(calesita, tranquila(chicos)).

% intensa(CoeficienteLanzamiento)
atraccion(barcoPirata, intensa(14)).
atraccion(tazasChinas, intensa(6)).
atraccion(simulador3D, intensa(2)).

% montaniaRusa(Giros, Duracion)

atraccion(abismoMortalRecargada, montaniaRusa(3, 194)).
atraccion(paseoPorElBosque, montaniaRusa(0, 45)).

% acuatica

atraccion(torpedoSalpicon, acuatica).
atraccion(esperoQueHayasTraidoMudaDeRopa, acuatica).

% visitante(Nombre, Dinero, Edad)
% sentimientos(Nombre, Hambre, Aburrimiento)

% grupo(Nombre, Integrante)

visitante(eusebio, 3000, 80).
visitante(carmela, 0, 80).

sentimientos(eusebio, 50, 0).
sentimientos(carmela, 0, 25).

grupo(viejitos, eusebio).
grupo(viejitos, carmela).

% Quien no tenga grupo ==> solo


esChico(Persona) :-
    visitante(Persona, _, Edad),
    Edad < 13.
 
% %%%%%%%%%%%%%
% %% Punto 2 %%
% %%%%%%%%%%%%%

vieneAcompaniado(Persona) :-
    visitante(Persona, _, _),
    grupo(_, Persona).

vieneSolo(Persona) :-
    visitante(Persona, _, _),
    not(vieneAcompaniado(Persona)).


estadoDeBienestar(Persona, Estado) :-
    sentimientos(Persona, Hambre, Aburrimiento),
    Suma is Hambre + Aburrimiento,
    bienestar(Persona, Suma, Estado).

bienestar(Persona, 0, felicidadPlena) :-
    vieneAcompaniado(Persona).

bienestar(Persona, 0, podriaEstarMejor) :-
    vieneSolo(Persona).

bienestar(_, Suma, podriaEstarMejor) :-
    between(1, 50, Suma).

bienestar(_, Suma, necesitaEntretenerse) :-
    between(51, 99, Suma).

bienestar(_, Suma, quiereIrseACasa) :-
    Suma > 100.

% %%%%%%%%%%%%%
% %% Punto 3 %%
% %%%%%%%%%%%%%


puedeSatisfacer(Comida, Grupo) :-
    grupo(Grupo, _),
    puestoDeComida(Comida, _),
    forall(grupo(Grupo, Integrante), satisface(Comida, Integrante)).


satisface(Comida, Integrante) :-
    puedeComprar(Comida, Integrante),
    leQuitaElHambre(Comida, Integrante).

puedeComprar(Comida, Integrante) :-
    puestoDeComida(Comida, Precio),
    visitante(Integrante, Dinero, _),
    Dinero >= Precio.

leQuitaElHambre(hamburgesa, Integrante) :-
    sentimientos(Integrante, Hambre, _),
    Hambre < 50.

leQuitaElHambre(panchitosConPapa, Integrante) :-
    esChico(Integrante).

leQuitaElHambre(lomitosCompletos, _).

leQuitaElHambre(caramelos, Integrante) :-
    visitante(Integrante, Dinero, _),
    forall(puestoDeComida(Comida, _), noLeAlcanza(Integrante, Comida)).

noLeAlcanza(Integrante, Comida) :-
    not(puedeComprar(Comida, Integrante)),
    Comida \= caramelos.

% %%%%%%%%%%%%%
% %% Punto 4 %%
% %%%%%%%%%%%%%

lluviaDeHamburgesas(Persona) :-
    puedeComprar(hamburgesa, Persona),
    vaAVomitarCon(Persona, Atraccion).

vaAVomitarCon(Persona, Atraccion) :-
    atraccion(Atraccion, Tipo),
    atraccionParaVomitar(Persona, Tipo).

atraccionParaVomitar(_, tobogan).

atraccionParaVomitar(_, intensa(CoeficienteLanzamiento)) :-
    CoeficienteLanzamiento > 10.

atraccionParaVomitar(Persona, montaniaRusa(Giros, Duracion)) :-
    peligrosaPara(Persona, Giros, Duracion).


peligrosaPara(Persona, Giros, _) :-
    not(esChico(Persona)),
    not(estadoDeBienestar(Persona, necesitaEntretenerse)),
    forall(atraccion(_, montaniaRusa(OtrosGiros, _)), Giros >= OtrosGiros).

peligrosaPara(Persona, _, Duracion) :-
    esChico(Persona), 
    Duracion > 60.

% %%%%%%%%%%%%%
% %% Punto 5 %%
% %%%%%%%%%%%%%

opcionDeEntretenimiento(Visitante, Mes, Opcion) :-
    visitante(Visitante, _, _),
    mes(Mes),
    opcionParaEntretenerse(Visitante, Mes, Opcion).

mes(Mes) :-
    member(Mes, [enero, febrero, marzo, abril, mayo, junio, julio, agosto, septiembre, octubre, noviembre, diciembre]).

opcionParaEntretenerse(Visitante, _, PuestoDeComida) :-
    puestoDeComida(PuestoDeComida, _),
    puedeComprar(PuestoDeComida, Visitante).

opcionParaEntretenerse(Visitante, _, Atraccion) :-
    atraccion(Atraccion, tranquila(Franja)),
    puedeAcceder(Visitante, Franja).

opcionParaEntretenerse(Visitante, _, Atraccion) :-
    atraccion(Atraccion, intensa(_)).

opcionParaEntretenerse(Visitante, _, Atraccion) :-
    atraccion(Atraccion, montaniaRusa(Giros, Duracion)),
    not(peligrosaPara(Visitante, Giros, Duracion)).

opcionParaEntretenerse(Visitante, Mes, Atraccion) :-
    atraccion(Atraccion, acuatica),
    member(Mes, [septiembre, octubre, noviembre, diciembre, enero, febrero, marzo]).


puedeAcceder(Visitante, familia).

puedeAcceder(Visitante, chicos) :-
    esChico(Visitante).

puedeAcceder(Visitante, chicos) :-
    grupo(Grupo, Visitante),
    not(esChico(Visitante)),
    hayAlgunChico(Grupo).

hayAlgunChico(Grupo) :-
    grupo(Grupo, Integrante),
    esChico(Integrante).