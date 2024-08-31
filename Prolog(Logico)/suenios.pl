% %%%%%%%%%%%%%
% %% Punto 1 %%
% %%%%%%%%%%%%%

% cree(Persona, Personaje)

cree(gabriel, campanita).
cree(gabriel, magoDeOz).
cree(gabriel, cavenaghi).
cree(juan, conejoDePascua).
cree(macarena, reyesMagos).
cree(macarena, magoCapria).
cree(macarena, campanita).

% suenio(Persona, Suenio())

% cantante(CantidadDiscos)
% futbolista(Equipo)
% loteria(NumerosQueAposto)

suenio(gabriel, loteria([5, 9])).
suenio(gabriel, futbolista(arsenal)).
suenio(juan, cantante(100000)).
suenio(macarena, cantante(10000)).

% %%%%%%%%%%%%%
% %% Punto 2 %%
% %%%%%%%%%%%%%

ambiciosa(Persona) :-
    suenio(Persona, _),
    dificultadDeSuenios(Persona, Dificultad),
    Dificultad > 20.

dificultadDeSuenios(Persona, DificultadTotal) :-
    findall(Dificultad, dificultadSuenio(Persona, Dificultad), Dificultades),
    sumlist(Dificultades, DificultadTotal).

dificultadSuenio(Persona, Dificultad) :-
    suenio(Persona, Suenio),
    dificultad(Suenio, Dificultad).

dificultad(cantante(CantidadDiscos), 6) :-
    CantidadDiscos > 500000.

dificultad(cantante(CantidadDiscos), 4) :-
    CantidadDiscos =< 500000.

dificultad(loteria(Numeros), Dificultad) :-
    length(Numeros, CantidadNumeros),
    Dificultad is 10 * CantidadNumeros.

dificultad(futbolista(Equipo), 3) :-
    equipoChico(Equipo).

dificultad(futbolista(Equipo), 16) :-
    not(equipoChico(Equipo)).

equipoChico(arsenal).
equipoChico(aldosivi).

% %%%%%%%%%%%%%
% %% Punto 3 %%
% %%%%%%%%%%%%%

tieneQuimica(Persona, Personaje) :-
    cree(Persona, Personaje),
    sonCompatibles(Persona, Personaje).

sonCompatibles(Persona, campanita) :-
    dificultadSuenio(Persona, Dificultad),
    Dificultad =< 5.

sonCompatibles(Persona, Personaje) :-
    Personaje \= campanita,
    not(ambiciosa(Persona)),
    forall(suenio(Persona, Suenio), puro(Suenio)).


puro(futbolista(_)).
puro(cantante(CantidadDiscos)) :-
    CantidadDiscos < 200000.

% %%%%%%%%%%%%%
% %% Punto 4 %%
% %%%%%%%%%%%%%

amigos(campanita, reyesMagos).
amigos(campanita, conejoDePascua).

amigos(conejoDePascua, cavenaghi).

puedeAlegrar(Personaje, Persona) :-
    suenio(Persona, _),
    tieneQuimica(Persona, Personaje),
    estaEnBuenasCondiciones(Personaje).

estaEnBuenasCondiciones(Personaje) :-
    not(estaEnfermo(Personaje)).

estaEnBuenasCondiciones(Personaje) :-
    estaEnfermo(Personaje),
    amigos(Personaje, Amigo),
    estaEnBuenasCondiciones(Amigo).

estaEnfermo(campanita).
estaEnfermo(reyesMagos).
estaEnfermo(conejoDePascua).