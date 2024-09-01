% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%% Parte 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% sangre(Nombre, Sangre)

% caracteristica(Nombre, Personalidad)

% odia(Nombre, Casa)

sangre(harry, mestiza).
sangre(draco, pura).
sangre(hermione, impura).


caracteristica(harry, corajudo).
caracteristica(harry, amistoso).
caracteristica(harry, orgulloso).
caracteristica(harry, inteligente).
caracteristica(draco, inteligente).
caracteristica(draco, orgulloso).
caracteristica(hermione, inteligente).
caracteristica(hermione, orgulloso).
caracteristica(hermione, responsable).

odia(harry, slytherin).
odia(draco, hufflepuff).

casa(ravenclaw).
casa(slytherin).
casa(hufflepuff).
casa(gryffindor).

% requerimiento(Casa, Personalidad)
requerimiento(gryffindor, corajudo).
requerimiento(slytherin, orgulloso).
requerimiento(slytherin, inteligente).
requerimiento(ravenclaw, inteligente).
requerimiento(ravenclaw, responsable).
requerimiento(hufflepuff, amistoso).

% %%%%%%%%%%%%%
% %% Punto 1 %%
% %%%%%%%%%%%%%

mago(Mago) :-
    sangre(Mago, _).

permiteEntrar(Casa, Mago) :-
    casa(Casa),
    mago(Mago),
    Casa \= slytherin.

permiteEntrar(slytherin, Mago) :-
    sangre(Mago, Sangre),
    Sangre \= impura.

% %%%%%%%%%%%%%
% %% Punto 2 %%
% %%%%%%%%%%%%%

tieneCaracterApropiado(Mago, Casa) :-
    mago(Mago),
    casa(Casa),
    forall(requerimiento(Casa, Personalidad), caracteristica(Mago, Personalidad)).

% %%%%%%%%%%%%%
% %% Punto 3 %%
% %%%%%%%%%%%%%

enDondePuedeQuedar(Mago, Casa) :-
    permiteEntrar(Casa, Mago),
    tieneCaracterApropiado(Mago, Casa),
    not(odia(Mago, Casa)).

enDondePuedeQuedar(hermione, gryffindor).


% %%%%%%%%%%%%%
% %% Punto 4 %%
% %%%%%%%%%%%%%

% cadenaDeAmistades(Magos) Es con explosion combinatoria

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%% Parte 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

accion(harry, irA(fueraDeCama)).
accion(hermione, irA(tercerPiso)).
accion(hermione, irA(seccionRestringidaBiblioteca)).
accion(harry, irA(bosque)).
accion(harry, irA(tercerPiso)).
accion(draco, irA(mazmorras)).
accion(ron, ganarAjedrezMagico).
accion(hermione, salvarAmigos).
accion(harry, ganarAVoldemort).

% Punto 4
accion(hermione, responder(ubicacionBezoar, 20, snapa)).
accion(hermione, responder(comoLevitarPluma, 25, flitwick)).


esDe(hermione, gryffindor).
esDe(ron, gryffindor).
esDe(harry, gryffindor).
esDe(draco, slytherin).
esDe(luna, ravenclaw).

puntosDeAccion(Mago, Puntaje) :-
    accion(Mago, Accion),
    puntos(Accion, Puntaje).

puntos(irA(fueraDeCama), -50).
puntos(irA(bosque), -50).
puntos(irA(tercerPiso), -75).
puntos(irA(seccionRestringidaBiblioteca), -10).
puntos(ganarAjedrezMagico, 50).
puntos(salvarAmigos, 50).
puntos(ganarAVoldemort, 60).
puntos(responder(_, Dificultad, _), Dificultad).

% %%%%%%%%%%%%%
% %% Punto 1 %%
% %%%%%%%%%%%%%

buenAlumno(Mago) :-
    accion(Mago, Accion),
    not(malaAccion(Accion)).

malaAccion(Accion) :-
    puntos(Accion, Puntaje),
    Puntaje < 0.

esRecurrente(Accion) :-
    accion(Mago, Accion),
    accion(OtroMago, Accion),
    Mago \= OtroMago.

% %%%%%%%%%%%%%
% %% Punto 2 %%
% %%%%%%%%%%%%%

puntajeTotal(Casa, PuntajeTotal) :-
    casa(Casa),
    findall(Puntaje, puntajeParaCasa(Casa, Puntaje), Puntajes),
    sumlist(Puntajes, PuntajeTotal).

puntajeParaCasa(Casa, Puntaje) :-
    esDe(Mago, Casa),
    puntosDeAccion(Mago, Puntaje).

% %%%%%%%%%%%%%
% %% Punto 3 %%
% %%%%%%%%%%%%%

casaGanadora(Casa) :-
    puntajeTotal(Casa, Puntaje),
    forall(puntajeTotal(_, OtroPuntaje), Puntaje >= OtroPuntaje).