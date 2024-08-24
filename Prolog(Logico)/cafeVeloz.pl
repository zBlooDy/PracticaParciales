% jugadores conocidos
jugador(maradona).
jugador(chamot).
jugador(balbo).
jugador(caniggia).
jugador(passarella).
jugador(pedemonti).
jugador(basualdo).

% relaciona lo que toma cada jugador
tomo(maradona, sustancia(efedrina)).
tomo(maradona, compuesto(cafeVeloz)).
tomo(caniggia, producto(cocacola, 2)).
tomo(chamot, compuesto(cafeVeloz)).
tomo(balbo, producto(gatoreit, 2)).

% relaciona la máxima cantidad de un producto que 1 jugador puede ingerir
maximo(cocacola, 3). 
maximo(gatoreit, 1).
maximo(naranju, 5).

% relaciona las sustancias que tiene un compuesto
composicion(cafeVeloz, [efedrina, ajipupa, extasis, whisky, cafe]).

% sustancias prohibidas por la asociación
sustanciaProhibida(efedrina). 
sustanciaProhibida(cocaina).

% %%%%%%%%%%%%%
% %% Punto 1 %%
% %%%%%%%%%%%%%

% tomo(passarella, Sustancia) 
%     tomo(_, Sustancia),
%     not(tomo(maradona, Sustancia)).

% tomo(pedemonti, Sustancia) 
%     tomo(maradona, Sustancia),
%     tomo(chamot, Sustancia).

% %%%%%%%%%%%%%
% %% Punto 2 %%
% %%%%%%%%%%%%%

puedeSerSuspendido(Jugador) :-
    tomoSustanciaProhibida(Jugador).

puedeSerSuspendido(Jugador) :-
    tomoDeMas(Jugador).

tomoSustanciaProhibida(Jugador) :-
    tomo(Jugador, Sustancia),
    estaProhibida(Sustancia).

estaProhibida(sustancia(Sustancia)) :-
    sustanciaProhibida(Sustancia).

estaProhibida(compuesto(Sustancia)) :-
    composicion(Sustancia, Ingredientes),
    member(SustanciaDeCompuesto, Ingredientes),
    sustanciaProhibida(SustanciaDeCompuesto).

tomoDeMas(Jugador) :-
    tomo(Jugador, producto(Bebida, Cantidad)),
    maximo(Bebida, Limite),
    Cantidad > Limite.

% %%%%%%%%%%%%%
% %% Punto 3 %%
% %%%%%%%%%%%%%

amigo(maradona, caniggia).
amigo(caniggia, balbo).
amigo(balbo, chamot).
amigo(balbo, pedemonti).

malaInfluencia(Jugador1, Jugador2) :-
    puedeSerSuspendido(Jugador1),
    puedeSerSuspendido(Jugador2),
    sonConocidos(Jugador1, Jugador2).


sonConocidos(Jugador1, Jugador2) :-
    amigo(Jugador1, Jugador2).

sonConocidos(Jugador1, Jugador2) :-
    amigo(Jugador1, OtroJugador),
    sonConocidos(OtroJugador, Jugador2).

% %%%%%%%%%%%%%
% %% Punto 4 %%
% %%%%%%%%%%%%%

% atiende(Medico, Jugador)
atiende(cahe, maradona).
atiende(cahe, chamot).
atiende(cahe, balbo).
atiende(cahe, caniggia).

atiende(zin, caniggia).
atiende(cureta, pedemonti).
atiende(cureta, basualdo).

chanta(Medico) :-
    atiende(Medico, Jugador),
    puedeSerSuspendido(Jugador).

% %%%%%%%%%%%%%
% %% Punto 5 %%
% %%%%%%%%%%%%%


% nivelFalopez/2 (Sustancia, Nivel).
nivelFalopez(efedrina, 10).
nivelFalopez(cocaina, 100).
nivelFalopez(extasis, 120).
nivelFalopez(omeprazol, 5).

cuantaFalopaTiene(Jugador, Cantidad) :-
    tomo(Jugador, _),
    findall(Nivel, nivelDeFalopa(Jugador, Nivel), Niveles),
    sumlist(Niveles, Cantidad).

nivelDeFalopa(Jugador, Nivel) :-
    tomo(Jugador, Sustancia),
    nivelDeSustancia(Sustancia, Nivel).

nivelDeSustancia(producto(_, _), 0).

nivelDeSustancia(sustancia(Sustancia), Nivel) :-
    nivelFalopez(Sustancia, Nivel).

nivelDeSustancia(compuesto(SustanciaCompuesta), NivelTotal) :-
    composicion(SustanciaCompuesta, Ingredientes),
    findall(Nivel, nivelComposicion(Ingredientes, Nivel), Niveles),
    sumlist(Niveles, NivelTotal).

nivelComposicion(Ingredientes, Nivel) :-
    member(Componente, Ingredientes),
    nivelFalopez(Componente, Nivel).


% %%%%%%%%%%%%%
% %% Punto 6 %%
% %%%%%%%%%%%%%

medicoConProblemas(Medico) :-
    atiende(Medico, _),
    cantidadJugadoresConflictivosDe(Medico, Cantidad),
    Cantidad > 3.

cantidadJugadoresConflictivosDe(Medico, Cantidad) :-
    findall(Jugador, atiendeAConflictivo(Medico, Jugador), Jugadores),
    length(Jugadores, Cantidad).

atiendeAConflictivo(Medico, Jugador) :-
    atiende(Medico, Jugador),
    esConflictivo(Jugador).

esConflictivo(Jugador) :-
    puedeSerSuspendido(Jugador).

esConflictivo(Jugador) :-
    sonConocidos(Jugador, maradona).


% %%%%%%%%%%%%%
% %% Punto 7 %%
% %%%%%%%%%%%%%

programaTVFantinesco(Jugadores) :-
    findall(Jugador, puedeSerSuspendido(Jugador), Jugadores).