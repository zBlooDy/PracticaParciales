% resultado(UnPais, GolesDeUnPais, OtroPais, GolesDeOtroPais)

resultado(paises_bajos, 3, estados_unidos, 1).
resultado(australia, 1, argentina, 2).
resultado(polonia, 3, francia, 1).
resultado(inglaterra, 3, senegal, 0).

% pronostico(Nombre, Apuesta())

pronostico(juan, gano(paises_bajos, estados_unidos, 3 , 1)).
pronostico(juan, gano(argentina, australia, 3, 0)).
pronostico(juan, empataron(inglaterra, senegal, 0)).
pronostico(gus, gano(estados_unidos, paises_bajos, 1, 0)).
pronostico(gus, gano(japon, croacia, 2, 0)).
pronostico(lucas, gano(paises_bajos, estados_unidos, 3, 1)).
pronostico(lucas, gano(argentina, australia, 2, 0)).
pronostico(lucas, gano(croacia, japon, 1, 0)).

% %%%%%%%%%%%%%
% %% Punto 1 %%
% %%%%%%%%%%%%%

jugaron(Pais1, Pais2, Diferencia) :-
    resultado(Pais1, GolesPais1, Pais2, GolesPais2),
    Diferencia is GolesPais1 - GolesPais2.

gano(Pais, OtroPais) :-
    jugaron(Pais, OtroPais, Diferencia),
    Diferencia > 0.

% %%%%%%%%%%%%%
% %% Punto 2 %%
% %%%%%%%%%%%%%

puntosPronostico(Pronostico, Puntos) :-
    pronostico(_, Pronostico),
    puntosSegunPronostico(Pronostico, Puntos).

puntosSegunPronostico(gano(PaisGanador, PaisPerdedor, GolesGanador, GolesPerdedor), 200) :-
    resultado(PaisGanador, GolesGanador, PaisPerdedor, GolesPerdedor),
    gano(PaisGanador, PaisPerdedor).

puntosSegunPronostico(empataron(Pais, OtroPais, Goles), 200) :-
    resultado(Pais, Goles, OtroPais, Goles),
    empataron(Pais, OtroPais).






empataron(Pais, OtroPais) :-
    jugaron(Pais, OtroPais, 0).



