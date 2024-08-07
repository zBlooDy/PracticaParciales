% <-- Base de conocimiento -->

%puntajes(Competidor, PuntajesDeSaltos)
puntajes(hernan,[3,5,8,6,9]).
puntajes(julio,[9,7,3,9,10,2]).
puntajes(ruben,[3,5,3,8,3]).
puntajes(roque,[7,10,10]).


% -------------
% -- Punto 1 --
% -------------

puntajeSegunSalto(Competidor, Salto, Puntaje) :-
    puntajes(Competidor, PuntajesDeSaltos),
    nth1(Salto, PuntajesDeSaltos, Puntaje).

% -------------
% -- Punto 2 --
% -------------

estaDescalificado(Competidor) :-
    puntajes(Competidor, PuntajesDeSaltos),
    length(PuntajesDeSaltos, Cantidad),
    Cantidad > 5.

% -------------
% -- Punto 3 --
% -------------

clasificaALaFinal(Competidor) :-
    puntajes(Competidor, PuntajesDeSaltos),
    sumlist(PuntajesDeSaltos, Resultado),
    Resultado >= 28.

clasificaALaFinal(Competidor) :-
    puntajes(Competidor, PuntajesDeSaltos),
    findall(_, buenaPuntuacion(PuntajesDeSaltos), PuntuacionesAltas),
    length(PuntuacionesAltas, Cantidad),
    saltosNecesarios(Cantidad).

saltosNecesarios(2).

buenaPuntuacion(PuntajesDeSaltos) :-
    nth1(_, PuntajesDeSaltos, Elemento),
    Elemento >= 8.