%tarea(agente, tarea, ubicacion)
%tareas:
%  ingerir(descripcion, tamaño, cantidad)
%  apresar(malviviente, recompensa)
%  asuntosInternos(agenteInvestigado)
%  vigilar(listaDeNegocios)

tarea(vigilanteDelBarrio, ingerir(pizza, 1.5, 2),laBoca).
tarea(vigilanteDelBarrio, vigilar([pizzeria, heladeria]), barracas).
tarea(canaBoton, asuntosInternos(vigilanteDelBarrio), barracas).
tarea(sargentoGarcia, vigilar([pulperia, haciendaDeLaVega, plaza]),puebloDeLosAngeles).
tarea(sargentoGarcia, ingerir(vino, 0.5, 5),puebloDeLosAngeles).
tarea(sargentoGarcia, apresar(elzorro, 100), puebloDeLosAngeles). 
tarea(vega, apresar(neneCarrizo,50),avellaneda).
tarea(jefeSupremo, vigilar([congreso,casaRosada,tribunales]),laBoca).

ubicacion(puebloDeLosAngeles).
ubicacion(avellaneda).
ubicacion(barracas).
ubicacion(marDelPlata).
ubicacion(laBoca).
ubicacion(uqbar).

%jefe(jefe, subordinado)
jefe(jefeSupremo,vega ).
jefe(vega, vigilanteDelBarrio).
jefe(vega, canaBoton).
jefe(jefeSupremo,sargentoGarcia).


% %%%%%%%%%%%%%
% %% Punto 1 %%
% %%%%%%%%%%%%%

frecuenta(Agente, Ubicacion) :-
    tarea(Agente, _, Ubicacion).

frecuenta(Agente, buenosAires) :-
    tarea(Agente, _, _).

frecuenta(vega, quilmes).

frecuenta(Agente, marDelPlata) :-
    tarea(Agente, vigilar(Negocios), _),
    member(negocioDeAlfajor, Negocios).

% %%%%%%%%%%%%%
% %% Punto 2 %%
% %%%%%%%%%%%%%

innacesible(Ubicacion) :-
    ubicacion(Ubicacion),
    not(frecuenta(_, Ubicacion)).

% %%%%%%%%%%%%%
% %% Punto 3 %%
% %%%%%%%%%%%%%

afincado(Agente) :-
    tarea(Agente, _, Ubicacion),
    forall(tarea(Agente, _, OtraUbicacion) , sonIguales(Ubicacion, OtraUbicacion)).

sonIguales(Ubicacion, Ubicacion).

% %%%%%%%%%%%%%
% %% Punto 4 %%
% %%%%%%%%%%%%%


% No se como hacerlo je

% %%%%%%%%%%%%%
% %% Punto 5 %%
% %%%%%%%%%%%%%

agentePremiado(Agente) :-
    puntuacionAgente(Agente, Puntuacion),
    forall(puntuacionAgente(_, OtraPuntuacion), OtraPuntuacion =< Puntuacion).

puntuacionAgente(Agente, Puntuacion) :-
    tarea(Agente, _, _),
    findall(Puntaje, puntuacionSegunTarea(Agente, Puntaje), Puntajes),
    sumlist(Puntajes, Puntuacion).

puntuacionSegunTarea(Agente, Puntaje) :-
    tarea(Agente, Tarea, _),
    puntajeDeTarea(Tarea, Puntaje).

puntajeDeTarea(vigilar(Negocios), Puntaje) :-
    length(Negocios, CantidadDeNegocios),
    Puntaje is CantidadDeNegocios * 5.

puntajeDeTarea(ingerir(_, Tamanio, Cantidad) , Puntaje) :-
    Unidad is Tamanio * Cantidad,
    Puntaje is Unidad * (-10).

puntajeDeTarea(apresar(_, Recompensa), Puntaje) :-
    Puntaje is Recompensa / 2.

puntajeDeTarea(asuntosInternos(AgenteInvestigado), Puntaje) :-
    puntuacionAgente(AgenteInvestigado, Puntuacion),
    Puntaje is Puntuacion * 2.