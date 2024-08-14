% vocaloid(Nombre, CancionQueSabeCantar(Nombre, Duracion))
vocaloid(megurineLuka, cancion(nightFever, 4)).
vocaloid(megurineLuka, cancion(foreverYoung, 5)).
vocaloid(hatsuneMiku, cancion(tellYourWord, 4)).
vocaloid(gumi, cancion(foreverYoung, 4)).
vocaloid(gumi, cancion(tellYourWord, 5)).
vocaloid(seeU, cancion(novemberRain, 6)).
vocaloid(seeU, cancion(nightFever, 5)).

%%%%%%%%%%%%%
%% Punto 1 %%
%%%%%%%%%%%%%

esNovedoso(Vocaloid) :-
    sabeAlMenosDosCanciones(Vocaloid),
    duracionCanciones(Vocaloid, Duracion),
    Duracion < 15.

sabeAlMenosDosCanciones(Vocaloid) :-
    vocaloid(Vocaloid, Cancion),
    vocaloid(Vocaloid, OtraCancion),
    Cancion \= OtraCancion.

duracionCanciones(Vocaloid, Duracion) :-
    vocaloid(Vocaloid, _),
    findall(Tiempo, tiempoDeCancion(Vocaloid, Tiempo), TiempoCanciones),
    sumlist(TiempoCanciones, Duracion).

tiempoDeCancion(Vocaloid, Tiempo) :-
    vocaloid(Vocaloid, Cancion),
    duracionCancion(Cancion, Tiempo).


duracionCancion(cancion(_, Duracion), Duracion).

%%%%%%%%%%%%%
%% Punto 2 %%
%%%%%%%%%%%%%

esAcelerado(Vocaloid) :-
    tiempoDeCancion(Vocaloid, Tiempo),
    Tiempo =< 4.


%%%%%%%%%%%%%
%% Punto 3 %%
%%%%%%%%%%%%%

% concierto(Nombre, Pais, Fama, Tipo())

% gigante(MinimoCanciones, DuracionTotalMayorA)
% mediano(DuracionTotalMenorA)
% pequenio(DuracionCancionMayorA)

concierto(mikuExpo, estadosUnidos, 2000, gigante(2, 6)).
concierto(magicalMirai, japon, 3000, gigante(3, 10)).
concierto(vocalektVisions, estadosUnidos, 1000, mediano(9)).
concierto(mikuFest, argentina, 100, pequenio(4)).

%%%%%%%%%%%%%
%% Punto 4 %%
%%%%%%%%%%%%%

puedeParticipar(Vocaloid, Concierto) :-
    vocaloid(Vocaloid, Cancion),
    concierto(Concierto, _, _, Tipo),
    aptoParaConcierto(Vocaloid, Tipo).

aptoParaConcierto(Vocaloid, gigante(MinimoCanciones, DuracionTotalMinima)) :-
    cantidadCanciones(Vocaloid, Cantidad),
    Cantidad > MinimoCanciones,
    duracionCanciones(Vocaloid, DuracionTotal),
    DuracionTotal > DuracionTotalMinima.

cantidadCanciones(Vocaloid, Cantidad) :-
    vocaloid(Vocaloid, _),
    findall(Cancion, vocaloid(Vocaloid, Cancion), Canciones),
    length(Canciones, Cantidad).

aptoParaConcierto(Vocaloid, mediano(DuracionTotalMaxima)) :-
    duracionCanciones(Vocaloid, DuracionTotal),
    DuracionTotal < DuracionTotalMaxima.

aptoParaConcierto(Vocaloid, pequenio(DuracionMinima)) :-
    vocaloid(Vocaloid, Cancion),
    duracionCancion(Cancion, Tiempo),
    Tiempo > DuracionMinima.