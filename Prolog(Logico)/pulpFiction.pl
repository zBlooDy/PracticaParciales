personaje(pumkin, ladron([licorerias, estacionesDeServicio])).
personaje(honeyBunny, ladron([licorerias, estacionesDeServicio])).
personaje(vincent, mafioso(maton)).
personaje(jules, mafioso(maton)).
personaje(marsellus, mafioso(capo)).
personaje(winston, mafioso(resuelveProblemas)).
personaje(mia, actriz([foxForceFive])).
personaje(butch, boxeador).

pareja(marsellus, mia).
pareja(pumkin, honeyBunny).

%trabajaPara(Empleador, Empleado)
trabajaPara(marsellus, vincent).
trabajaPara(marsellus, jules).
trabajaPara(marsellus, winston).


% %%%%%%%%%%%%%
% %% Punto 1 %%
% %%%%%%%%%%%%%

esPeligroso(Personaje) :-
    realizaActividadesPeligrosas(Personaje).

esPeligroso(Personaje) :-
    trabajaPara(Personaje, Empleado),
    realizaActividadesPeligrosas(Empleado).

realizaActividadesPeligrosas(Personaje) :-
    personaje(Personaje, mafioso(maton)).

realizaActividadesPeligrosas(Personaje) :-
    personaje(Personaje, ladron(LugaresQueRoba)),
    member(licorerias, LugaresQueRoba).

% %%%%%%%%%%%%%
% %% Punto 2 %%
% %%%%%%%%%%%%%

amigo(vincent, jules).
amigo(jules, jimmie).
amigo(vincent, elVendedor).


duoTemible(Personaje1, Personaje2) :-
    esPeligroso(Personaje1),
    esPeligroso(Personaje2),
    tienenUnVinculo(Personaje1, Personaje2).

tienenUnVinculo(Personaje, OtroPersonaje) :-
    amigo(Personaje, OtroPersonaje).

tienenUnVinculo(Personaje, OtroPersonaje) :-
    pareja(Personaje, OtroPersonaje).

% %%%%%%%%%%%%%
% %% Punto 3 %%
% %%%%%%%%%%%%%

%encargo(Solicitante, Encargado, Tarea). 
% Las tareas pueden ser cuidar(Protegido), ayudar(Ayudado), buscar(Buscado, Lugar)
encargo(marsellus, vincent, cuidar(mia)).
encargo(vincent,  elVendedor, cuidar(mia)).
encargo(marsellus, winston, ayudar(jules)).
encargo(marsellus, winston, ayudar(vincent)).
encargo(marsellus, vincent, buscar(butch, losAngeles)).

estaEnProblemas(Personaje) :-
    trabajaPara(Jefe, Personaje),
    esPeligroso(Jefe),
    pareja(Jefe, Pareja),
    encargo(Jefe, Personaje, cuidar(Pareja)).

estaEnProblemas(Personaje) :-
    encargo(_, Personaje, buscar(OtroPersonaje, _)),
    personaje(OtroPersonaje, boxeador).

estaEnProblemas(butch).

% %%%%%%%%%%%%%
% %% Punto 4 %%
% %%%%%%%%%%%%%

sanCayetano(Personaje) :-
    tieneCerca(Personaje, _),
    forall(tieneCerca(Personaje, OtroPersonaje), encargo(Personaje, OtroPersonaje, _)).

tieneCerca(Personaje, OtroPersonaje) :-
    amigo(Personaje, OtroPersonaje).

tieneCerca(Personaje, OtroPersonaje) :-
    trabajaPara(Personaje, OtroPersonaje).

% %%%%%%%%%%%%%
% %% Punto 5 %%
% %%%%%%%%%%%%%

masAtareado(Personaje) :-
    cantidadEncargos(Personaje, CantidadEncargos),
    forall(cantidadEncargos(_, OtraCantidadEncargos), CantidadEncargos >= OtraCantidadEncargos).

cantidadEncargos(Personaje, Cantidad) :-
    personaje(Personaje, _),
    findall(Encargo, encargo(_, Personaje, Encargo), Encargos),
    length(Encargos, Cantidad).

% %%%%%%%%%%%%%
% %% Punto 6 %%
% %%%%%%%%%%%%%

personajesRespetables(Personajes) :-
    findall(Personaje, esRespetable(Personaje), Personajes).

esRespetable(Personaje) :-
    personaje(Personaje, Actividad),
    nivelDeRespetoSegun(Actividad, Nivel),
    Nivel > 9.

nivelDeRespetoSegun(actriz(Peliculas), Nivel) :-
    length(Peliculas, Cantidad),
    Nivel is Cantidad / 10.

nivelDeRespetoSegun(mafioso(TipoDeMafioso), Nivel) :-
    nivelDeMafia(TipoDeMafioso, Nivel).

nivelDeMafia(resuelveProblemas, 10).
nivelDeMafia(maton, 1).
nivelDeMafia(capo, 20).

% %%%%%%%%%%%%%
% %% Punto 7 %%
% %%%%%%%%%%%%%

hartoDe(Personaje, OtroPersonaje) :-
    encargo(_, Personaje, _),
    personaje(OtroPersonaje, _),
    forall(encargo(_, Personaje, Tarea), interactua(Tarea, OtroPersonaje)).


interactua(TareaDeOtro, OtroPersonaje) :-
    estaInvolucrado(TareaDeOtro, OtroPersonaje).

interactua(TareaDeOtro, OtroPersonaje) :-
    amigo(OtroPersonaje, Amigo),
    estaInvolucrado(TareaDeOtro, Amigo).

estaInvolucrado(cuidar(Persona), Persona).
estaInvolucrado(ayudar(Persona), Persona).
estaInvolucrado(buscar(Persona, _), Persona).

% %%%%%%%%%%%%%
% %% Punto 8 %%
% %%%%%%%%%%%%%

caracteristicas(vincent, [negro, muchoPelo, tieneCabeza]).
caracteristicas(jules, [tieneCabeza, muchoPelo]).
caracteristicas(marvin, [negro]).


duoDiferenciable(Personaje, OtroPersonaje) :-
    tienenUnVinculo(Personaje, OtroPersonaje),
    tieneAlgoQueElOtroNo(Personaje, OtroPersonaje).

tieneAlgoQueElOtroNo(Personaje, OtroPersonaje) :-
    caracteristicas(Personaje, Caracteristicas),
    caracteristicas(OtroPersonaje, OtrasCaracteristicas),
    member(Elemento, Caracteristicas),
    not(member(Elemento, OtrasCaracteristicas)).

