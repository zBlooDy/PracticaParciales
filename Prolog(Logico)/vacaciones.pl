% %%%%%%%%%%%%%
% %% Punto 1 %%
% %%%%%%%%%%%%%

% destino(Persona, Lugar)

destino(dodain, pehuenia).
destino(dodain, sanMartinDeLosAndes).
destino(dodain, esquel).
destino(dodain, sarmiento).
destino(dodain, camarones).
destino(dodain, playasDoradas).


destino(alf, bariloche).
destino(alf, sanMartinDeLosAndes).
destino(alf, elBolson).

destino(nico, marDelPlata).

destino(vale, calafate).
destino(vale, elBolson).

destino(martu, Lugar) :-
    destino(nico, Lugar).

destino(martu, Lugar) :-
    destino(alf, Lugar).

% %%%%%%%%%%%%%
% %% Punto 2 %%
% %%%%%%%%%%%%%

% atraccion(Lugar, Atraccion())

atraccion(esquel, parqueNacional(losAlercers)).
atraccion(esquel, excursion(trochita)).
atraccion(esquel, excursion(trevelin)).

atraccion(pehuenia, cerro(bateaMahuida, 2000)).
atraccion(pehuenia, cuerpoAgua(moquehue, pescaHabilitada, 14)).
atraccion(pehuenia, cuerpoAgua(alumine, pescaHabilitada, 19)).

vacacionesCopadas(Persona) :-
    destino(Persona, _),
    forall(destino(Persona, Lugar), tieneAtraccionCopada(Lugar)).

tieneAtraccionCopada(Lugar) :-
    atraccion(Lugar, Atraccion),
    copada(Atraccion).

copada(cerro(_, Altura)) :-
    Altura > 2000.

copada(cuerpoAgua(_, pescaHabilitada, _)).
copada(cuerpoAgua(_, _, TemperaturaMedia)) :-
    TemperaturaMedia > 20.

copada(playa(DiferenciaMarea)) :-
    DiferenciaMarea < 5.

copada(excursion(Nombre)) :-
    atom_length(Nombre, CantidadLetras),
    CantidadLetras > 7.

copada(parqueNacional(_)).
    
% %%%%%%%%%%%%%
% %% Punto 3 %%
% %%%%%%%%%%%%%


noSeCruzaron(Persona, OtraPersona) :-
    destino(Persona, _),
    destino(OtraPersona, _),
    Persona \= OtraPersona,
    not(seCruzaron(Persona, OtraPersona)).

seCruzaron(Persona, OtraPersona) :-
    destino(Persona, Lugar),
    destino(OtraPersona, Lugar).


% %%%%%%%%%%%%%
% %% Punto 4 %%
% %%%%%%%%%%%%%

costoDeVida(sarmiento, 100).
costoDeVida(esquel, 150).
costoDeVida(pehuenia, 180).
costoDeVida(sanMartinDeLosAndes, 150).
costoDeVida(camarones, 135).
costoDeVida(playasDoradas, 170).
costoDeVida(bariloche, 140).
costoDeVida(calafate, 240).
costoDeVida(elBolson, 145).
costoDeVida(marDelPlata, 140).

vacacionesGasoleras(Persona) :-
    destino(Persona, _),
    forall(destino(Persona, Lugar), destinoGasolero(Lugar)).

destinoGasolero(Lugar) :-
    costoDeVida(Lugar, Costo),
    Costo < 160.

% %%%%%%%%%%%%%
% %% Punto 5 %%
% %%%%%%%%%%%%%

itinerariosPosibles(Persona, Destinos) :-
    destino(Persona, _),
    findall(Destino, destino(Persona, Destino), Destinos).