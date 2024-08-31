% %%%%%%%%%%%%%
% %% Parte 1 %%
% %%%%%%%%%%%%%

% pokemon(Nombre, Tipo)

pokemon(pikachu, electrico).
pokemon(charizard, fuego).
pokemon(venusaur, planta).
pokemon(blastoise, agua).
pokemon(totodile, agua).
pokemon(snorlax, normal).
pokemon(rayquaza, dragon).
pokemon(rayquaza, volador).

% entrenador(Nombre, PokemonQueTiene)

entrenador(ash, pikachu).
entrenador(ash, charizard).
entrenador(brock, snorlax).
entrenador(misty, blastoise).
entrenador(misty, venusaur).
entrenador(misty, arceus).

tipoMultiple(Pokemon) :-
    pokemon(Pokemon, Tipo),
    pokemon(Pokemon, OtroTipo),
    Tipo \= OtroTipo.

legendario(Pokemon) :-
    tipoMultiple(Pokemon),
    not(entrenador(_, Pokemon)).

misterioso(Pokemon) :-
    pokemon(Pokemon, Tipo),
    unicoONadieLoTiene(Pokemon, Tipo).

unicoONadieLoTiene(Pokemon, Tipo) :-
    forall((pokemon(OtroPokemon, OtroTipo), Pokemon \= OtroPokemon), Tipo \= OtroTipo).

unicoONadieLoTiene(Pokemon, _) :-
    not(entrenador(_, Pokemon)).

% %%%%%%%%%%%%%
% %% Parte 2 %%
% %%%%%%%%%%%%%

% %%%%%%%%%%%%%%%%%%%%% Punto 1 %%%%%%%%%%%%%%%%%%%%% %

movimiento(pikachu, fisico(mordedura, 95)).
movimiento(pikachu, especial(impactrueno, electrico, 40)).

movimiento(charizard, especial(garraDragon, dragon, 100)).
movimiento(charizard, fisico(mordedura, 95)).

movimiento(blastoise, defensivo(proteccion, 10)).
movimiento(blastoise, fisico(placaje, 50)).

movimiento(arceus, especial(impactrueno, electrico, 40)).
movimiento(arceus, especial(garraDragon, dragon, 100)).
movimiento(arceus, defensivo(proteccion, 10)).
movimiento(arceus,fisico(placaje, 50)).
movimiento(arceus, defensivo(alivio , 100)).


danioDeAtaque(Movimiento, Danio) :-
    movimiento(_, TipoMovimiento),
    nombreMovimiento(TipoMovimiento, Movimiento),
    danio(TipoMovimiento, Danio).

nombreMovimiento(fisico(Nombre, _), Nombre).
nombreMovimiento(especial(Nombre, _, _), Nombre).
nombreMovimiento(defensivo(Nombre, _), Nombre).

danio(fisico(_, Potencia), Potencia).
danio(defensivo(_, _), 0).
danio(especial(_, Tipo, Potencia), Danio) :-
    multiplicadorSegunTipo(Tipo, Multiplicador),
    Danio is Potencia * Multiplicador.

multiplicadorSegunTipo(Tipo, 1.5) :-
    tipoBasico(Tipo).

multiplicadorSegunTipo(dragon, 3).

multiplicadorSegunTipo(Tipo, 1) :-
    not(tipoBasico(Tipo)),
    Tipo \= dragon.

tipoBasico(fuego).
tipoBasico(agua).
tipoBasico(planta).
tipoBasico(normal).

% %%%%%%%%%%%%%%%%%%%%% Punto 2 %%%%%%%%%%%%%%%%%%%%% %

capacidadOfensiva(Pokemon, Capacidad) :-
    movimiento(Pokemon, _),
    findall(Danio, danioDeUnAtaque(Pokemon, Danio), Danios),
    sumlist(Danios, Capacidad).

danioDeUnAtaque(Pokemon, Danio) :-
    movimiento(Pokemon, TipoMovimiento),
    danio(TipoMovimiento, Danio).

% %%%%%%%%%%%%%%%%%%%%% Punto 3 %%%%%%%%%%%%%%%%%%%%% %

picante(Entrenador) :-
    entrenador(Entrenador, _),
    forall(entrenador(Entrenador, Pokemon), pokemonPicante(Pokemon)).

pokemonPicante(Pokemon) :-
    misterioso(Pokemon).

pokemonPicante(Pokemon) :-
    capacidadOfensiva(Pokemon, Capacidad),
    Capacidad > 200.
