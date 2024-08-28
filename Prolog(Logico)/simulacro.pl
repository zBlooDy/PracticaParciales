% juego(Nombre, Tipo(), Precio)

% accion
% rol(CantidadJugadores)
% puzzle(Niveles, Dificultad)

juego(gta, caracteristicas(accion), 2000).
juego(fortnite, caracteristicas(accion), 300).
juego(dungeonsAndDragons, caracteristicas(rol , 500), 3500).
juego(detectives, caracteristicas(puzzle , 10, facil), 4000).

% oferta(Juego, PorcentajeDescuento)

oferta(gta, 25).


cuantoSale(Juego, Dinero) :-
    juego(Juego, _, Precio),
    precioSegunoferta(Juego, Precio, Dinero).

precioSegunoferta(Juego, PrecioJuego , Dinero) :-
    oferta(Juego, Porcentaje),
    Dinero is PrecioJuego - PrecioJuego * (Porcentaje / 100).

precioSegunoferta(Juego, PrecioJuego, PrecioJuego) :-
    not(oferta(Juego, _)).


tieneBuenDescuento(Juego) :-
    oferta(Juego, PorcentajeDescuento),
    PorcentajeDescuento >= 50.

popular(minecraft).
popular(counterStrike).

popular(Juego) :-
    juego(Juego, Caracteristicas, _),
    popularSegun(Caracteristicas).

popularSegun(caracteristicas(accion)).

popularSegun(caracteristicas(rol, CantidadJugadores)) :-
    CantidadJugadores > 1000000.

popularSegun(caracteristicas(puzzle, _, facil)).
popularSegun(caracteristicas(puzzle, 25, _)).




% Otra forma de hacer lo de usuario

usuario(blood, gta).
usuario(blood, fortnite).
usuario(blood, dungeonsAndDragons).
usuario(fer, detectives).

compra(blood, propio(detectives)).
compra(blood, regalo(counterStrike, fer)).
compra(fer, regalo(minecraft, blood)).

%% Adicto a los descuentos %%

adictoADescuentos(Usuario) :-
    compra(Usuario, _),
    forall(compra(Usuario, Compra), compraConBuenDescuento(Compra)).

compraConBuenDescuento(Compra) :-
    juegoDeCompra(Compra, Juego),
    tieneBuenDescuento(Juego).

juegoDeCompra(propio(Juego), Juego).
juegoDeCompra(regalo(Juego, _), Juego).


%% Fanatico %%

fanatico(Usuario, Genero) :-
    tieneJuegoDe(Usuario, Genero, Juego1),
    tieneJuegoDe(Usuario, Genero, Juego2),
    Juego1 \= Juego2.

tieneJuegoDe(Usuario, Genero, Juego) :-
    usuario(Usuario, Juego),
    generoDe(Juego, Genero).

generoDe(Juego, Genero) :-
    juego(Juego, Caracteristicas, _),
    genero(Caracteristicas, Genero).


genero(caracteristicas(Genero), Genero).    
genero(caracteristicas(Genero, _), Genero).
genero(caracteristicas(Genero, _, _), Genero).

%% Monotematico %%

monotematico(Usuario, Genero) :-
    usuario(Usuario, _),
    forall(usuario(Usuario, Juego), generoDe(Juego, Genero)).

%% Buenos Amigos %%

buenosAmigos(Usuario, OtroUsuario) :-
    haceRegaloPopular(Usuario, OtroUsuario),
    haceRegaloPopular(OtroUsuario, Usuario).

haceRegaloPopular(Usuario, OtroUsuario) :-
    compra(Usuario, regalo(Juego, OtroUsuario)),
    popular(Juego).


%% Cuanto Gastara %%

cuantoGastara(Usuario, Dinero) :-
    usuario(Usuario, _),
    findall(ValorJuego, valorDeCompra(Usuario, ValorJuego), ValoresJuegos),
    sumlist(ValoresJuegos, Dinero).

valorDeCompra(Usuario, Valor) :-
    compra(Usuario, Compra),
    juegoDeCompra(Compra, Juego),
    cuantoSale(Juego, Valor).