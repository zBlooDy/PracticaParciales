% amigo/1 Relaciona quienes son amigos de nuestro cliente
amigo(mati). 
amigo(pablo). 
amigo(leo).
amigo(fer). 
amigo(flor).
amigo(ezequiel). 
amigo(marina).

% define quiénes no se pueden ver
noSeBanca(leo, flor). 
noSeBanca(pablo, fer).
noSeBanca(fer, leo). 
noSeBanca(flor, fer).

% define cuáles son las comidas y cómo se componen
% achura(Nombre, CantidadCalorias)
% ensalada(Nombre, Ingredientes)
% morfi(Nombre)
comida(achura(chori, 200)). 
comida(achura(chinchu, 150)).
comida(ensalada(waldorf, [manzana, apio, nuez, mayo])).
comida(ensalada(mixta, [lechuga, tomate, cebolla])).
comida(morfi(vacio)).
comida(morfi(mondiola)).
comida(morfi(asado)).

% relacionamos la comida que se sirvió en cada asado
% cada asado se realizó en una única fecha posible: functor fecha + comida

asado(fecha(22,9,2011), chori). 
asado(fecha(22,9,2011), waldorf). 
asado(fecha(22,9,2011), vacio).
asado(fecha(15,9,2011), mondiola).
asado(fecha(15,9,2011), mixta).
asado(fecha(15,9,2011), chinchu).

% relacionamos quiénes asistieron a ese asado
asistio(fecha(15,9,2011), flor). 
asistio(fecha(22,9,2011), marina).
asistio(fecha(15,9,2011), pablo). 
asistio(fecha(22,9,2011), pablo).
asistio(fecha(15,9,2011), leo). 
asistio(fecha(22,9,2011), flor).
asistio(fecha(15,9,2011), fer). 
asistio(fecha(22,9,2011), mati).

% definimos qué le gusta a cada persona
leGusta(mati, chori). 
leGusta(fer, mondiola). 
leGusta(pablo, asado).
leGusta(mati, vacio). 
leGusta(fer, vacio).
leGusta(mati, waldorf). 
leGusta(flor, mixta).

% %%%%%%%%%%%%%
% %% Punto 1 %%
% %%%%%%%%%%%%%

leGusta(ezequiel , chori).
leGusta(ezequiel , vacio).
leGusta(ezequiel , mondiola).
leGusta(ezequiel , waldorf).

leGusta(marina, mixta).
leGusta(marina, mondiola).

% %%%%%%%%%%%%%
% %% Punto 2 %%
% %%%%%%%%%%%%%

asadoViolento(FechaAsado) :-
    asado(FechaAsado, _),
    forall(asistio(FechaAsado, Persona), tuvoQueSoportar(Persona, FechaAsado)).

tuvoQueSoportar(Persona, FechaAsado) :-
    asistio(FechaAsado, OtraPersona),
    noSeBanca(Persona, OtraPersona).

% %%%%%%%%%%%%%
% %% Punto 3 %%
% %%%%%%%%%%%%%

calorias(NombreComida, Calorias) :-
    comida(Comida),
    caloriasSegunComida(Comida, Calorias, NombreComida).

caloriasSegunComida(ensalada(Nombre, Ingredientes), Calorias, Nombre) :-
    length(Ingredientes, Calorias).

caloriasSegunComida(achura(Nombre, Calorias), Calorias, Nombre).

caloriasSegunComida(morfi(Nombre), 200, Nombre).

% %%%%%%%%%%%%%
% %% Punto 4 %%
% %%%%%%%%%%%%%


asadoFlojito(FechaAsado) :-
    asado(FechaAsado, _),
    caloriasDeTodoElAsado(FechaAsado, Calorias),
    Calorias < 400.

caloriasDeTodoElAsado(FechaAsado, TotalDeCalorias) :-
    findall(Caloria , caloriasDeAsado(FechaAsado, Caloria), Calorias),
    sumlist(Calorias, TotalDeCalorias).

caloriasDeAsado(FechaAsado, Calorias) :-
    asado(FechaAsado, Comida),
    calorias(Comida, Calorias).

% %%%%%%%%%%%%%
% %% Punto 5 %%
% %%%%%%%%%%%%%

hablo(fecha(15,09,2011), flor, pablo). 
hablo(fecha(22,09,2011), flor, marina).
hablo(fecha(15,09,2011), pablo, leo). 
hablo(fecha(22,09,2011), marina, pablo).
hablo(fecha(15,09,2011), leo, fer). 
reservado(marina).

chismeDe(FechaAsado, Persona1, Persona2) :-
    hablo(FechaAsado, Persona2, Persona1).

% No se entiende la otra parte, de que alguien que sabe un chisme de la P2, le cuente a la P1 ?


% %%%%%%%%%%%%%
% %% Punto 6 %%
% %%%%%%%%%%%%%

disfruto(Persona, FechaAsado) :-
    asistio(FechaAsado, Persona),
    cantidadComidasQueLeGustaron(Persona, FechaAsado, Cantidad),
    Cantidad >= 3.

cantidadComidasQueLeGustaron(Persona, FechaAsado, Cantidad) :-
    findall(Comida, (asado(FechaAsado,Comida),leGusta(Persona, Comida)), Comidas),
    length(Comidas, Cantidad).


leGustaComidaDelAsado(FechaAsado, Persona, Comida) :-
    asado(FechaAsado, Comida),
    leGusta(Persona, Comida).

% %%%%%%%%%%%%%
% %% Punto 7 %%
% %%%%%%%%%%%%%

asadoRico(Comidas) :-
    comida(Comida),
    findall(Comida, comidaRica(Comida), Comidas).

comidaRica(morfi(_)).

comidaRica(ensalada(_, Ingredientes)) :-
    length(Ingredientes, Cantidad),
    Cantidad > 3.

comidaRica(achura(chori, _)).
comidaRica(achura(morci, _)).


