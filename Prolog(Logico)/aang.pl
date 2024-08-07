% <-- Base de Conocimiento --> 
esPersonaje(aang).
esPersonaje(katara).
esPersonaje(zoka).
esPersonaje(appa).
esPersonaje(momo).
esPersonaje(toph).
esPersonaje(tayLee).
esPersonaje(zuko).
esPersonaje(azula).

% esElementoBasico/1 nos permite conocer los elementos básicos que pueden controlar algunos personajes

esElementoBasico(fuego).
esElementoBasico(agua).
esElementoBasico(tierra).
esElementoBasico(aire).

% elementoAvanzadoDe/2 relaciona un elemento básico con otro avanzado asociado

elementoAvanzadoDe(fuego, rayo).
elementoAvanzadoDe(agua, sangre).
elementoAvanzadoDe(tierra, metal).

% controla/2 relaciona un personaje con un elemento que controla

controla(zuko, rayo).
controla(toph, metal).
controla(katara, sangre).
controla(aang, aire).
controla(aang, agua).
controla(aang, tierra).
controla(aang, fuego).
controla(azula, rayo).
controla(iroh, rayo).

% visito/2 relaciona un personaje con un lugar que visitó. Los lugares son functores que tienen la siguiente forma:
% reinoTierra(nombreDelLugar, estructura)
% nacionDelFuego(nombreDelLugar, soldadosQueLoDefienden)
% tribuAgua(puntoCardinalDondeSeUbica)
% temploAire(puntoCardinalDondeSeUbica)
visito(aang, reinoTierra(baSingSe, [muro, zonaAgraria, sectorBajo, sectorMedio])).
visito(iroh, reinoTierra(baSingSe, [muro, zonaAgraria, sectorBajo, sectorMedio])).
visito(zuko, reinoTierra(baSingSe, [muro, zonaAgraria, sectorBajo, sectorMedio])).
visito(toph, reinoTierra(fortalezaDeGralFong, [cuartel, dormitorios, enfermeria, salaDeGuerra, templo, zonaDeRecreo])).
visito(aang, nacionDelFuego(palacioReal, 1000)).
visito(katara, tribuAgua(norte)).
visito(katara, tribuAgua(sur)).
visito(aang, temploAire(norte)).
visito(aang, temploAire(oeste)).
visito(aang, temploAire(este)).
visito(aang, temploAire(sur)).


% -------------
% -- Punto 1 --
% -------------

esElAvatar(Personaje) :-
    esPersonaje(Personaje),
    forall(esElementoBasico(Elemento), controla(Personaje, Elemento)).

% -------------
% -- Punto 2 --
% -------------

noEsMaestro(Personaje) :-
    esPersonaje(Personaje),
    not((controlaBasico(Personaje), controlaAvanzado(Personaje))).

controlaBasico(Personaje) :-
    controla(Personaje, Elemento),
    esElementoBasico(Elemento).

controlaAvanzado(Personaje) :-
    controla(Personaje, Elemento),
    elementoAvanzadoDe(_, Elemento).

esMaestroPrincipiante(Personaje) :-
    esPersonaje(Personaje),
    controlaBasico(Personaje),
    not(controlaAvanzado(Personaje)).

esMaestroAvanzado(Personaje) :-
    controlaAvanzado(Personaje).

esMaestroAvanzado(Personaje) :-
    esElAvatar(Personaje).

% -------------
% -- Punto 3 --
% -------------

% Para UN personaje, si UN seguidor visito TODOS los lugares del personaje

sigueA(Personaje, Seguidor) :-
    esPersonaje(Personaje),         
    esPersonaje(Seguidor),
    Personaje \= Seguidor,
    forall(visito(Personaje, Lugar), visito(Seguidor, Lugar)).

sigueA(zuko, aang).

% -------------
% -- Punto 4 --
% -------------

esDignoDeConocer(Lugar) :-
    visito(_,Lugar),
    esDigno(Lugar).

esDigno(temploAire(_)).
esDigno(tribuAgua(norte)).
esDigno(reinoTierra(_, Estructura)) :-
    not(member(muro, Estructura)).

% -------------
% -- Punto 5 --
% -------------

esPopular(Lugar) :-
    visito(_, Lugar),
    findall(Personaje, visito(Personaje, Lugar), PersonajesVisitantes),
    length(PersonajesVisitantes, Cantidad),
    Cantidad > 4.

% -------------
% -- Punto 6 --
% -------------

esPersonaje(bumi).

controla(bumi, tierra).

visito(bumi, reinoTierra(baSingSe, [muro, zonaAgraria, sectorBajo, sectorMedio])).

esPersonaje(suki).

visito(suki, nacionDelFuego(prisionMaximaSeguridad, 200)).