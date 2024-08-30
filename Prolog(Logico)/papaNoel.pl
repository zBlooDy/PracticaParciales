% persona(Nombre, Edad)

persona(laura, 24).
persona(federico, 31).
persona(maria, 23).
persona(jacobo, 45).
persona(enrique, 49).
persona(andrea, 38).
persona(gabriela, 4).
persona(gonzalo, 23).
persona(alejo, 20).
persona(andres, 11).
persona(ricardo, 39).
persona(ana, 7).
persona(juana, 15).

% quiere(Quien, Quiere)
quiere(andres, juguete(maxStell, 150)).
quiere(andres, bloques([piezaT, piezaL, cubo, piezaChata])).
quiere(maria, bloques([piezaT, piezaT])).
quiere(alejo, bloques([piezaT])).
quiere(juana, juguete(barbie, 175)).
quiere(federico, abrazo).
quiere(enrique, abrazo).
quiere(gabriela, juguete(gabeneitor2000, 5000)).
quiere(laura, abrazo).
quiere(gonzalo, abrazo).

% presupuesto(Quien, Presupuesto)

presupuesto(jacobo, 20).
presupuesto(enrique, 2311).
presupuesto(ricardo, 154).
presupuesto(andrea, 100).
presupuesto(laura, 2000).

% accion(Quien, Hizo)

accion(andres, travesura(3)).
accion(andres, ayudar(ana)).
accion(ana, golpear(andres)).
accion(ana, travesura(1)).
accion(maria, ayudar(federico)).
accion(maria, favor(juana)).
accion(juana, favor(maria)).
accion(federico, golpear(enrique)).
accion(gonzalo, golpear(alejo)).
accion(alejo, travesura(4)).

% padre(Padre o Madre, Hijo o Hija)

padre(jacobo, ana).
padre(jacobo, juana).
padre(enrique, federico).
padre(ricardo, maria).
padre(andrea, andres).
padre(laura, gabriela).


creeEnPapaNoel(federico).

creeEnPapaNoel(Persona) :-
    esChico(Persona).

esChico(Persona) :-
    persona(Persona, Edad),
    Edad < 13.

% %%%%%%%%%%%%%
% %% Punto 1 %%
% %%%%%%%%%%%%%

buenaAccion(Accion) :-
    accion(_, Accion),
    esBuena(Accion).

esBuena(favor(_)).
esBuena(ayudar(_)).
esBuena(travesura(Nivel)) :-
    Nivel =< 3.

% %%%%%%%%%%%%%
% %% Punto 2 %%
% %%%%%%%%%%%%%

sePortoBien(Persona) :-
    accion(Persona, _),
    forall(accion(Persona, Accion), esBuena(Accion)).

% %%%%%%%%%%%%%
% %% Punto 3 %%
% %%%%%%%%%%%%%

malcriador(Padre) :-
    padre(Padre, _),
    forall(padre(Padre, Hijo), malcriado(Hijo)).

malcriado(Persona) :-
    accion(Persona, _),
    forall(accion(Persona, Accion), hizoMalaAccion(Persona, Accion)).

malcriado(Persona) :-
    not(creeEnPapaNoel(Persona)).


hizoMalaAccion(Persona, Accion) :-
    accion(Persona, Accion),
    not(buenaAccion(Accion)).

% %%%%%%%%%%%%%
% %% Punto 4 %%
% %%%%%%%%%%%%%

puedeCostear(Padre, Hijo) :-
    padre(Padre, Hijo),
    precioDeJuguetes(Hijo, PrecioTotal),
    costea(Padre, PrecioTotal).

costea(Padre, PrecioTotal) :-
    presupuesto(Padre, Presupuesto),
    Presupuesto >= PrecioTotal.

precioDeJuguetes(Hijo, PrecioFinal) :-
    findall(Precio, precioDeLoQueQuiere(Hijo, Precio), Precios),
    sumlist(Precios, PrecioFinal).

precioDeLoQueQuiere(Hijo, Precio) :-
    quiere(Hijo, Quiere),
    precio(Quiere, Precio).

precio(juguete(_, Precio), Precio).
precio(bloques(Bloques), Precio) :-
    length(Bloques, CantidadBloques),
    Precio is CantidadBloques * 3.
precio(abrazo(_), 0).

% %%%%%%%%%%%%%
% %% Punto 5 %%
% %%%%%%%%%%%%%

regaloCandidatoPara(Regalo, Persona) :-
    sePortoBien(Persona),
    quiere(Persona, Regalo),
    capazDePagarSuPadre(Persona, Regalo),
    creeEnPapaNoel(Persona).

capazDePagarSuPadre(Persona, Regalo) :-
    padre(Padre, Persona),
    precio(Regalo, Precio),
    costea(Padre, Precio).

% %%%%%%%%%%%%%
% %% Punto 6 %%
% %%%%%%%%%%%%%

regalosQueRecibe(Persona, Regalos) :-
    quiere(Persona, _),
    findall(Regalo, recibeRegalo(Persona, Regalo), Regalos).

recibeRegalo(Persona, Regalo) :-
    quiere(Persona, Regalo),
    puedePagar(Persona, Regalo).

puedePagar(Persona, Regalo) :-
    capazDePagarSuPadre(Persona, Regalo).

puedePagar(Persona, Regalo) :-
    not(capazDePagarSuPadre(Persona, Regalo)),
    regaloDeCompensacion(Persona, Regalo).

regaloDeCompensacion(Persona, parDeMedias) :-
    sePortoBien(Persona).

regaloDeCompensacion(Persona, carbon) :-
    hizoMalaAccion(Persona, Accion1),
    hizoMalaAccion(Persona, Accion2),
    Accion1 \= Accion2.

% %%%%%%%%%%%%%
% %% Punto 7 %%
% %%%%%%%%%%%%%

suggarDaddy(Padre) :-
    padre(Padre, _),
    forall(padre(Padre, Hijo), quiereRegaloEspecial(Hijo)).

quiereRegaloEspecial(Hijo) :-
    quiere(Hijo, Regalo),
    regaloEspecial(Regalo).

regaloEspecial(Regalo) :-
    precio(Regalo, Precio),
    Precio > 500.

regaloEspecial(juguete(woody, _)).
regaloEspecial(juguete(buzz, _)).
regaloEspecial(bloques(Bloques)) :-
    member(cubo, Bloques).
