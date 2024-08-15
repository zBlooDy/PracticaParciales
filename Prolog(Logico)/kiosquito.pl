% atiende(Persona, Dia, H.base , H.final))
atiende(dodain, lunes, 9, 15).
atiende(dodain, miercoles, 9, 15).
atiende(dodain, viernes, 9, 15).

atiende(lucas, martes,10, 20).

atiende(juanC, sabados,18, 22).
atiende(juanC, domingos,18, 22).

atiende(juanFdS, jueves,10, 20).
atiende(juanFdS, viernes,12, 20).

atiende(leoC, lunes,14, 18).
atiende(leoC, miercoles,14, 18).

atiende(martu, miercoles,23,24).

atiende(vale, lunes,9, 15).
atiende(vale, miercoles,9, 15).
atiende(vale, viernes,9, 15).
atiende(vale, sabados,18, 22).
atiende(vale, domingos,18, 22).

% %%%%%%%%%%%%%
% %% Punto 1 %%
% %%%%%%%%%%%%%

% Por principio de universo cerrado, todo lo que no esta en la base de conocimiento es falso, entonces nadie hace el horario de leoC, y a maiu no la agregamos todavia

% %%%%%%%%%%%%%
% %% Punto 2 %%
% %%%%%%%%%%%%%

quienAtiende(Persona, Dia, Hora) :-
    atiende(Persona, Dia, HoraEntrada, HoraSalida),
    between(HoraEntrada, HoraSalida, Hora).


% %%%%%%%%%%%%%
% %% Punto 3 %%
% %%%%%%%%%%%%%

foreverAlone(Persona, Dia, Hora) :-
    quienAtiende(Persona, Dia, Hora),
    not(estaAcompaniado(Persona, Dia, Hora)).

estaAcompaniado(Persona, Dia, Hora) :-
    quienAtiende(Persona, Dia, Hora),
    quienAtiende(OtraPersona, Dia, Hora),
    Persona \= OtraPersona.

% %%%%%%%%%%%%%
% %% Punto 4 %%
% %%%%%%%%%%%%%

posibilidadesDeAtencion(Dia, Persona) :-
    atiende(Persona, Dia, _, _).

% %%%%%%%%%%%%%
% %% Punto 5 %%
% %%%%%%%%%%%%%

% venta(Persona, Dia, [producto()])

venta(dodain, lunes, [golosina(1200), cigarrillos([jockey]), golosina(50)]).
venta(dodain, miercoles, [bebida(alcoholica, 8), bebida(noAlcoholica, 1), golosina(10)]).
venta(martu, miercoles, [golosina(1000), cigarrillos([chesterfield, colorado, parisiennes])]).
venta(lucas, martes, [golosina(600)]).
venta(lucas, martes, [bebida(noAlcoholica, 2),cigarrillos([derby])]).

vendedorSuertudo(Persona) :-
    venta(Persona, Dia, _),
    forall(venta(Persona, Dia, Ventas), primeraVentaImportante(Ventas)).

primeraVentaImportante(Ventas) :-
    nth1(1, Ventas, PrimeraVenta),
    ventaImportante(PrimeraVenta).

ventaImportante(golosina(Precio)) :-
    Precio > 100.

ventaImportante(cigarrillos(CigarrillosVendidos)) :-
    length(CigarrillosVendidos, Cantidad),
    Cantidad > 2.

ventaImportante(bebidas(alcoholica, _)).

ventaImportante(bebidas(_, Cantidad)) :-
    Cantidad > 5.
