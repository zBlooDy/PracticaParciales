% disco(artista, nombreDelDisco, cantidad, año).
disco(floydRosa, elLadoBrillanteDeLaLuna, 1000000, 1973).
disco(tablasDeCanada, autopistaTransargentina, 500, 2006).
disco(rodrigoMalo, elCaballo, 5000000, 1999).
disco(rodrigoMalo, loPeorDelAmor, 50000000, 1996).
disco(rodrigoMalo, loMejorDe, 50000000, 2018).
disco(losOportunistasDelConurbano, ginobili, 5, 2018).
disco(losOportunistasDelConurbano, messiMessiMessi, 5, 2018).
disco(losOportunistasDelConurbano, marthaArgerich, 15, 2019).
disco(coscu, marthaArgerich, 300, 2019).
disco(coscu, xd, 2, 2018).


%manager(artista, manager).
manager(floydRosa, normal(15)).
manager(tablasDeCanada, internacional(cachito, canada)).
manager(rodrigoMalo, estafador(tito)).

% normal(porcentajeComision) 
% internacional(nombre, lugar)
% estafador(nombre)  


% %%%%%%%%%%%%%
% %% Punto 1 %%
% %%%%%%%%%%%%%

clasico(Artista) :-
    disco(Artista, loMejorDe, _, _).

clasico(Artista) :-
    disco(Artista, _, CopiasVendidas, _),
    CopiasVendidas > 100000.

% %%%%%%%%%%%%%
% %% Punto 2 %%
% %%%%%%%%%%%%%

artista(Artista) :-
    disco(Artista, _ , _ , _).


cantidadesVendidas(Artista, Cantidad) :-
    artista(Artista),
    findall(UnidadVendida, disco(Artista, _, UnidadVendida, _), UnidadesVendidas),
    sumlist(UnidadesVendidas, Cantidad).


% %%%%%%%%%%%%%
% %% Punto 3 %%
% %%%%%%%%%%%%%

derechosDeAutor(Artista, ImporteTotal) :-
    artista(Artista),
    cantidadesVendidas(Artista, Cantidad),
    comisionDeVenta(Artista, Comision),
    ImporteTotal is Cantidad * (100 - Comision).

comisionDeVenta(Artista, Comision) :-
    manager(Artista, Manager),
    comisionDeManager(Manager, Comision).

comisionDeVenta(Artista, 0) :-
    sinManager(Artista).

comisionDeManager(normal(PorcentajeComision), Comision) :-
    comisionFinal(PorcentajeComision, Comision).

comisionDeManager(internacional(_, Lugar), Comision) :-
    porcentajeSegunLugar(Porcentaje, Lugar),
    comisionFinal(Porcentaje, Comision).

comisionDeManager(estafador(_), 100).

comisionFinal(Porcentaje, Comision) :-
    Comision is 100 * (Porcentaje / 100).

porcentajeSegunLugar(5, canada).
porcentajeSegunLugar(15, mexico).

sinManager(Artista) :-
    artista(Artista),
    not(manager(Artista, _)).

% %%%%%%%%%%%%%
% %% Punto 4 %%
% %%%%%%%%%%%%%

namberuan(Artista, Anio) :-
    sinManager(Artista),
    discoMasVendidoEn(Artista, Anio, Cantidad),
    forall(discoMasVendidoEn(_, Anio, OtraCantidad), Cantidad >= OtraCantidad).

discoMasVendidoEn(Artista, Anio, Cantidad) :-
    sinManager(Artista),
    disco(Artista, _ , Cantidad , Anio),
    forall(disco(Artista, _, OtraCantidad, Anio) , Cantidad >= OtraCantidad).

% %%%%%%%%%%%%%
% %% Punto 5 %%
% %%%%%%%%%%%%%

% Si se agrega un nuevo tipo de manager, ¿qué hay que hacer? ¿Qué concepto nos ayuda?

% Para agregar un nuevo tipo de manager, simplemente se utilizaria el predicado de manager(Artista, TipoDeManager)

% En caso de que exista un nuevo tipo de manager, simplemente se modificaria "comisionDeManager" agregando el nuevo caso

% Esto se da gracias al concepto de Polimorfismo.