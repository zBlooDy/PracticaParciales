comercioAdherido(iguazu, grandHotelIguazu).
comercioAdherido(iguazu, gargantaDelDiabloTour).
comercioAdherido(bariloche, aerolineas).
comercioAdherido(iguazu, aerolineas).

%factura(Persona, DetalleFactura).

%Detalles de facturas posibles:
% hotel(ComercioAdherido, ImportePagado)
% excursion(ComercioAdherido, ImportePagadoTotal, CantidadPersonas)
% vuelo(NroVuelo,NombreCompleto)
factura(estanislao, hotel(grandHotelIguazu, 2000)).
factura(antonieta, excursion(gargantaDelDiabloTour, 5000, 4)).
factura(antonieta, vuelo(1515, antonietaPerez)).


valorMaximoHotel(5000).

%registroVuelo(NroVuelo,Destino,ComercioAdherido,Pasajeros,Precio)
registroVuelo(1515, iguazu, aerolineas, [estanislaoGarcia, antonietaPerez, danielIto], 10000).


% %%%%%%%%%%%%%
% %% Punto 1 %%
% %%%%%%%%%%%%%

persona(Persona) :-
    factura(Persona, _).

montoADevolver(Persona, MontoFinal) :-
    persona(Persona),
    % montoDeCiudades(Persona, MontoCiudades),  Hay que usar una funcion que saque los repetidos ==> Algoritmico
    montosDeFacturas(Persona, MontoParcial),
    descuentoSiHayTrucha(Persona, MontoParcial, Monto),
    montoMaximoADevolver(Monto, MontoFinal).

montosDeFacturas(Persona, MontoParcial) :-
    findall(MontoFactura, montoDeFacturaValida(Persona, MontoFactura), MontosFacturas),
    sumlist(MontosFacturas, MontoParcial).

montoDeFacturaValida(Persona, MontoFactura) :-
    factura(Persona, Factura),
    esValida(Factura),
    montoSegunFactura(Factura, MontoFactura).

montoSegunFactura(hotel(_, Importe), MontoFactura) :-
    MontoFactura is Importe * 0.5.

montoSegunFactura(vuelo(NroVuelo, _), MontoFactura) :-
    registroVuelo(NroVuelo, Destino, _, _, Precio),
    montoRestringido(Destino, Precio, MontoFactura).

montoRestringido(buenosAires, _, 0).

montoRestringido(Destino, Precio, MontoFactura) :-
    MontoFactura is Precio * 0.3.

montoSegunFactura(excursion(_, Importe, Personas), MontoFactura) :-
    MontoFactura is (Importe/Personas) * 0.8.


esValida(Factura) :-
    tieneComercioAdherido(Factura),
    not(superaMontoHotel(Factura)),
    estaEnElVueloQuienPago(Factura).

tieneComercioAdherido(Factura) :-
    comercio(Factura, Comercio),
    comercioAdherido(_, Comercio).

comercio(hotel(Comercio, _), Comercio).
comercio(excursion(Comercio, _, _), Comercio).
comercio(vuelo(NroVuelo, _), Comercio) :-
    registroVuelo(NroVuelo, _, Comercio, _, _).

superaMontoHotel(hotel(_, Importe)) :-
    valorMaximoHotel(Valor),
    Importe > Valor.

estaEnElVueloQuienPago(vuelo(NroVuelo, Persona)) :-
    registroVuelo(NroVuelo, _, _, Pasajeros, _),
    member(Persona, Pasajeros).


descuentoSiHayTrucha(Persona, MontoParcial, MontoFinal) :-
    factura(Persona, Factura),
    not(esValida(Factura)),
    MontoFinal is MontoParcial - 15000.

descuentoSiHayTrucha(Persona, MontoParcial, MontoParcial) :-
    factura(Persona, Factura),
    esValida(Factura).

montoMaximoADevolver(MontoParcial, 10000) :-
    MontoParcial > 10000.

montoMaximoADevolver(MontoParcial, MontoParcial) :-
    MontoParcial =< 10000.

% %%%%%%%%%%%%%
% %% Punto 2 %%
% %%%%%%%%%%%%%

destinoDeTrabajo(Destino) :-
    registroVuelo(_, Destino, _, _, _),
    esDeTrabajo(Destino).

esDeTrabajo(Destino) :-
    registroVuelo(_, Destino, _, Pasajeros, _),
    member(Persona, Pasajeros),
    not((factura(Persona, hotel(Hotel, _)),
    comercioAdherido(Destino, Hotel))).

esDeTrabajo(Destino) :-
    comercioAdherido(Destino, Hotel),
    factura(_, hotel(Hotel, _)),
    not(factura(_, hotel(Hotel2, _))),
    Hotel \= Hotel2.

% %%%%%%%%%%%%%
% %% Punto 3 %%
% %%%%%%%%%%%%%

estafador(Persona) :-
    persona(Persona),
    forall(factura(Persona, Factura), facturaDeEstafador(Factura)).

facturaDeEstafador(Factura) :-
    not(esValida(Factura)).

facturaDeEstafador(Factura) :-
    montoSegunFactura(Factura, 0).

% %%%%%%%%%%%%%
% %% Punto 4 %%
% %%%%%%%%%%%%%

% Concepto de POLIMORFISMO