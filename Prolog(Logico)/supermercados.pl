%primeraMarca(Marca)
primeraMarca(laSerenisima).
primeraMarca(gallo).
primeraMarca(vienisima).

%precioUnitario(Producto,Precio)
%donde Producto puede ser arroz(Marca), lacteo(Marca,TipoDeLacteo), salchichas(Marca,Cantidad)
precioUnitario(arroz(gallo),25.10).
precioUnitario(lacteo(laSerenisima,leche), 6.00).
precioUnitario(lacteo(laSerenisima,crema), 4.00).
precioUnitario(lacteo(gandara,queso(gouda)), 13.00).
precioUnitario(lacteo(vacalin,queso(mozzarella)), 12.50).
precioUnitario(salchichas(vienisima,12), 9.80).
precioUnitario(salchichas(vienisima, 6), 5.80).
precioUnitario(salchichas(granjaDelSol, 8), 5.10).

%descuento(Producto, Descuento)
descuento(lacteo(laSerenisima,leche), 0.20).
descuento(lacteo(laSerenisima,crema), 0.70).
descuento(lacteo(gandara,queso(gouda)), 0.70).
descuento(lacteo(vacalin,queso(mozzarella)), 0.05).

% %%%%%%%%%%%%%
% %% Punto 1 %%
% %%%%%%%%%%%%%

descuento(arroz(Marca), 1.50) :-
    producto(arroz(Marca)).

descuento(salchichas(Marca, Cantidad), 0.50) :-
    producto(salchichas(Marca, Cantidad)),
    Marca \= vienisima.

descuento(lacteo(Marca, leche), 2) :-
    producto(lacteo(Marca, leche)).

descuento(lacteo(Marca, queso(Tipo)), 2) :-
    producto(lacteo(Marca, queso(Tipo))),
    primeraMarca(Marca).

descuento(Producto, Descuento) :-
    precioUnitario(Producto, PrecioMayor),
    forall(precioUnitario(_, OtroPrecio), PrecioMayor >= OtroPrecio),
    Descuento is PrecioMayor * 0.05.


%compro(Cliente,Producto,Cantidad)
compro(juan,lacteo(laSerenisima,crema),2).


% %%%%%%%%%%%%%
% %% Cont. 1 %%
% %%%%%%%%%%%%%

producto(Producto) :-
    precioUnitario(Producto, _).

% %%%%%%%%%%%%%
% %% Punto 2 %%
% %%%%%%%%%%%%%

% Interpreto que la consigna es que : TODOS los productos que compro UN cliente, sean de primera marca con descuento (Si es al reves basta con invertir el orden).

cliente(Cliente) :-
    compro(Cliente, _, _).

compradorCompulsivo(Cliente) :-
    cliente(Cliente),
    forall(compro(Cliente, Producto, _), primeraMarcaConDescuento(Producto)).

primeraMarcaConDescuento(Producto) :-
    descuento(Producto, _),
    esPrimeraMarca(Producto).

esPrimeraMarca(Producto) :-
    marcaProducto(Producto, Marca),
    primeraMarca(Marca).

marcaProducto(arroz(Marca), Marca).
marcaProducto(lacteo(Marca, _), Marca).
marcaProducto(salchichas(Marca, _), Marca).

% %%%%%%%%%%%%%
% %% Punto 3 %%
% %%%%%%%%%%%%%

totalAPagar(Cliente, Total) :-
    cliente(Cliente),
    findall(Precio, precioFinalCompra(Cliente, Precio), Precios),
    sumlist(Precios, Total).

precioFinalCompra(Cliente, Precio) :-
    compro(Cliente, Producto, Cantidad),
    precioProducto(Producto, PrecioProducto),
    Precio is Cantidad * PrecioProducto.


precioProducto(Producto, Precio) :-
    precioUnitario(Producto, PrecioUnidad),
    descuento(Producto, Descuento),
    Precio is PrecioUnidad - (Descuento * PrecioUnidad).

precioProducto(Producto, PrecioUnidad) :-
    precioUnitario(Producto, PrecioUnidad),
    not(descuento(Producto, _)).
