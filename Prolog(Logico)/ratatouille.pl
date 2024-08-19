% rata(Nombre, LugarDondeVive).
rata(remy, gusteaus).
rata(emile, bar). 
rata(django, pizzeria).

% cocina(Nombre, PlatoQueCocina, Experiencia).
cocina(linguini, ratatouille, 3).
cocina(linguini, papasFritas, 5).
cocina(colette, salmonAsado, 9).
cocina(horst, ensaladaRusa, 8).
cocina(amelie, milanapotina, 10).
cocina(amelie, ensaladaAlbertEinstein, 10).

% trabajaEn(Restaurante, Persona).
trabajaEn(gusteaus, linguini). 
trabajaEn(gusteaus, colette). 
trabajaEn(gusteaus, skinner). 
trabajaEn(gusteaus, horst). 
trabajaEn(cafeDes2Moulins, amelie).


% %%%%%%%%%%%%%
% %% Punto 1 %%
% %%%%%%%%%%%%%

inspeccionSatisfactoria(Restaurante) :-
    trabajaEn(Restaurante, _),
    not(rata(_, Restaurante)).

% %%%%%%%%%%%%%
% %% Punto 2 %%
% %%%%%%%%%%%%%

chef(Persona, Restaurante) :-
    trabajaEn(Restaurante, Persona),
    cocina(Persona, _, _).

% %%%%%%%%%%%%%
% %% Punto 3 %%
% %%%%%%%%%%%%%

chefcito(Rata) :-
    rata(Rata, LugarDondeVive),
    cocina(linguini, LugarDondeVive, _).

% %%%%%%%%%%%%%
% %% Punto 4 %%
% %%%%%%%%%%%%%

cocinaBien(Persona, Plato) :-
    cocina(Persona, Plato, Experiencia),
    Experiencia > 7.

cocinaBien(remy, Plato) :-
    cocina(_, Plato, _).

% %%%%%%%%%%%%%
% %% Punto 5 %%
% %%%%%%%%%%%%%

encargadoDe(NombreDePlato, Restaurante, Encargado) :-
    cocinaPlatoEn(Encargado, NombreDePlato, Restaurante, Experiencia),
    forall(cocinaPlatoEn(_, NombreDePlato, Restaurante, OtraExperiencia), Experiencia >= OtraExperiencia).


cocinaPlatoEn(Persona, NombreDePlato, Restaurante, Experiencia) :-
    chef(Persona , Restaurante),
    cocina(Persona, NombreDePlato, Experiencia).


% %%%%%%%%%%%%%
% %% Punto 6 %%
% %%%%%%%%%%%%%

% plato(Nombre, Tipo())

% entrada(Ingredientes)
% principal(Guarnicion, MinutosDeCoccion)
% postre(Calorias)

plato(ensaladaAlbertEinstein, entrada([zanahoria])).
plato(ensaladaRusa, entrada([papa, zanahoria, arvejas, huevo, mayonesa])). 
plato(bifeDeChorizo, principal(pure, 25)).
plato(frutillasConCrema, postre(265)).
plato(milanapotina, principal(ensalada, 5)).

grupo(frutillasConCrema).

saludable(Plato) :-
    plato(Plato, TipoDePlato),
    caloriasSegun(TipoDePlato, Calorias),
    Calorias < 75.

saludable(Plato) :-
    plato(Plato, postre(_)),
    grupo(Plato).

caloriasSegun(entrada(Ingredientes), Calorias) :-
    length(Ingredientes, CantidadIngredientes),
    Calorias is CantidadIngredientes * 15.

caloriasSegun(principal(Guarnicion, TiempoCoccion), Calorias) :-
    caloriaDeGuarnicion(Guarnicion, CaloriasGuarnicion),
    Calorias is (5 * TiempoCoccion) + CaloriasGuarnicion.

caloriasSegun(postre(Calorias), Calorias).


caloriaDeGuarnicion(papasFritas, 50).
caloriaDeGuarnicion(pure, 20).
caloriaDeGuarnicion(ensalada, 0).


% %%%%%%%%%%%%%
% %% Punto 7 %%
% %%%%%%%%%%%%%

criticaPositiva(Restaurante, Critico) :-
    inspeccionSatisfactoria(Restaurante),
    criterioDe(Critico, Restaurante).

criterioDe(antonEgo, Restaurante) :-
    especialistaEn(ratatouille, Restaurante).

criterioDe(christophe, Restaurante) :-
    findall(Chef, chef(Chef, Restaurante), Chefs),
    length(Chefs, CantidadChefs),
    CantidadChefs > 3.

criterioDe(cormillot, Restaurante) :-
    restauranteSaludable(Restaurante).

restauranteSaludable(Restaurante) :-
    tieneChefs(Restaurante),
    forall(cocinaPlatoEn(_, NombreDePlato, Restaurante, _), saludable(NombreDePlato)),
    forall(ingredientesDeEntrada(Restaurante, Ingredientes), member(zanahoria, Ingredientes)).

ingredientesDeEntrada(Restaurante, Ingredientes) :-
    cocinaPlatoEn(_, NombreDePlato, Restaurante, _),
    plato(NombreDePlato, entrada(Ingredientes)).

especialistaEn(Plato, Restaurante) :-
    cocina(_, Plato, _),
    forall(trabajaEn(Restaurante, Persona), cocinaBien(Persona, Plato)).

tieneChefs(Restaurante) :-
    chef(_, Restaurante).