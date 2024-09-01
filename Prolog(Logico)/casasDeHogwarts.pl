% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%% Parte 1 %%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% sangre(Nombre, Sangre)

% caracteristica(Nombre, Personalidad)

% odia(Nombre, Casa)

sangre(harry, mestiza).
sangre(draco, pura).
sangre(hermione, impura).


caracteristica(harry, corajudo).
caracteristica(harry, amistoso).
caracteristica(harry, orgulloso).
caracteristica(harry, inteligente).
caracteristica(draco, inteligente).
caracteristica(draco, orgulloso).
caracteristica(hermione, inteligente).
caracteristica(hermione, orgulloso).
caracteristica(hermione, responsable).

odia(harry, slytherin).
odia(draco, hufflepuff).

casa(ravenclaw).
casa(slytherin).
casa(hufflepuff).
casa(gryffindor).

% requerimiento(Casa, Personalidad)
requerimiento(gryffindor, corajudo).
requerimiento(slytherin, orgulloso).
requerimiento(slytherin, inteligente).
requerimiento(ravenclaw, inteligente).
requerimiento(ravenclaw, responsable).
requerimiento(hufflepuff, amistoso).

% %%%%%%%%%%%%%
% %% Punto 1 %%
% %%%%%%%%%%%%%

mago(Mago) :-
    sangre(Mago, _).

permiteEntrar(Casa, Mago) :-
    casa(Casa),
    mago(Mago),
    Casa \= slytherin.

permiteEntrar(slytherin, Mago) :-
    sangre(Mago, Sangre),
    Sangre \= impura.

% %%%%%%%%%%%%%
% %% Punto 2 %%
% %%%%%%%%%%%%%

tieneCaracterApropiado(Mago, Casa) :-
    mago(Mago),
    casa(Casa),
    forall(requerimiento(Casa, Personalidad), caracteristica(Mago, Personalidad)).

% %%%%%%%%%%%%%
% %% Punto 3 %%
% %%%%%%%%%%%%%

enDondePuedeQuedar(Mago, Casa) :-
    permiteEntrar(Casa, Mago),
    tieneCaracterApropiado(Mago, Casa),
    not(odia(Mago, Casa)).

enDondePuedeQuedar(hermione, gryffindor).


% %%%%%%%%%%%%%
% %% Punto 4 %%
% %%%%%%%%%%%%%

% cadenaDeAmistades(Magos) Es con explosion combinatoria

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%% Parte 2 %%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%