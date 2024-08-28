% integrante(Grupo, Persona, InstrumentoQueToca)

integrante(sophieTrio, sophie, violin).
integrante(sophieTrio, santi, guitarra).
integrante(vientosDelEste, lisa, saxo).
integrante(vientosDelEste, santi, voz).
integrante(vientosDelEste, santi, guitarra).
integrante(jazzmin, santi, bateria).

% nivelQueTiene(Persona, Instrumento, Nivel)

nivelQueTiene(sophie, violin, 5).
nivelQueTiene(santi, guitarra, 2).
nivelQueTiene(santi, voz, 3).
nivelQueTiene(santi, bateria, 4).
nivelQueTiene(lisa, saxo, 4).
nivelQueTiene(lore, violin, 4).
nivelQueTiene(luis, trompeta, 1).
nivelQueTiene(luis, contrabajo, 4).


% instrumento(Instrumento, RolQueCumple())
instrumento(violin, melodico(cuerdas)).
instrumento(guitarra, armonico).
instrumento(bateria, ritmico).
instrumento(saxo, melodico(viento)).
instrumento(trompeta, melodico(viento)).
instrumento(contrabajo, armonico).
instrumento(bajo, armonico).
instrumento(piano, armonico).
instrumento(pandereta, ritmico).
instrumento(voz, melodico(vocal)).

% %%%%%%%%%%%%%
% %% Punto 1 %%
% %%%%%%%%%%%%%

tieneBuenaBase(Grupo) :-
    integrante(Grupo, Persona, Instrumento),
    instrumento(Instrumento, ritmico),
    integrante(Grupo, OtraPersona, OtroInstrumento),
    instrumento(OtroInstrumento, armonico),
    Persona \= OtraPersona.

% %%%%%%%%%%%%%
% %% Punto 2 %%
% %%%%%%%%%%%%%

destaca(Persona, Grupo) :-
    integrante(Grupo, Persona, Instrumento),
    nivelQueTiene(Persona, Instrumento, Nivel),
    forall((integrante(Grupo, OtraPersona, OtroInstrumento), Persona \= OtraPersona), mayorNivel(Nivel, OtraPersona, OtroInstrumento)).

mayorNivel(Nivel, Persona, Instrumento) :-
    nivelQueTiene(Persona, Instrumento, OtroNivel),
    Diferencia is Nivel - OtroNivel,
    Diferencia >= 2.


% %%%%%%%%%%%%%
% %% Punto 3 %%
% %%%%%%%%%%%%%

% grupo (Nombre, tipo())

% Punto 8 tambien aca

% 

grupo(vientosDelEste, tipo(bigband)).
grupo(sophieTrio, tipo(formacionParticular, [contrabajo, guitarra, violin])).
grupo(jazzmin, tipo(formacionParticular, [bateria, bajo, trompeta, piano, guitarra])).
grupo(estudio1, tipo(ensamble, 3)).

% %%%%%%%%%%%%%
% %% Punto 4 %%
% %%%%%%%%%%%%%

hayCupo(Instrumento, Grupo) :-
    grupo(Grupo, tipo(bigband)),
    instrumento(Instrumento, melodico(viento)).

hayCupo(Instrumento, Grupo) :-
    grupo(Grupo, TipoDeGrupo),
    nadieToca(Instrumento, Grupo),
    sirveParaElGrupo(Instrumento, TipoDeGrupo).

nadieToca(Instrumento, Grupo) :-
    instrumento(Instrumento, _),
    not(integrante(Grupo, _, Instrumento)).

sirveParaElGrupo(Instrumento, tipo(formacionParticular, Instrumentos)) :-
    member(Instrumento, Instrumentos).

sirveParaElGrupo(bateria, tipo(bigband)).
sirveParaElGrupo(bajo, tipo(bigband)).
sirveParaElGrupo(piano, tipo(bigband)).
sirveParaElGrupo(_, tipo(ensamble, _)).


% %%%%%%%%%%%%%
% %% Punto 5 %%
% %%%%%%%%%%%%%

puedeIncorporarse(Persona, Grupo, Instrumento) :-
    nivelQueTiene(Persona, Instrumento, Nivel),
    grupo(Grupo, TipoDeGrupo),
    not(integrante(Grupo, Persona, _)),
    hayCupo(Instrumento, Grupo),
    superaNivelEsperado(Nivel, TipoDeGrupo).

superaNivelEsperado(Nivel, TipoDeGrupo) :-
    nivelEsperado(TipoDeGrupo, NivelEsperado),
    Nivel >= NivelEsperado.

nivelEsperado(tipo(bigband), 1).
nivelEsperado(tipo(formacionParticular, Instrumentos), NivelEsperado) :-
    length(Instrumentos, CantidadInstrumentos),
    NivelEsperado is 7 - CantidadInstrumentos.
nivelEsperado(tipo(ensamble, NivelEsperado), NivelEsperado).

% %%%%%%%%%%%%%
% %% Punto 6 %%
% %%%%%%%%%%%%%

seQuedoEnBanda(Persona) :-
    nivelQueTiene(Persona, _, _),
    not(integrante(_, Persona, _)),
    not(puedeIncorporarse(Persona, _, _)).

% %%%%%%%%%%%%%
% %% Punto 7 %%
% %%%%%%%%%%%%%

puedeTocar(Grupo) :-
    grupo(Grupo, TipoDeGrupo),
    cubrenRequisitos(Grupo, TipoDeGrupo).

cubrenRequisitos(Grupo, tipo(bigband)) :-
    tieneBuenaBase(Grupo),
    cantidadQueToca(Grupo, melodico(viento), Cantidad),
    Cantidad >= 5.

cubrenRequisitos(Grupo, tipo(formacionParticular, Instrumentos)) :-
    forall(member(Instrumento, Instrumentos), integrante(Grupo, _, Instrumento)).

% Punto 8
cubrenRequisitos(Grupo, tipo(ensamble, _)) :-
    tieneBuenaBase(Grupo),
    integrante(Grupo, _, Instrumento),
    instrumento(Instrumento, melodico(_)).

cantidadQueToca(Grupo, TipoDeInstrumento, Cantidad) :-
    findall(Persona, (integrante(Grupo, Persona, Instrumento), instrumento(Instrumento, TipoDeInstrumento)), Personas),
    length(Personas, Cantidad).

