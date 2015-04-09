jugar:-
    consult('modo_facil.pl'),
    consult('modo_dificil.pl'),
    consult('utilidades.pl'),
    write('Elija modo de juego [facil|avanzado]: '),
    read(Modo),
    Modo = 'facil',
    facil;
    dificil.
