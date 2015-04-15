analizador:-
    consult(draw),
    consult(ioan_pecl2),
    writeln('NOTA: La frase tiene que estar encapsulada en comillas simples, y
        acabar en punto (despues de comilla simple)'),
    write('escriba su frase: '),
    read(X),
    interprete(X).
    %interprete.

interprete(I):-
    atomic_list_concat(L,' ',I),
    analizar(L),
    write('escriba su frase: '),
    read(I2),
    writeln(''),
    interprete(I2).