es_chico([albert,paul,tom,derek,richard,louis,michael,charles,sam,steve,will,anthony,billy,henry]).
pelo_rubio([paul,michael,sam,will,anthony,billy,natalie,roxanne,emma]).
ropa_roja([albert,paul,richard,louis,sam,steve,anthony,henry,natalie,sarah,cindy]).
esta_triste([paul,derek,louis,sam,billy,henry,sarah]).
gafas([albert,michael,charles,anthony,natalie,sabrina]).
ojos_azules([albert,richard,louis,sam,will,billy,natalie,roxanne,sabrina]).

acciones_maquina(['chico','chica','gafas','pelo_rubio','pelo_negro','feliz','triste','ropa_roja','ropa_verde','ojos_azules','ojos_marrones']).

personas([albert, paul, tom, derek, richard, louis, michael, charles, sam, steve, will, anthony, billy, henry, tiffany, natalie, roxanne, sarah, sabrina, cindy, emma]).
personas_maquina([]).
personas_jugador([]).
test([derek]).

personaje_maquina([]).
personaje_jugador([]).

asignar_maquina:- personas(L),random_select(X,L,L1), personaje_maquina(M), 
    append(M,X), personas_maquina(M2), append(M2,L1).
asignar_jugador:- personas(L), random_select(X,L,L1), personaje_jugador(J), append(J,X).

do_accion(A):- A = 'chico', writeln('personaje chico').
do_accion(A):-A = 'chica', writeln('personaje chica').
do_accion(A):-A = 'gafas', writeln('personaje gafas').
do_accion(A):-A = 'pelo_rubio', writeln('personaje pelo_rubio').
do_accion(A):-A = 'pelo_negro', writeln('personaje pelo_negro').
do_accion(A):-A = 'feliz', writeln('personaje feliz').
do_accion(A):-A = 'triste', writeln('personaje triste').
do_accion(A):-A = 'ropa_roja', writeln('personaje ropa_roja').
do_accion(A):-A = 'ropa_verde', writeln('personaje ropa_verde').
do_accion(A):-A = 'ojos_azules', writeln('personaje ojos_azules').
do_accion(A):-A = 'ojos_marrones', writeln('personaje ojos_marrones').
do_accion(A):- A = 'salir', writeln('saliendo').
do_accion(A):- writeln('cualidad no reconocida :3').

leer_entrada(X):- write('escoge cualidades: '), read(X).

jugar_maquina(Acc,NAcc):- random_select(X,Acc,NAcc), do_accion(X).

jugar:- acciones_maquina(L), jugar(L).

jugar(Acc):- leer_entrada(X), do_accion(X), X\='salir' ,write('la maquina ha escogido: '), jugar_maquina(Acc,NAcc), jugar(NAcc).