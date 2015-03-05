es_chico([albert,paul,tom,derek,richard,louis,michael,charles,sam,steve,will,anthony,billy,henry]).
pelo_rubio([paul,michael,sam,will,anthony,billy,natalie,roxanne,emma]).
ropa_roja([albert,paul,richard,louis,sam,steve,anthony,henry,natalie,sarah,cindy]).
esta_triste([paul,derek,louis,sam,billy,henry,sarah]).
gafas([albert,michael,charles,anthony,natalie,sabrina]).
ojos_azules([albert,richard,louis,sam,will,billy,natalie,roxanne,sabrina]).

acciones_maquina(['chico','chica','gafas','pelo_rubio','pelo_negro','feliz','triste','ropa_roja','ropa_verde','ojos_azules','ojos_marrones']).

personas([albert, paul, tom, derek, richard, louis, michael, charles, sam, steve, will, anthony, billy, henry, tiffany, natalie, roxanne, sarah, sabrina, cindy, emma]).
test([derek]).

personaje_maquina([]).
personaje_jugador([]).

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
do_accion(A):- writeln('cualidad no reconocida :3').

leer_entrada(X):- write('escoge cualidades: '), read(X).

jugar_maquina(Acc,NAcc):- random_select(X,Acc,NAcc), do_accion(X).

facil:- 
    %recuperar lista de acciones disponibles para la maquina.
    acciones_maquina(L),
    %Pm -> personaje maquina, Lb-> lista inicial, La-> Lb-Pm
    personas(Lbm),
    random_select(Pm,Lbm,Lam),
    random_select(Pj, Lam, Laj),
    %ejecutar consola de juego
    jugar(L, Pm, Pj).

jugar(Acc, Pm, Pj):- write('tu personaje es: '), writeln(Pj), leer_entrada(X), do_accion(X),
    write('la maquina ha escogido: '), jugar_maquina(Acc,NAcc), jugar(NAcc, Pm, Pj).