es_chico([albert,paul,tom,derek,richard,louis,michael,charles,sam,steve,will,anthony,billy,henry]).
pelo_rubio([paul,michael,sam,will,anthony,billy,natalie,roxanne,emma]).
ropa_roja([albert,paul,richard,louis,sam,steve,anthony,henry,natalie,sarah,cindy]).
esta_triste([paul,derek,louis,sam,billy,henry,sarah]).
gafas([albert,michael,charles,anthony,natalie,sabrina]).
ojos_azules([albert,richard,louis,sam,will,billy,natalie,roxanne,sabrina]).

preguntas(['chico','chica','gafas','pelo_rubio','pelo_negro','feliz','triste','ropa_roja','ropa_verde','ojos_azules','ojos_marrones']).

personas([albert, paul, tom, derek, richard, louis, michael, charles, sam, steve, will, anthony, billy, henry, tiffany, natalie, roxanne, sarah, sabrina, cindy, emma]).
test([derek]).

%hacer_pregunta(A, Personaje, Lista_personajes):- A = 'chico', es_chico(Chicos), member(P,Chicos),descartarchicas.
%hacer_pregunta(A, P, L):- A = 'chico', .
hacer_pregunta(P, Sol):-P=Sol, write('Has acertado, mi personaje era '), writeln(Sol).
hacer_pregunta(P, Sol):-P = 'chico', writeln('personaje chico').
hacer_pregunta(P, Sol):-P = 'chica', writeln('personaje chica').
hacer_pregunta(P, Sol):-P = 'gafas', writeln('personaje gafas').
hacer_pregunta(P, Sol):-P = 'pelo_rubio', writeln('personaje pelo_rubio').
hacer_pregunta(P, Sol):-P = 'pelo_negro', writeln('personaje pelo_negro').
hacer_pregunta(P, Sol):-P = 'feliz', writeln('personaje feliz').
hacer_pregunta(P, Sol):-P = 'triste', writeln('personaje triste').
hacer_pregunta(P, Sol):-P = 'ropa_roja', writeln('personaje ropa_roja').
hacer_pregunta(P, Sol):-P = 'ropa_verde', writeln('personaje ropa_verde').
hacer_pregunta(P, Sol):-P = 'ojos_azules', writeln('personaje ojos_azules').
hacer_pregunta(P, Sol):-P = 'ojos_marrones', writeln('personaje ojos_marrones').
hacer_pregunta(P, Sol):- writeln('cualidad no reconocida :3').

leer_pregunta(X):- write('haz tu pregunta: '), read(X).
%pregunta optima: si tengo igual de una caracteristica q de la contraria, elijo esa, porque me divide en 2 el problema
jugar_maquina(Preg,NPreg,Personaje_jugador):- random_select(X,Preg,NPreg), hacer_pregunta(X,Personaje_jugador).

imprimir_candidatos([Candidato|Candidatos]):- 
    write(Candidato), write(': '),print_genero(Candidato),write(', '),print_pelo(Candidato),write(', '),
    print_estado(Candidato),write(', '), print_gafas(Candidato),write(', '),
    print_ojos(Candidato),writeln(''), imprimir_candidatos(Candidatos).
imprimir_candidatos([]):-write('').
print_genero(Candidato):- es_chico(Chicos) ,member(Candidato, Chicos), write('Chico');write('Chica').
print_pelo(Candidato):- pelo_rubio(Rubios), member(Candidato, Rubios), write('Pelo rubio');write('Pelo negro').
print_ropa(Candidato):- ropa_roja(Rojos), member(Candidato, Rojos), write('Ropa roja');write('Ropa verde').
print_estado(Candidato):-esta_triste(Tristes), member(Candidato, Tristes), write('triste');write('feliz').
print_gafas(Candidato):-gafas(Gafotas), member(Candidato, Gafotas), write('lleva gafas');write('sin gafas').
print_ojos(Candidato):-ojos_azules(OjosAzules), member(Candidato, OjosAzules), write('Ojos azules');write('Ojos marrones').


facil:-     
    %recuperar todas las acciones disponibles
    preguntas(Preguntas),
    personas(Personas),
    random_select(Personaje_maquina,Personas,Personas2),
    random_select(Personaje_jugador, Personas2, _),
    select(Personaje_maquina, Personas, Candidatos_maquina),
    select(Personaje_jugador, Personas, Candidatos_jugador),
    writeln(Candidatos_jugador),
    write('tu personaje es: '), writeln(Personaje_jugador),
    write('DEBUG!! El personaje de la maquina es: '), writeln(Personaje_maquina),
    %ejecutar consola de juego
    jugar(Preguntas, Candidatos_jugador, Candidatos_maquina, Personaje_jugador, Personaje_maquina).

jugar(Preguntas, Candidatos_jugador, Candidatos_maquina, Personaje_jugador, Personaje_maquina):-      
length(Candidatos_jugador, L),L=1,write('Tu ganas, mi personaje es '),writeln(Personaje_maquina);
length(Candidatos_maquina,L),L=1, write('Yo gano, tu personaje es '), writeln(Personaje_jugador).
jugar(Preguntas, Candidatos_jugador, Candidatos_maquina, Personaje_jugador, Personaje_maquina):-      
    writeln('-------------------------------------------------------------------'),
    writeln('Tus candidatos a elegir son:'),
    imprimir_candidatos(Candidatos_jugador),
    %ejecutar accion jugador
    write('Las preguntas que puedes hacer son: '),
    preguntas(L),
    writeln(L),
    leer_pregunta(X), hacer_pregunta(X,Personaje_maquina),
    %ejecutar accion maquina
    write('la maquina ha escogido: '), jugar_maquina(Preguntas,NPreg,Personaje_jugador), 
    jugar(NPreg, [Personaje_maquina], Candidatos_maquina, Personaje_jugador, Personaje_maquina).



