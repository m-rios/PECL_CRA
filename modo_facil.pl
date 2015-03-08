es_chico([albert,paul,tom,derek,richard,louis,michael,charles,sam,steve,will,anthony,billy,henry]).
pelo_rubio([paul,michael,sam,will,anthony,billy,natalie,roxanne,emma]).
ropa_roja([albert,paul,richard,louis,sam,steve,anthony,henry,natalie,sarah,cindy]).
esta_triste([paul,derek,louis,sam,billy,henry,sarah]).
gafas([albert,michael,charles,anthony,natalie,sabrina]).
ojos_azules([albert,richard,louis,sam,will,billy,natalie,roxanne,sabrina]).

preguntas(['chico','chica','gafas','pelo_rubio','pelo_negro','feliz','triste','ropa_roja','ropa_verde','ojos_azules','ojos_marrones']).

personas([albert, paul, tom, derek, richard, louis, michael, charles, sam, steve, will, anthony, billy, henry, tiffany, natalie, roxanne, sarah, sabrina, cindy, emma]).

%hacer_pregunta(A, Personaje, Lista_personajes):- A = 'chico', es_chico(Chicos), member(P,Chicos),descartarchicas.
%hacer_pregunta(A, P, L):- A = 'chico', .
hacer_pregunta(P,Candidatos,Sol,N_candidatos):-P=Sol, write('Has acertado, mi personaje era '), writeln(Sol).

hacer_pregunta(P,Candidatos,Sol,N_candidatos):-
    P = 'chico',es_chico(Chicos), member(Sol,Chicos), intersection(Candidatos,Chicos,N_candidatos);
    P='chico',es_chico(Chicos),resta(Candidatos,Chicos,N_candidatos).

hacer_pregunta(P,Candidatos,Sol,N_candidatos):-
    %si la pregunta es chica? y la solución es un chica, nos quedamos con los candidatos que no sean chicos
    P = 'chica',es_chico(Chicos), not(member(Sol,Chicos)),resta(Candidatos,Chicos,N_candidatos)
    %si la pregunta es 'chica?' y la solución es chico, nos quedamos con los candidatos que sean chicos
    ;P='chica',es_chico(Chicos), intersection(Candidatos,Chicos,N_candidatos).

hacer_pregunta(P,Candidatos,Sol,N_candidatos):-
    %si la pregunta es gafas y la solución tiene gafas, quedarse con los candidatos que tengan gafas
    P = 'gafas', gafas(Gafotas), member(Sol,Gafotas),intersection(Candidatos,Gafotas,N_candidatos);
    %si la solución no tiene gafas, quedarse con los que no llevan gafas.
    P='gafas',gafas(Gafotas),resta(Candidatos,Gafotas,N_candidatos).

hacer_pregunta(P,Candidatos,Sol,N_candidatos):-
    %si la pregunta es pelo rubio, y el personaje es rubio
    P = 'pelo_rubio', pelo_rubio(Rubios), member(Sol,Rubios),intersection(Candidatos,Rubios,N_candidatos); 
    %si el personaje no es rubio
    P='pelo_rubio',pelo_rubio(Rubios),personas(Personas),
    resta(Personas,Rubios,Morenos),resta(Candidatos,Rubios,N_candidatos).

hacer_pregunta(P,Candidatos,Sol,N_candidatos):-
    %si la pregunta es pelo negro y el candidato es moreno, nos quedamos con los no rubios
    P = 'pelo_negro', pelo_rubio(Rubios), not(member(Sol,Rubios)),resta(Candidatos,Rubios,N_candidatos);
    P='pelo_negro',pelo_rubio(Rubios), intersection(Candidatos,Rubios,N_candidatos).

hacer_pregunta(P,Candidatos,Sol,N_candidatos):-
    %si la pregunta es feliz y la solución esta triste, nos quedamos con los que estan tristes
    P = 'feliz', esta_triste(Tristes), member(Sol,Tristes), intersection(Candidatos,Tristes,N_candidatos);
    %si esta feliz, nos quedamos con los que no estan tristes.
    P='feliz',esta_triste(Tristes),resta(Candidatos,Tristes,N_candidatos).
hacer_pregunta(P,Candidatos,Sol,N_candidatos):-
    %si la pregunta es triste y la solución no está triste, nos quedamos con los que no estan tristes
    P = 'triste', esta_triste(Tristes), not(member(Sol,Tristes)),resta(Candidatos,Tristes,N_candidatos);
    %si la solución esta triste, nos quedamos con los tristes.
    P='triste',esta_triste(Tristes),intersection(Candidatos,Tristes,N_candidatos).


hacer_pregunta(P,Candidatos,Sol,N_candidatos):-    
    P = 'ropa_roja', ropa_roja(RopaRoja), member(Sol,RopaRoja), intersection(Candidatos,RopaRoja,N_candidatos);
    P = 'ropa_roja',ropa_roja(RopaRoja),resta(Candidatos,RopaRoja,N_candidatos).

hacer_pregunta(P,Candidatos,Sol,N_candidatos):-
    P = 'ropa_verde', ropa_roja(RopaRoja), member(Sol,RopaRoja), resta(Candidatos,RopaRoja,N_candidatos);
    P='ropa_verde',ropa_roja(RopaRoja),intersection(Candidatos,RopaRoja,N_candidatos).

hacer_pregunta(P,Candidatos,Sol,N_candidatos):-
    P ='ojos_azules', ojos_azules(OjosAzules),member(Sol,OjosAzules),intersection(Candidatos,OjosAzules,N_candidatos);
    P='ojos_azules',ojos_azules(OjosAzules),resta(Candidatos,OjosAzules,N_candidatos).

hacer_pregunta(P,Candidatos,Sol,N_candidatos):-
    P ='ojos_verdes', ojos_azules(OjosAzules),member(Sol,OjosAzules),resta(Candidatos,OjosAzules,N_candidatos);
    P='ojos_verdes',ojos_azules(OjosAzules),intersection(Candidatos,OjosAzules,N_candidatos).

hacer_pregunta(P,Candidatos,Sol,N_candidatos):- writeln('cualidad no reconocida :3').

resta(L,[],Resultado):-append(L,[],Resultado).	
resta(Candidatos,[X|Aeliminar],Resultado):-
	elimina(X,Candidatos,Resto_aux),
	resta(Resto_aux,Aeliminar,Resultado).

elimina(X,[X|T],T).
elimina(X,[],[]).
elimina(X,[H|T],[H|T1]):- elimina(X,T,T1).

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

%descartar(Candidatos, Descartados):-.fa



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
    jugar(Preguntas, Candidatos_jugador, Candidatos_maquina, Personaje_jugador, emma).

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
    leer_pregunta(X), hacer_pregunta(X,Candidatos_jugador,Personaje_maquina,N_candidatos_jugador),
    %ejecutar accion maquina
    %write('la maquina ha escogido: '), 
    %random_select(X,Preguntas,NPreg),
    %hacer_pregunta(X,Candidatos_maquina,Personaje_jugador,N_candidatos_maquina),
    jugar(NPreg, N_candidatos_jugador, Candidatos_maquina, Personaje_jugador, Personaje_maquina).



