%en la kb solo se codifica media población. [Las chicas son (todas las personas) - (los chicos)] etc.
es_chico([albert,paul,tom,derek,richard,louis,michael,charles,sam,steve,will,anthony,billy,henry]).
pelo_rubio([paul,michael,sam,will,anthony,billy,natalie,roxanne,emma]).
ropa_roja([albert,paul,richard,louis,sam,steve,anthony,henry,natalie,sarah,cindy]).
esta_triste([paul,derek,louis,sam,billy,henry,sarah]).
gafas([albert,michael,charles,anthony,natalie,sabrina]).
ojos_azules([albert,richard,louis,sam,will,billy,natalie,roxanne,sabrina]).

preguntas(['chico','chica','gafas','pelo_rubio','pelo_negro','feliz','triste','ropa_roja','ropa_verde','ojos_azules','ojos_marrones']).

personas([albert, paul, tom, derek, richard, louis, michael, charles, sam, steve, will, anthony, billy, henry, tiffany, natalie, roxanne, sarah, sabrina, cindy, emma]).

hacer_pregunta(P,Candidatos,Sol,N_candidatos):-
    P = 'chico',es_chico(Chicos), member(Sol,Chicos), intersection(Candidatos,Chicos,N_candidatos),writeln('Si, es un chico!');
    P='chico',es_chico(Chicos),resta(Candidatos,Chicos,N_candidatos),writeln('No, no es un chico').

hacer_pregunta(P,Candidatos,Sol,N_candidatos):-
    %si la pregunta es chica? y la solución es un chica, nos quedamos con los candidatos que no sean chicos
    P = 'chica',es_chico(Chicos), not(member(Sol,Chicos)),resta(Candidatos,Chicos,N_candidatos),writeln('Si, es una chica!')
    %si la pregunta es 'chica?' y la solución es chico, nos quedamos con los candidatos que sean chicos
    ;P='chica',es_chico(Chicos),intersection(Candidatos,Chicos,N_candidatos), writeln('No es una chica').

hacer_pregunta(P,Candidatos,Sol,N_candidatos):-
    %si la pregunta es gafas y la solución tiene gafas, quedarse con los candidatos que tengan gafas
    P = 'gafas', gafas(Gafas), member(Sol,Gafas),intersection(Candidatos,Gafas,N_candidatos),writeln('Si, lleva gafas');
    %si la solución no tiene gafas, quedarse con los que no llevan gafas.
    P='gafas',gafas(Gafas),resta(Candidatos,Gafas,N_candidatos),writeln('No lleva gafas').

hacer_pregunta(P,Candidatos,Sol,N_candidatos):-
    %si la pregunta es pelo rubio, y el personaje es rubio
    P = 'pelo_rubio', pelo_rubio(Rubios), member(Sol,Rubios),intersection(Candidatos,Rubios,N_candidatos),writeln('si, es rubio'); 
    %si el personaje no es rubio
    P='pelo_rubio',pelo_rubio(Rubios),resta(Candidatos,Rubios,N_candidatos),writeln('no es rubio').

hacer_pregunta(P,Candidatos,Sol,N_candidatos):-
    %si la pregunta es pelo negro y el candidato es moreno, nos quedamos con los no rubios
    P = 'pelo_negro', pelo_rubio(Rubios), not(member(Sol,Rubios)),
    resta(Candidatos,Rubios,N_candidatos),writeln('si, es moreno');
    P='pelo_negro',pelo_rubio(Rubios),intersection(Candidatos,Rubios,N_candidatos),writeln('no es moreno').

hacer_pregunta(P,Candidatos,Sol,N_candidatos):-
    %si la pregunta es feliz y la solución esta triste, nos quedamos con los que estan tristes
    P = 'feliz', esta_triste(Tristes), member(Sol,Tristes),intersection(Candidatos,Tristes,N_candidatos),writeln('Esta feliz!!');
    %si esta feliz, nos quedamos con los que no estan tristes.
    P='feliz',esta_triste(Tristes),resta(Candidatos,Tristes,N_candidatos),writeln('No esta feliz D:').
hacer_pregunta(P,Candidatos,Sol,N_candidatos):-
    %si la pregunta es triste y la solución no está triste, nos quedamos con los que no estan tristes
    P = 'triste', esta_triste(Tristes), not(member(Sol,Tristes)),resta(Candidatos,Tristes,N_candidatos),writeln('esta feliz :D');
    %si la solución esta triste, nos quedamos con los tristes.
    P='triste',esta_triste(Tristes),intersection(Candidatos,Tristes,N_candidatos),writeln('esta triste D:').


hacer_pregunta(P,Candidatos,Sol,N_candidatos):-    
    P = 'ropa_roja', ropa_roja(RopaRoja), member(Sol,RopaRoja),intersection(Candidatos,RopaRoja,N_candidatos), writeln('Si, lleva la ropa roja');
    P = 'ropa_roja',ropa_roja(RopaRoja),resta(Candidatos,RopaRoja,N_candidatos),writeln('no, la ropa no es roja').

hacer_pregunta(P,Candidatos,Sol,N_candidatos):-
    P = 'ropa_verde', ropa_roja(RopaRoja), not(member(Sol,RopaRoja)),resta(Candidatos,RopaRoja,N_candidatos), writeln('si, la ropa es verde');
    P='ropa_verde',ropa_roja(RopaRoja),intersection(Candidatos,RopaRoja,N_candidatos),writeln('no, no es verde!').

hacer_pregunta(P,Candidatos,Sol,N_candidatos):-
    P ='ojos_azules', ojos_azules(OjosAzules),member(Sol,OjosAzules),intersection(Candidatos,OjosAzules,N_candidatos),writeln('si, sus ojos son azules');
    P='ojos_azules',ojos_azules(OjosAzules),resta(Candidatos,OjosAzules,N_candidatos),writeln('no, no son azules').

hacer_pregunta(P,Candidatos,Sol,N_candidatos):-
    P ='ojos_marrones', ojos_azules(OjosAzules),not(member(Sol,OjosAzules)),resta(Candidatos,OjosAzules,N_candidatos),writeln('Si, son marrones');
    P='ojos_marrones',ojos_azules(OjosAzules),intersection(Candidatos,OjosAzules,N_candidatos),writeln('No, no son marrones').

hacer_pregunta(P,Candidatos,Sol,N_candidatos):- writeln('pregunta no reconocida :3'), append(Candidatos,[],N_candidatos).

%caso base de resta
resta(L,[],Resultado):-append(L,[],Resultado).	
%resta de candidatos el segundo parametro, y la solucion la mete en resultado
resta(Candidatos,[X|Aeliminar],Resultado):-
	elimina(X,Candidatos,Resto_aux),
	resta(Resto_aux,Aeliminar,Resultado).

elimina(X,[X|T],T).
elimina(X,[],[]).
elimina(X,[H|T],[H|T1]):- elimina(X,T,T1).

leer_pregunta(X):- write('haz tu pregunta: '), read(X).

imprimir_candidatos([Candidato|Candidatos]):- 
    write(Candidato), write(': '),print_genero(Candidato),write(', '),print_pelo(Candidato),write(', '),
    print_estado(Candidato),write(', '), print_gafas(Candidato),write(', '),
    print_ojos(Candidato),write(', '),print_ropa(Candidato),writeln('') ,imprimir_candidatos(Candidatos).
imprimir_candidatos([]):-write('').
print_genero(Candidato):- es_chico(Chicos) ,member(Candidato, Chicos), write('Chico');write('Chica').
print_pelo(Candidato):- pelo_rubio(Rubios), member(Candidato, Rubios), write('Pelo rubio');write('Pelo negro').
print_ropa(Candidato):- ropa_roja(Rojos), member(Candidato, Rojos), write('Ropa roja');write('Ropa verde').
print_estado(Candidato):-esta_triste(Tristes), member(Candidato, Tristes), write('triste');write('feliz').
print_gafas(Candidato):-gafas(Gafas), member(Candidato, Gafas), write('lleva gafas');write('sin gafas').
print_ojos(Candidato):-ojos_azules(OjosAzules), member(Candidato, OjosAzules), write('Ojos azules');write('Ojos marrones').

calcular_posibilidades(N_Candidatos_maquina):-
    %Si en la lista solo hay 1 elemento, la maquina sabe quien eres
    length(N_Candidatos_maquina,N), N = 1, writeln('Ya se quien eres!');
    %si hay mas de 1, te informa de cuantos le quedan
    length(N_Candidatos_maquina,N), 
    write('mmm, dudo entre '),write(N),writeln(' posibilidades').

facil:-     
    %recuperar todas las acciones disponibles
    preguntas(Preguntas),
    personas(Personas),
    %asignar personaje para maquina y jugador
    random_select(Personaje_maquina,Personas,Personas2),
    random_select(Personaje_jugador, Personas2, _),
    %crear listas de candidatos para maquina y jugador
    select(Personaje_maquina, Personas, Candidatos_maquina),
    select(Personaje_jugador, Personas, Candidatos_jugador),
    write('tu personaje es: '), writeln(Personaje_jugador),
    write('DEBUG!! El personaje de la maquina es: '), writeln(Personaje_maquina),
    %ejecutar consola de juego
    jugar(Preguntas, Candidatos_jugador, Candidatos_maquina, Personaje_jugador, Personaje_maquina).

%casos base, si algunas de las listas de candidatos tienen solo 1 elemento, ese personaje ha ganado.
%como juega primero la persona, si en el mismo turno ambos ganan, se da preferencia al jugador. 
jugar(_, [Sol_jugador],_,_,_):- write('Tu ganas, mi personaje es '),writeln(Sol_jugador).
jugar(_, _, [Sol_maquina],_,_):- write('Yu gano, tu personaje es '),writeln(Sol_maquina).

%función recursiva para cada turno
jugar(Preguntas, Candidatos_jugador, Candidatos_maquina, Personaje_jugador, Personaje_maquina):-      
    writeln('-------------------------------------------------------------------'),
    writeln('Tus candidatos a elegir son:'),
    imprimir_candidatos(Candidatos_jugador),
    %ejecutar accion jugador
    write('Las preguntas que puedes hacer son: '),
    preguntas(L), writeln(L),
    leer_pregunta(P), hacer_pregunta(P,Candidatos_jugador,Personaje_maquina,N_candidatos_jugador),
    %turno maquina:
    write('Ahora me toca a mi, '),random_select(Preg,Preguntas,NPreg), write(Preg), writeln(' ?'),
    hacer_pregunta(Preg,Candidatos_maquina,Personaje_jugador,N_Candidatos_maquina),
    calcular_posibilidades(N_Candidatos_maquina),
    jugar(NPreg, N_candidatos_jugador, N_Candidatos_maquina, Personaje_jugador, Personaje_maquina).