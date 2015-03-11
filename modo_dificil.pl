es_chico([albert,paul,tom,derek,richard,louis,michael,charles,sam,steve,will,anthony,billy,henry]).
pelo_rubio([paul,michael,sam,will,anthony,billy,natalie,roxanne,emma]).
ropa_roja([albert,paul,richard,louis,sam,steve,anthony,henry,natalie,sarah,cindy]).
esta_triste([paul,derek,louis,sam,billy,henry,sarah]).
gafas([albert,michael,charles,anthony,natalie,sabrina]).
ojos_azules([albert,richard,louis,sam,will,billy,natalie,roxanne,sabrina]).

putos([albert,charles,michael,sam,billy,henry]).


preguntas(['chico','chica','gafas','pelo_rubio','pelo_negro','feliz','triste','ropa_roja','ropa_verde','ojos_azules','ojos_marrones']).

personas([albert, paul, tom, derek, richard, louis, michael, charles, sam, steve, will, anthony, billy, henry, tiffany, natalie, roxanne, sarah, sabrina, cindy, emma]).

hacer_pregunta(P,Candidatos,Sol,N_candidatos):-P=Sol, write('Has acertado, mi personaje era '), writeln(Sol).

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
    P = 'gafas', gafas(Gafotas), member(Sol,Gafotas),intersection(Candidatos,Gafotas,N_candidatos),writeln('Si, lleva gafas');
    %si la solución no tiene gafas, quedarse con los que no llevan gafas.
    P='gafas',gafas(Gafotas),resta(Candidatos,Gafotas,N_candidatos),writeln('No lleva gafas').

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

resta(L,[],Resultado):-append(L,[],Resultado).	
resta(Candidatos,[X|Aeliminar],Resultado):-
	elimina(X,Candidatos,Resto_aux),
	resta(Resto_aux,Aeliminar,Resultado).

elimina(X,[X|T],T).
elimina(X,[],[]).
elimina(X,[H|T],[H|T1]):- elimina(X,T,T1).

leer_pregunta(X):- write('haz tu pregunta: '), read(X).
%pregunta optima: si tengo igual de una caracteristica q de la contraria, elijo esa, porque me divide en 2 el problema
jugar_maquina(Preg,NPreg,Candidatos_maquina,Personaje_jugador,N_candidatos_maquina):- 
    write('Ahora me toca a mi, '),random_select(P,Preg,NPreg), write(P), write(' ?'),
    hacer_pregunta(P,Candidatos_maquina,Personaje_jugador,N_candidatos_maquina).

imprimir_candidatos([Candidato|Candidatos]):- 
    write(Candidato), write(': '),print_genero(Candidato),write(', '),print_pelo(Candidato),write(', '),
    print_estado(Candidato),write(', '), print_gafas(Candidato),write(', '),
    print_ojos(Candidato),write(', '),print_ropa(Candidato),writeln('') ,imprimir_candidatos(Candidatos).
imprimir_candidatos([]):-write('').
print_genero(Candidato):- es_chico(Chicos) ,member(Candidato, Chicos), write('Chico');write('Chica').
print_pelo(Candidato):- pelo_rubio(Rubios), member(Candidato, Rubios), write('Pelo rubio');write('Pelo negro').
print_ropa(Candidato):- ropa_roja(Rojos), member(Candidato, Rojos), write('Ropa roja');write('Ropa verde').
print_estado(Candidato):-esta_triste(Tristes), member(Candidato, Tristes), write('triste');write('feliz').
print_gafas(Candidato):-gafas(Gafotas), member(Candidato, Gafotas), write('lleva gafas');write('sin gafas').
print_ojos(Candidato):-ojos_azules(OjosAzules), member(Candidato, OjosAzules), write('Ojos azules');write('Ojos marrones').

calcular_posibilidades(N_Candidatos_maquina):-
    length(N_Candidatos_maquina,N), N = 1, writeln('Ya se quien eres!');
    length(N_Candidatos_maquina,N), 
    write('mmm, dudo entre '),write(N),writeln(' posibilidades').


relacion(P,Candidatos,Relacion):-
    P = 'chico',es_chico(Chicos), intersection(Candidatos,Chicos,N_candidatos1), longitud(N_candidatos1,N1),
    resta(Candidatos,Chicos,N_candidatos2), longitud(N_candidatos2, N2),
    Relacion is abs(N1-N2)
    ;
    P='chica',es_chico(Chicos), resta(Candidatos,Chicos,N_candidatos1),longitud(N_candidatos1,N1),
    intersection(Candidatos,Chicos,N_candidatos2), longitud(N_candidatos2,N2),
    Relacion is abs(N1-N2).

relacion(P,Candidatos,Relacion):-
    P = 'gafas', gafas(Gafotas), 
    intersection(Candidatos,Gafotas,N1_candidatos), longitud(N1_candidatos,N1), 
    resta(Candidatos,Gafotas,N2_candidatos),longitud(N2_candidatos,N2),
    Relacion is abs(N1-N2).
   

relacion(P,Candidatos,Relacion):-
    P = 'pelo_rubio',pelo_rubio(Rubios), intersection(Candidatos,Rubios,N_candidatos1), longitud(N_candidatos1,N1),
    resta(Candidatos,Rubios,N_candidatos2),longitud(N_candidatos2,N2),
    Relacion is abs(N1 - N2)
    ;
    P = 'pelo_negro',pelo_rubio(Rubios), resta(Candidatos,Rubios,N_candidatos1),longitud(N_candidatos1,N1),
    intersection(Candidatos,Rubios,N_candidatos2), longitud(N_candidatos2,N2),
    Relacion is abs(N1 - N2).

relacion(P,Candidatos,Relacion):-
    P = 'triste', esta_triste(Tristes), intersection(Candidatos,Tristes,N_candidatos1), longitud(N_candidatos1,N1),
    resta(Candidatos,Tristes,N_candidatos2), longitud(N_candidatos2,N2),
    Relacion is abs(N1-N2)
    ;
    P = 'feliz',esta_triste(Tristes), resta(Candidatos,Tristes,N_candidatos1), longitud(N_candidatos1,N1),
    intersection(Candidatos,Tristes,N_candidatos2), longitud(N_candidatos2,N2),
    Relacion is abs(N1-N2).

relacion(P,Candidatos,Relacion):-
    P = 'ropa_roja', ropa_roja(RopaRoja), intersection(Candidatos,RopaRoja,N_candidatos1), longitud(N_candidatos1,N1),
    resta(Candidatos,RopaRoja,N_candidatos2), longitud(N_candidatos2,N2),
    Relacion is abs(N1-N2)
    ;
    P = 'ropa_verde',ropa_roja(RopaRoja), resta(Candidatos,RopaRoja,N_candidatos1), longitud(N_candidatos1,N1),
    intersection(Candidatos,RopaRoja,N_candidatos2), longitud(N_candidatos2,N2),
    Relacion is abs(N1-N2).

relacion(P,Candidatos,Relacion):-
    P = 'ojos_azules', ojos_azules(OjosAzules), intersection(Candidatos,OjosAzules,N_candidatos1), longitud(N_candidatos1,N1),
    resta(Candidatos,OjosAzules,N_candidatos2), longitud(N_candidatos2,N2),
    Relacion is abs(N1-N2)
    ;
    P = 'ojos_marrones',ojos_azules(OjosAzules), resta(Candidatos,OjosAzules,N_candidatos1), longitud(N_candidatos1,N1),
    intersection(Candidatos,OjosAzules,N_candidatos2), longitud(N_candidatos2,N2),
    Relacion is abs(N1-N2).


elegir(Candidatos,[], Siguiente, RelacionSiguiente, Optima, RelacionOptima,Siguiente):-
    RelacionSiguiente =< RelacionOptima.
elegir(Candidatos,[], Siguiente, RelacionSiguiente, Optima, RelacionOptima,Optima):-
    RelacionSiguiente > RelacionOptima.

elegir(Candidatos,Preguntas, Siguiente, RelacionSiguiente, Optima, RelacionOptima,Solucion):-
    RelacionSiguiente =< RelacionOptima, get(Preguntas,Primera,Resto),relacion(Primera,Candidatos,RelacionPrimera) ,elegir(Candidatos,Resto,Primera,RelacionPrimera, Siguiente,RelacionSiguiente,Solucion).
elegir(Candidatos,Preguntas, Siguiente, RelacionSiguiente, Optima, RelacionOptima,Solucion):-
    RelacionSiguiente > RelacionOptima,  get(Preguntas,Primera,Resto),relacion(Primera,Candidatos,RelacionPrimera) ,elegir(Candidatos,Resto,Primera,RelacionPrimera, Optima,RelacionOptima,Solucion).
count(_,[],N):- N = 0.
count(A,[A|L],N):- count(A,L,N1),N is N1+1.
count(A,[_|L],N):- count(A,L,N). 
longitud([X|Y],N):- 
    longitud(Y,N1),
    N is N1 + 1.
longitud([],N):- N is 0.
swap(X,L,L,X).
get([X|L],X,L).

dificil:-     
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

jugar(Preguntas, [C1|Candidatos_jugador], [C2|Candidatos_maquina], Personaje_jugador, Personaje_maquina):-      
    length(Candidatos_jugador, L),L=0,write('Tu ganas, mi personaje es '),writeln(C1);
    length(Candidatos_maquina,L),L=0, write('Yo gano, tu personaje es '),writeln(C2).
jugar(Preguntas, Candidatos_jugador, Candidatos_maquina, Personaje_jugador, Personaje_maquina):-      
    writeln('-------------------------------------------------------------------'),
    writeln('Tus candidatos a elegir son:'),
    imprimir_candidatos(Candidatos_jugador),
    %ejecutar accion jugador
    write('Las preguntas que puedes hacer son: '),
    preguntas(L), writeln(L),
    leer_pregunta(P), hacer_pregunta(P,Candidatos_jugador,Personaje_maquina,N_candidatos_jugador),
    %turno maquina:
    %jugar_maquina(Preguntas,NPreg,Candidatos_maquina,Personaje_jugador,N_candidatos_maquina),
    write('Ahora me toca a mi, '),
    get(Preguntas,Primera,RestoPreguntas),
    relacion(Primera,Candidatos_maquina,RelacionPrimera),
    elegir(Candidatos_maquina,Preguntas,Primera,RelacionPrimera,Primera,RelacionPrimera,Preg),
    select(Preg,Preguntas,NPreg), write(Preg), writeln(' ?'),

    hacer_pregunta(Preg,Candidatos_maquina,Personaje_jugador,N_Candidatos_maquina),
    calcular_posibilidades(N_Candidatos_maquina),
    jugar(NPreg, N_candidatos_jugador, N_Candidatos_maquina, Personaje_jugador, Personaje_maquina).