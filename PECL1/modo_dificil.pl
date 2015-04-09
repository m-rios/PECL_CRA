%El metodo relación devuelve la relacion entre las respuestas negativa y las positivas de una pregunta
%   Primero se obtienen el numero de respuestas negativas y el numero de respuestas negativas.
%   El resultado será el valor absoluto de esa resta ya que nos facilitará el proceso de busqueda de mejor relacion,
%   ya la mejor solución siempre sera la menor (es decir, 0)

% Chico y chica 
relacion(P,Candidatos,Relacion):-
    P = 'chico',es_chico(Chicos), intersection(Candidatos,Chicos,N_candidatos1), longitud(N_candidatos1,N1),
    resta(Candidatos,Chicos,N_candidatos2), longitud(N_candidatos2, N2),
    Relacion is abs(N1-N2)
    ;
    P='chica',es_chico(Chicos), resta(Candidatos,Chicos,N_candidatos1),longitud(N_candidatos1,N1),
    intersection(Candidatos,Chicos,N_candidatos2), longitud(N_candidatos2,N2),
    Relacion is abs(N1-N2).

%Gafas 
relacion(P,Candidatos,Relacion):-
    P = 'gafas', gafas(Gafas), 
    intersection(Candidatos,Gafas,N1_candidatos), longitud(N1_candidatos,N1), 
    resta(Candidatos,Gafas,N2_candidatos),longitud(N2_candidatos,N2),
    Relacion is abs(N1-N2).
   
%Pelo rubio y pelo negro 
relacion(P,Candidatos,Relacion):-
    P = 'pelo_rubio',pelo_rubio(Rubios), intersection(Candidatos,Rubios,N_candidatos1), longitud(N_candidatos1,N1),
    resta(Candidatos,Rubios,N_candidatos2),longitud(N_candidatos2,N2),
    Relacion is abs(N1 - N2)
    ;
    P = 'pelo_negro',pelo_rubio(Rubios), resta(Candidatos,Rubios,N_candidatos1),longitud(N_candidatos1,N1),
    intersection(Candidatos,Rubios,N_candidatos2), longitud(N_candidatos2,N2),
    Relacion is abs(N1 - N2).

%Triste y feliz
relacion(P,Candidatos,Relacion):-
    P = 'triste', esta_triste(Tristes), intersection(Candidatos,Tristes,N_candidatos1), longitud(N_candidatos1,N1),
    resta(Candidatos,Tristes,N_candidatos2), longitud(N_candidatos2,N2),
    Relacion is abs(N1-N2)
    ;
    P = 'feliz',esta_triste(Tristes), resta(Candidatos,Tristes,N_candidatos1), longitud(N_candidatos1,N1),
    intersection(Candidatos,Tristes,N_candidatos2), longitud(N_candidatos2,N2),
    Relacion is abs(N1-N2).

%Ropa roja y ropa verde
relacion(P,Candidatos,Relacion):-
    P = 'ropa_roja', ropa_roja(RopaRoja), intersection(Candidatos,RopaRoja,N_candidatos1), longitud(N_candidatos1,N1),
    resta(Candidatos,RopaRoja,N_candidatos2), longitud(N_candidatos2,N2),
    Relacion is abs(N1-N2)
    ;
    P = 'ropa_verde',ropa_roja(RopaRoja), resta(Candidatos,RopaRoja,N_candidatos1), longitud(N_candidatos1,N1),
    intersection(Candidatos,RopaRoja,N_candidatos2), longitud(N_candidatos2,N2),
    Relacion is abs(N1-N2).

%Ojos azules y Ojos marrones
relacion(P,Candidatos,Relacion):-
    P = 'ojos_azules', ojos_azules(OjosAzules), intersection(Candidatos,OjosAzules,N_candidatos1), longitud(N_candidatos1,N1),
    resta(Candidatos,OjosAzules,N_candidatos2), longitud(N_candidatos2,N2),
    Relacion is abs(N1-N2)
    ;
    P = 'ojos_marrones',ojos_azules(OjosAzules), resta(Candidatos,OjosAzules,N_candidatos1), longitud(N_candidatos1,N1),
    intersection(Candidatos,OjosAzules,N_candidatos2), longitud(N_candidatos2,N2),
    Relacion is abs(N1-N2).



% elegir() escoge entre todos los candidatos pasados por parametro, el que mejor relacion
%   entre resupuestas positivas y negativas tiene.
%   Los parámetros son:
%       - Candidatos. Necesarios para averiguar la relacion de las preguntas.
%       - Preguntas a hacer.
%       - Siguiente y RelacionSiguiente. La pregunta y la relacion de la pregunta a evaluar
%       - Optima y RelacionOptima. Pregunta más optima y su relacion
%       - Solucion. En la primera llamada, la más optima es la primera pregunta.
%               A lo largo de las llamadas recursivas se va arrastrando la mejor.

%Elección de la solucion en el caso base
%   Si la relacion de la siguiente es la más optima, se escoje la siguiente,
%   En caso contrario, se escoje la optima.
elegir(_,[], Siguiente, RelacionSiguiente, _, RelacionOptima,Siguiente):-
    RelacionSiguiente =< RelacionOptima.
elegir(_,[], _, RelacionSiguiente, Optima, RelacionOptima,Optima):-
    RelacionSiguiente > RelacionOptima.

%Elección de la solucion para los casos generales.
%   Si la relación se la siguiente 
elegir(Candidatos,Preguntas, Siguiente, RelacionSiguiente, _, RelacionOptima,Solucion):-
    RelacionSiguiente =< RelacionOptima, get(Preguntas,Primera,Resto),relacion(Primera,Candidatos,RelacionPrimera) ,elegir(Candidatos,Resto,Primera,RelacionPrimera, Siguiente,RelacionSiguiente,Solucion).
elegir(Candidatos,Preguntas, _, RelacionSiguiente, Optima, RelacionOptima,Solucion):-
    RelacionSiguiente > RelacionOptima,  get(Preguntas,Primera,Resto),relacion(Primera,Candidatos,RelacionPrimera) ,elegir(Candidatos,Resto,Primera,RelacionPrimera, Optima,RelacionOptima,Solucion).

%longitud de una lista
longitud([_|Y],N):- 
    longitud(Y,N1),
    N is N1 + 1.
longitud([],N):- N is 0.

%El proposito de este método es de devolver el primer elementos de una lista
%   y el resto
get([X|L],X,L).

%Procedimiento principal del programa
dificil:-     
    %recuperar todas las acciones disponibles
    preguntas(Preguntas),
    personas(Personas),
    random_select(Personaje_maquina,Personas,Personas2),
    random_select(Personaje_jugador, Personas2, _),

    %Selecciona los personajes y escribe el elegido para el judador
    select(Personaje_maquina, Personas, Candidatos_maquina),
    select(Personaje_jugador, Personas, Candidatos_jugador),
    writeln(Candidatos_jugador),
    write('tu personaje es: '), writeln(Personaje_jugador), 
    %ejecutar consola de juego

    turno_dificil(Preguntas, Candidatos_jugador, Candidatos_maquina, Personaje_jugador, Personaje_maquina).

turno_dificil(_, [C1],_,_,_):- write('Tu ganas, mi personaje es '),writeln(C1).
turno_dificil(_, _, [C2],_,_):- write('Yo gano, tu personaje es '),writeln(C2).

turno_dificil(Preguntas, Candidatos_jugador, Candidatos_maquina, Personaje_jugador, Personaje_maquina):-      
    writeln('-------------------------------------------------------------------'),
    writeln('Tus candidatos a elegir son:'),
    imprimir_candidatos(Candidatos_jugador),

    %Ejecutar accion jugador
    write('Las preguntas que puedes hacer son: '),
    preguntas(L), writeln(L),
    leer_pregunta(P), hacer_pregunta(P,Candidatos_jugador,Personaje_maquina,N_candidatos_jugador),

    %Ejecutar accion maquina
    write('Ahora me toca a mi, '),
    get(Preguntas,Primera,_),
    relacion(Primera,Candidatos_maquina,RelacionPrimera),
    elegir(Candidatos_maquina,Preguntas,Primera,RelacionPrimera,Primera,RelacionPrimera,Preg),
    select(Preg,Preguntas,NPreg), write(Preg), writeln(' ?'),
    hacer_pregunta(Preg,Candidatos_maquina,Personaje_jugador,N_Candidatos_maquina),

    %Comprobar si hay ganador
    calcular_posibilidades(N_Candidatos_maquina),

    %seguir jugando
    turno_dificil(NPreg, N_candidatos_jugador, N_Candidatos_maquina, Personaje_jugador, Personaje_maquina).