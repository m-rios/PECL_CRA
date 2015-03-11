
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
    %ejecutar consola de juego
    turno_facil(Preguntas, Candidatos_jugador, Candidatos_maquina, Personaje_jugador, Personaje_maquina).

%casos base, si algunas de las listas de candidatos tienen solo 1 elemento, ese personaje ha ganado.
%como juega primero la persona, si en el mismo turno ambos ganan, se da preferencia al jugador. 
turno_facil(_, [Sol_jugador],_,_,_):- write('Tu ganas, mi personaje es '),writeln(Sol_jugador).
turno_facil(_, _, [Sol_maquina],_,_):- write('Yu gano, tu personaje es '),writeln(Sol_maquina).

%función recursiva para cada turno
turno_facil(Preguntas, Candidatos_jugador, Candidatos_maquina, Personaje_jugador, Personaje_maquina):-      
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
    turno_facil(NPreg, N_candidatos_jugador, N_Candidatos_maquina, Personaje_jugador, Personaje_maquina).