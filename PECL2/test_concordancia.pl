% Reglas gramaticales

test:- consult(draw), oracionCompuesta(X,[el,gato,que,es,gris,ama,a,maria,que,es,negro],[]), draw(X). 

countV([X|L],P):-countV(L,P1), v(X), P is P1+1.
countV([_|L],P):-countV(L,P).
countV([],0).

analizar(ORACION):-consult(draw),countV(ORACION,V), V = 1, oracion(X,ORACION,[]), draw(X).
analizar(ORACION):- oracionCompuesta(X,ORACION,[]), draw(X).

oracion(o(GN,GV)) --> g_nominal(GN), g_verbal(GV).
oracion(o(GV)) --> g_verbal(GV).

oracionSimple(X) --> oracion(X).

oracionCompuesta(X) --> orsubordinada(X).
oracionCompuesta(X) --> orcoordinada(X).

orcoordinada((oc(OR1, PR, OR2))) --> oracion(OR1), nexo(PR), oracion(OR2).
orcoordinada((oc(OR1, PR1, OR2, PR2, OR3))) --> oracion(OR1), nexo(PR1), oracion(OR2), nexo(PR2), oracion(OR3).

g_nominal(gn(D,N,ADJ)) --> determinante(D,C), nombre(N,C), g_adjetival(ADJ,C).
g_nominal(gn(N,ADJ,C)) --> nombre(N,C), g_adjetival(ADJ,C).

g_adjetival(gadj(ADJ),C) --> adjetivo(ADJ,C).

%Diccionario
        

%determinantes
determinante(det(X),C) --> [X],{det(X,C)}.
det(el,ms).
det(la,fs).
det(un,ms).
det(una,fs).

%nombres
nombre(n(X),C) --> [X],{n(X,C)}.
n(hombre,ms).
n(mujer,fs).
n(manzanas,fp).
n(universidad,fs).
n(manzana,fs).
n(gato,ms).
n(canario,ms).
n(ratones,mp).
n(raton,ms).
n(alumno,ms).

%adjetivos
adjetivo(adj(X),C) --> [X],{adj(X,C)}.
adj(roja,fs). 
adj(rojo,ms).
adj(negro,ms).
adj(grande,ms).
adj(grande,fs).
adj(gris,ms).
adj(gris,fs).
adj(alegre,ms).
adj(alegre,fs).
adj(pequeno,ms).   