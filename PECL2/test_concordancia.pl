% Reglas gramaticales

test:- consult(draw), oracionCompuesta(X,[el,gato,que,es,gris,ama,a,maria,que,es,negro],[]), draw(X). 

countV([X|L],P):-countV(L,P1), v(X), P is P1+1.
countV([_|L],P):-countV(L,P).
countV([],0).

analizar(ORACION):-consult(draw),countV(ORACION,V), V = 1, oracion(X,ORACION,[]), draw(X).
analizar(ORACION):- oracionCompuesta(X,ORACION,[]), draw(X).

oracion(o(GN,GV)) --> g_nominal(GN,Gen,Num,Per), g_verbal(GV,Gen,Num,Per).

oracionSimple(X) --> oracion(X).

oracionCompuesta(X) --> orsubordinada(X).
oracionCompuesta(X) --> orcoordinada(X).

orcoordinada((oc(OR1, PR, OR2))) --> oracion(OR1), nexo(PR), oracion(OR2).
orcoordinada((oc(OR1, PR1, OR2, PR2, OR3))) --> oracion(OR1), nexo(PR1), oracion(OR2), nexo(PR2), oracion(OR3).

g_nominal(gn(D,N,ADJ),Gen,Num,Per) --> determinante(D,Gen,Num,Per), nombre(N,Gen,Num,Per), g_adjetival(ADJ,Gen,Num,Per).
g_nominal(gn(N,ADJ),Gen,Num,Per) --> nombre(N,Gen,Num,Per), g_adjetival(ADJ,Gen,Num,Per).

g_verbal(gv(V),Gen,Num,Per) --> verbo(V,Gen,Num,Per).

g_adjetival(gadj(ADJ),Gen,Num,Per) --> adjetivo(ADJ,Gen,Num,Per).

%Diccionario
%verbos
verbo(v(X),Gen,Num,Per) --> [X],{v(X,Gen,Num,Per)}.
v(ama,_,s,3).
v(come,_,s,3).
v(cazo,_,s,3).
v(estudia,_,s,3).
v(canta,_,s,3).
v(es,_,s,3).
v(son,_,p,3).
v(salta,_,s,3).
v(sonrie,_,s,3).
v(era,_,s,3).        

%determinantes
determinante(det(X),Gen,Num,Per) --> [X],{det(X,Gen,Num,Per)}.
det(el,m,s,_).
det(la,f,s,_).
det(un,m,s,_).
det(una,f,s,_).

%nombres
nombre(n(X),Gen,Num,Per) --> [X],{n(X,Gen,Num,Per)}.
n(hombre,m,s,_).
n(mujer,f,s,_).
n(manzanas,f,p,_).
n(universidad,f,s,_).
n(manzana,f,s,_).
n(gato,m,s,_).
n(canario,m,s,_).
n(ratones,m,p,_).
n(raton,m,s,_).
n(alumno,m,s,_).

%adjetivos
adjetivo(adj(X),Gen,Num,Per) --> [X],{adj(X,Gen,Num,Per)}.
adj(roja,f,s,_). 
adj(rojo,m,s,_).
adj(negro,m,s,_).
adj(grande,_,s,_).
adj(gris,_,s,_).
adj(alegre,_,s,_).
adj(pequeno,m,s,_).   