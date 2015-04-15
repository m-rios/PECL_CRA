% Reglas gramaticales

test:- consult(draw), oracionCompuesta(X,[el,gato,que,es,gris,ama,a,maria,que,es,negro],[]), draw(X). 

countV([X|L],P):-countV(L,P1), v(X), P is P1+1.
countV([_|L],P):-countV(L,P).
countV([],0).

analizar(ORACION):-consult(draw),countV(ORACION,V), V = 1, oracion(X,ORACION,[]), draw(X).
analizar(ORACION):- oracionCompuesta(X,ORACION,[]), draw(X).

oracion(o(GN,GV)) --> g_nominal(GN,Gen,Num,Per,T,C), g_verbal(GV,Gen,Num,Per,T,C),{T,C}.
oracion(o(GN,GV)) --> g_nominal(GN,Gen,Num,Per,T,C1), g_verbal(GV,Gen,Num,Per,T,_),{not(C1),writeln('Fallo de concordancia')}.
oracion(o(GN,GV)) --> g_nominal(GN,Gen,Num,Per,T,_), g_verbal(GV,Gen,Num,Per,T,C2),{not(C2),writeln('Fallo de concordancia')}.
oracion(o(GN,GV)) --> {writeln('Termino no reconocido'),true,false}.

oracionSimple(X) --> oracion(X).

oracionCompuesta(X) --> orsubordinada(X).
oracionCompuesta(X) --> orcoordinada(X).

orcoordinada((oc(OR1, PR, OR2))) --> oracion(OR1), nexo(PR), oracion(OR2).
orcoordinada((oc(OR1, PR1, OR2, PR2, OR3))) --> oracion(OR1), nexo(PR1), oracion(OR2), nexo(PR2), oracion(OR3).

g_nominal(gn(D,N,ADJ),Gen,Num,Per,T,C) --> determinante(D,Gen,Num,Per,T,C),{T,C}, nombre(N,Gen,Num,Per,T,C),{T,C}, g_adjetival(ADJ,Gen,Num,Per,T,C),{T,C}.
g_nominal(gn(N,ADJ),Gen,Num,Per,T,C) --> nombre(N,Gen,Num,Per,T,C),{T,C}, g_adjetival(ADJ,Gen,Num,Per,T,C),{T,C}.
g_nominal(gn(N,ADJ),Gen,Num,Per,T,C) --> nombrePropio(N,Gen,Num,Per,T,C),{T,C}, g_adjetival(ADJ,Gen,Num,Per,T,C),{T,C}.
g_nominal(gn(N1, PR, N2),_,p,3,T,C) --> nombrePropio(N1,_,_,_,T,C),{T,C}, nexo(PR),{T,C}, nombrePropio(N2,_,_,_,T,C),{T,C}.
g_nominal(gn(N),Gen,Num,Per,T,C) --> nombrePropio(N,Gen,Num,Per,T,C),{T,C}.

g_verbal(gv(V),Gen,Num,Per,T,C) --> verbo(V,Gen,Num,Per,T,C).

g_adjetival(gadj(ADJ),Gen,Num,Per,T,C) --> adjetivo(ADJ,Gen,Num,Per,T,C).

%Diccionario
%verbos
verbo(v(X),Gen,Num,Per,T,C) --> [X],{v(X,Gen,Num,Per),T=true,C=true}.
verbo(v(X),_,_,_,T,C) --> [X],{v(X,_,_,_),T=true,C=false}.
%verbo(_,_,_,_,T,_) --> [_],{T=false}.
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
determinante(det(X),Gen,Num,Per,T,C) --> [X],{det(X,Gen,Num,Per),T=true,C=true}.
determinante(det(X),_,_,_,T,C) --> [X],{det(X,_,_,_),T=true,C=false}.
%determinante(_,_,_,_,T,_) --> [_],{T=false}.
det(el,m,s,_).
det(la,f,s,_).
det(un,m,s,_).
det(una,f,s,_).

%nombres
nombre(n(X),Gen,Num,Per,T,C) --> [X],{n(X,Gen,Num,Per),T=true,C=true}.
nombre(n(X),_,_,_,T,C) --> [X],{n(X,_,_,_),T=true,C=false}.
%nombre(_,_,_,_,T,_) --> [_],{T=false}.
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

%nombres propios
nombrePropio(np(X),Gen,Num,Per,T,C) --> [X],{np(X,Gen,Num,Per),T=true,C=true}.
nombrePropio(np(X),_,_,_,T,C) --> [_],{np(X,_,_,_),T=true,C=false}.
%nombrePropio(_,_,_,_,T,_) --> [_],{T=false}.
np(juan,m,s,_).
np(maria,f,s,_).
np(irene,f,s,_).

%adjetivos
adjetivo(adj(X),Gen,Num,Per,T,C) --> [X],{adj(X,Gen,Num,Per),T=true,C=true}.
adjetivo(adj(X),_,_,_,T,C) --> [X],{adj(X,_,_,_),T=true,C=false}.
%adjetivo(_,_,_,_,T,_) --> [_],{T=false}.
adj(roja,f,s,_). 
adj(rojo,m,s,_).
adj(negro,m,s,_).
adj(grande,_,s,_).
adj(gris,_,s,_).
adj(alegre,_,s,_).
adj(pequeno,m,s,_).   

nexo(nx(X)) --> [X],{nx(X)}.
nx(y).  