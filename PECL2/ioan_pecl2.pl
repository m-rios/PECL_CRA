% Reglas gramaticales

oracion(o(GN,GV)) --> g_nominal(GN), g_verbal(GV).
oracion(o(GN,OR,GV)) --> g_nominal(GN), orsubordinada(OR), g_verbal(GV).

orsubordinada(or(C,GV)) --> conjunciones(C), g_verbal(GV).

g_nominal(gn(N)) --> nombre(N).
g_nominal(gn(D,N)) --> determinante(D), nombre(N).
g_nominal(gn(D,N,A)) --> determinante(D), nombre(N), adjetivo(A).
g_nominal(gn(N,A)) --> nombre(N), adjetivo(A).

g_verbal(gv(V)) --> verbo(V).
g_verbal(gv(V,GN)) --> verbo(V), g_nominal(GN).
g_verbal(gv(V,P,GN)) --> verbo(V), preposicion(P) ,g_nominal(GN).
g_verbal(gv(V,ADJ)) --> verbo(V), adjetivo(ADJ).


%Diccionario

%determinantes
determinante(det(X)) --> [X],{det(X)}.
det(el).
det(la).
det(un).
det(una).

%nombres
nombre(n(X)) --> [X],{n(X)}.
n(hombre).
n(mujer).
n(juan).
n(maria).
n(manzanas).
n(universidad).
n(manzana).
n(gato).
n(ratones).
n(raton).
n(alumno).
n(unive).

%verbos
verbo(v(X)) --> [X],{v(X)}.
v(ama).
v(come).
v(cazo).
v(estudia).
v(es).
v(era).

%conjunciones
conjunciones(conj(X)) --> [X],{conj(X)}.
conj(que).

%preposiciones
preposicion(prep(X)) --> [X],{prep(X)}.
prep(a).
prep(en).

%adjetivos
adjetivo(adj(X)) --> [X],{adj(X)}.
adj(roja). 
adj(negro).
adj(grande).
adj(gris).
adj(pequeno).	

%adverbios
adverbio(adv(X)) --> [X],{adv(X)}.

%pronombres