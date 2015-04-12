% Reglas gramaticales
consult(draw).

oracion(o(GN,GV)) --> g_nominal(GN), g_verbal(GV).

orsubordinada(or(C,GV)) --> conjunciones(C), g_verbal(GV).

g_nominal(gn(N)) --> nombre(N).
g_nominal(gn(D,N)) --> determinante(D), nombre(N).
g_nominal(gn(D,N,ADJ)) --> determinante(D), nombre(N), g_adjetival(ADJ).
g_nominal(gn(N,ADJ)) --> nombre(N), g_adjetival(ADJ).

g_verbal(gv(V)) --> verbo(V).
g_verbal(gv(V,GN)) --> verbo(V), g_nominal(GN).
g_verbal(gv(V,P,GN)) --> verbo(V), preposicion(P) ,g_nominal(GN).
g_verbal(gv(V,ADJ)) --> verbo(V), g_adjetival(ADJ).


g_adjetival(gadj(A)) --> adjetivo(A).
g_adjetival(gadj(OR)) --> orsubordinada(OR).

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