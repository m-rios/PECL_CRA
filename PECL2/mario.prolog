% Reglas gramaticales

test:- consult(draw), oracionCompuesta(X,[el,gato,que,es,gris,ama,a,maria,que,es,negro],[]), draw(X). 

countV([X|L],P):-countV(L,P1), v(X), P is P1+1.
countV([_|L],P):-countV(L,P).
countV([],0).


oracion(o(GN,GV)) --> g_nominal(GN), g_verbal(GV).
oracion(o(GV)) --> g_verbal(GV).

oracionSimple(X) --> oracion(X).

oracionCompuesta(X) --> orsubordinada(X).
oracionCompuesta(X) --> orcoordinada(X).

nexo(X) --> adverbio(X).
nexo(X) --> conjunciones(X).

orcoordinada((oc(OR1, PR, OR2))) --> oracion(OR1), nexo(PR), oracion(OR2).

orsubordinada(or(OR)) --> oracion(OR).

g_preposicional(gp(PR,GN)) --> preposicion(PR), g_nominal(GN).

g_nominal(gn(D,N,ADJ)) --> determinante(D), nombre(N), g_adjetival(ADJ).
g_nominal(gn(N,ADJ)) --> nombre(N), g_adjetival(ADJ).
g_nominal(gn(N,ADJ)) --> nombrePropio(N), g_adjetival(ADJ).
g_nominal(gn(D,N)) --> determinante(D), nombre(N).
g_nominal(gn(N)) --> nombrePropio(N).

g_verbal(gv(V,GPR)) --> verbo(V), g_preposicional(GPR).
g_verbal(gv(V,ADJ)) --> verbo(V), g_adjetival(ADJ).
g_verbal(gv(V,GN)) --> verbo(V), g_nominal(GN).
g_verbal(gv(V)) --> verbo(V).

g_adjetival(gadj(ADJ)) --> adjetivo(ADJ).
g_adjetival(gadj(CJ,OR)) --> conjunciones(CJ), orsubordinada(OR).

g_adverbial(gadv(ADV)) --> adverbio(ADV).
g_adverbial(gadj(CJ,OR)) --> conjunciones(CJ),orsubordinada(OR).
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
n(manzanas).
n(universidad).
n(manzana).
n(gato).
n(ratones).
n(raton).
n(alumno).

nombrePropio(np(X)) --> [X],{np(X)}.
np(juan).
np(maria).

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
conj(y).

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
adv(mientras).




%pronombres