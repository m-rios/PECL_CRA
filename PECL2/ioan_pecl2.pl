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

orsubordinada(or(OR)) --> oracion(OR).

g_preposicional(gp(PR,GN)) --> preposicion(PR), g_nominal(GN).

g_nominal(gn(D,N,ADJ)) --> determinante(D), nombre(N), g_adjetival(ADJ).
g_nominal(gn(D,N,PREP)) --> determinante(D), nombre(N), g_preposicional(PREP).
g_nominal(gn(N,ADJ)) --> nombre(N), g_adjetival(ADJ).
g_nominal(gn(N,ADJ)) --> nombrePropio(N), g_adjetival(ADJ).
g_nominal(gn(N1, PR, N2)) --> nombrePropio(N1), nexo(PR), nombrePropio(N2).
g_nominal(gn(D,N)) --> determinante(D), nombre(N).
g_nominal(gn(N)) --> nombrePropio(N).

g_verbal(gv(V1,PR,V2, ADJ)) --> verbo(V1), nexo(PR), verbo(V2), g_adjetival(ADJ).
g_verbal(gv(V1,PR,V2)) --> verbo(V1), nexo(PR), verbo(V2).
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
n(canario).
n(ratones).
n(raton).
n(alumno).

nombrePropio(np(X)) --> [X],{np(X)}.
np(juan).
np(maria).
np(irene).

%verbos
verbo(v(X)) --> [X],{v(X)}.
v(ama).
v(come).
v(cazo).
v(estudia).
v(canta).
v(es).
v(salta).
v(sonrie).
v(era).

%conjunciones
conjunciones(conj(X)) --> [X],{conj(X)}.
conj(que).
conj(de).

nexo(nx(X)) --> [X],{nx(X)}.
nx(y).	

%preposiciones
preposicion(prep(X)) --> [X],{prep(X)}.
prep(a).
prep(en).
prep(de).

%adjetivos
adjetivo(adj(X)) --> [X],{adj(X)}.
adj(roja). 
adj(negro).
adj(grande).
adj(gris).
adj(alegre).
adj(pequeno).	

%adverbios
adverbio(adv(X)) --> [X],{adv(X)}.
adv(mientras).




%pronombres