% Reglas gramaticales

test:- consult(draw), oracionCompuesta(X,[el,gato,que,es,gris,ama,a,maria,que,es,negro],[]), draw(X). 

countV([X|L],P):-countV(L,P1), v(X), P is P1+1.
countV([_|L],P):-countV(L,P).
countV([],0).

test1:-analizar([el,hombre,grande,come,la,manzana,roja]).
test2:-analizar([el,hombre,con,un,tenedor,grande,come,la,manzana,roja]).
test3:-analizar([juan,y,maria,comen,la,manzana,roja,con,un,tenedor,y,un,cuchillo]).
test4:-analizar([ella,hace,la,practica,de,juan]).
test5:-analizar([el,canario,de,juan,y,maria,canta]).
test6:-analizar([la,blanca,paloma,alzo,el,vuelo]).
test7:-analizar([está,muy,lejos,de,madrid]).
test8:-analizar([él,es,lento,de,reflejos]).
test9:-analizar([juan,habla,muy,claramente]).
test10:-analizar([la,esperanza,de,vida,de,un,niño,depende,de,su,lugar,de,nacimiento]).
test11:-analizar([el,hombre,que,vimos,en,la,universidad,era,mi,profesor]).
test12:-analizar([juan,que,es,muy,delicado,come,solamente,manzanas,rojas]).
test13:-analizar([el,procesador,de,textos,que,es,una,herramienta,muy,potente,sirve,para,escribir,documentos]).
test14:-analizar([juan,es,moreno,y,maria,es,alta]).
test15:-analizar([juan,recoge,la,mesa,mientras,maria,toma,un,café]).
test16:-analizar([compré,un,pantalón,y,una,corbata,negros]).
test17:-analizar([juan,y,héctor,comen,patatas,fritas,y,beben,cerveza]).
test18:-analizar([irene,canta,y,salta,mientras,juan,estudia]).
test19:-analizar([irene,canta,y,salta,y,sonrie,alegre]).
test20:-analizar([el,procesador,de,textos,es,una,herramienta,muy,potente,que,sirve,para,escribir,documentos,pero,es,bastante,lento]).






analizar(ORACION):-consult(draw),countV(ORACION,V), V = 1, oracion(X,ORACION,[]), draw(X).
analizar(ORACION):- oracionCompuesta(X,ORACION,[]), draw(X).

oracion(o(GN,GV)) --> g_nominal(GN), g_verbal(GV).
oracion(o(GV)) --> g_verbal(GV).

oracionCompuesta(X) --> orsubordinada(X).
oracionCompuesta(X) --> orcoordinada(X).

orcoordinada((oc(OR1, PR, OR2))) --> oracion(OR1), nexo(PR), oracion(OR2).
orcoordinada((oc(OR1, PR1, OR2, PR2, OR3))) --> oracion(OR1), nexo(PR1), oracion(OR2), nexo(PR2), oracion(OR3).

orsubordinada(or(OR)) --> oracion(OR).

g_preposicional(gp(PR,GN, GPRP)) --> preposicion(PR), g_nominal(GN), g_preposicional(GPRP).
g_preposicional(gp(PR,GN)) --> preposicion(PR), nombre(GN).
g_preposicional(gp(PR,GN)) --> preposicion(PR), g_nominal(GN).
g_preposicional(gp(PR,V, N)) --> preposicion(PR),verbo(V), nombre(N) .


g_nominal(gn(D, N, PREP)) --> determinante(D), nombre(N), g_preposicional(PREP).
g_nominal(gn(D, N, ADJ)) --> determinante(D), nombre(N), g_adjetival(ADJ).
g_nominal(gn(N, ADJ)) --> nombre(N), g_adjetival(ADJ).
g_nominal(gn(N, ADJ)) --> nombrePropio(N), g_adjetival(ADJ).
g_nominal(gn(N1, PR, N2)) --> nombrePropio(N1), nexo(PR), nombrePropio(N2).
g_nominal(gn(D1, N1, PR, D2, N2, ADJ)) --> determinante(D1), nombre(N1), nexo(PR), determinante(D2), nombre(N2),g_adjetival(ADJ).
g_nominal(gn(D1, N1, PR, D2, N2)) --> determinante(D1), nombre(N1), nexo(PR), determinante(D2), nombre(N2).
g_nominal(gn(D, ADJ, N)) --> determinante(D), g_adjetival(ADJ), nombre(N).
g_nominal(gn(D, N)) --> determinante(D), nombre(N).
g_nominal(gn(N)) --> nombrePropio(N).
g_nominal(gn(N)) --> nombre(N).

g_verbal(gv(V1,PR1,V2,PR2,V3,ADJ)) --> verbo(V1), nexo(PR1), verbo(V2),nexo(PR2), verbo(V3), g_adjetival(ADJ).
g_verbal(gv(V1,PR,V2, ADJ)) --> verbo(V1), nexo(PR), verbo(V2), g_adjetival(ADJ).
g_verbal(gv(V1,PR,V2)) --> verbo(V1), nexo(PR), verbo(V2).

g_verbal(gv(V,GPR)) --> verbo(V), g_preposicional(GPR).

g_verbal(gv(V,GN,ADV,ORSUB)) --> verbo(V), g_nominal(GN), g_adverbial(ADV), orsubordinada(ORSUB).

g_verbal(gv(V,ADV,GPR)) --> verbo(V), g_adverbial(ADV), g_preposicional(GPR).
g_verbal(gv(V,ADV)) --> verbo(V), g_adverbial(ADV).

g_verbal(gv(V,ADJ,GPR)) --> verbo(V), g_adjetival(ADJ), g_preposicional(GPR).
g_verbal(gv(V,ADJ)) --> verbo(V), g_adjetival(ADJ).


g_verbal(gv(V,GN,GPR)) --> verbo(V), g_nominal(GN), g_preposicional(GPR).
g_verbal(gv(V,GN)) --> verbo(V), g_nominal(GN).

g_verbal(gv(V)) --> verbo(V).

g_adjetival(gadj(ADJ)) --> adjetivo(ADJ).
g_adjetival(gadj(ADV,ADJ)) -->adverbio(ADV),adjetivo(ADJ).
g_adjetival(gadj(CJ,OR)) --> conjunciones(CJ), orsubordinada(OR).

g_adverbial(gadv(ADV1,ADV2)) --> adverbio(ADV1),adverbio(ADV2) .
g_adverbial(gadv(ADV,GN)) --> adverbio(ADV), g_nominal(GN).
g_adverbial(gadv(ADV, ADJ)) --> adverbio(ADV), adjetivo(ADJ).
g_adverbial(gadv(ADV)) --> adverbio(ADV).
g_adverbial(gadj(CJ,OR)) --> conjunciones(CJ),orsubordinada(OR).

%Diccionario
 		

%determinantes
determinante(det(X)) --> [X],{det(X)}.
det(el).
det(la).
det(mi).
det(un).
det(su).
det(una).

%nombres


nombre(n(X)) --> [X],{n(X)}.
n(hombre).
n(él).
n(ella).
n(mujer).
n(manzanas).
n(universidad).
n(manzana).
n(vida).
n(gato).
n(canario).
n(ratones).
n(raton).
n(niño).
n(pantalón).
n(tenedor).
n(cuchillo).
n(corbata).
n(alumno).
n(practica).
n(profesor).
n(vuelo).
n(paloma).
n(reflejos).
n(esperanza).
n(lugar).
n(nacimiento).
n(procesador).
n(textos).
n(herramienta).
n(documentos).
n(mesa).
n(café).
n(patatas).
n(cerveza).


nombrePropio(np(X)) --> [X],{np(X)}.
np(juan).
np(maria).
np(irene).
np(héctor).
np(madrid).

%verbos
verbo(v(X)) --> [X],{v(X)}.
v(ama).
v(come).
v(comen).
v(cazo).
v(estudia).
v(depende).
v(escribir).
v(compré).
v(canta).
v(es).
v(salta).
v(sonrie).
v(era).
v(hace).
v(habla).
v(está).
v(alzo).
v(vimos).
v(recoge).
v(sirve).
v(toma).
v(beben).

%conjunciones
conjunciones(conj(X)) --> [X],{conj(X)}.
conj(que).
conj(de).

nexo(nx(X)) --> [X],{nx(X)}.
nx(y).
nx(pero).	
nx(mientras).	

%preposiciones
preposicion(prep(X)) --> [X],{prep(X)}.
prep(a).
prep(de).
prep(con).
prep(para).

%adjetivos
adjetivo(adj(X)) --> [X],{adj(X)}.
adj(roja). 
adj(rojas). 
adj(negro).
adj(blanca).
adj(potente).
adj(grande).
adj(moreno).
adj(delicado).
adj(alta).
adj(gris).
adj(fritas).
adj(negros).
adj(alegre).
adj(pequeno).	
adj(lento).	

%adverbios
adverbio(adv(X)) --> [X],{adv(X)}.
adv(muy).
adv(en).
adv(claramente).
adv(solamente).
adv(bastante).
adv(lejos).


%pronombres