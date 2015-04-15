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

oracion(o(GN,GV),Gen,Num,Per,C) --> g_nominal(GN,Gen,Num,Per,C), g_verbal(GV,Gen,Num,Per,C).
oracion(o(GV),Gen,Num,Per,C) --> g_verbal(GV,Gen,Num,Per,C).

oracionCompuesta(X,Gen,Num,Per,C) --> orsubordinada(X,Gen,Num,Per,C).
oracionCompuesta(X,Gen,Num,Per,C) --> orcoordinada(X,Gen,Num,Per,C).

orcoordinada((oc(OR1, PR, OR2)),Gen,Num,Per,C) --> oracion(OR1,Gen,Num,Per,C), nexo(PR), oracion(OR2,Gen,Num,Per,C).
orcoordinada((oc(OR1, PR1, OR2, PR2, OR3)),Gen,Num,Per,C) --> oracion(OR1,Gen,Num,Per,C), nexo(PR1), oracion(OR2,Gen,Num,Per,C), nexo(PR2), oracion(OR3,Gen,Num,Per,C).

orsubordinada(or(OR),Gen,Num,Per,C) --> oracion(OR,Gen,Num,Per,C).

g_preposicional(gp(PR,GN, GPRP),Gen,Num,Per,C) --> preposicion(PR), g_nominal(GN,Gen,Num,Per,C), g_preposicional(GPRP,Gen,Num,Per,C).
g_preposicional(gp(PR,GN),Gen,Num,Per,C) --> preposicion(PR), nombre(GN,Gen,Num,Per,C).
g_preposicional(gp(PR,GN),Gen,Num,Per,C) --> preposicion(PR), g_nominal(GN,Gen,Num,Per,C).
g_preposicional(gp(PR,V, N),Gen,Num,Per,C) --> preposicion(PR),verbo(V,Gen,Num,Per,C), nombre(N,Gen,Num,Per,C) .


g_nominal(gn(D, N, PREP),Gen,Num,Per,C) --> determinante(D,Gen,Num,Per,C), nombre(N,Gen,Num,Per,C), g_preposicional(PREP,Gen,Num,Per,C).
g_nominal(gn(D, N, ADJ),Gen,Num,Per,C) --> determinante(D,Gen,Num,Per,C), nombre(N,Gen,Num,Per,C), g_adjetival(ADJ).
g_nominal(gn(N, ADJ),Gen,Num,Per,C) --> nombre(N,Gen,Num,Per,C), g_adjetival(ADJ).
g_nominal(gn(N, ADJ),Gen,Num,Per,C) --> nombrePropio(N,Gen,Num,Per,C), g_adjetival(ADJ).
g_nominal(gn(N1, PR, N2),Gen,Num,Per,C) --> nombrePropio(N1,Gen,Num,Per,C), nexo(PR), nombrePropio(N2,Gen,Num,Per,C).
g_nominal(gn(D1, N1, PR, D2, N2, ADJ),Gen,Num,Per,C) --> determinante(D1,Gen,Num,Per,C), nombre(N1,Gen,Num,Per,C), nexo(PR), determinante(D2,Gen,Num,Per,C), nombre(N2,Gen,Num,Per,C),g_adjetival(ADJ).
g_nominal(gn(D1, N1, PR, D2, N2),Gen,Num,Per,C) --> determinante(D1,Gen,Num,Per,C), nombre(N1,Gen,Num,Per,C), nexo(PR), determinante(D2,Gen,Num,Per,C), nombre(N2,Gen,Num,Per,C).
g_nominal(gn(D, ADJ, N),Gen,Num,Per,C) --> determinante(D,Gen,Num,Per,C), g_adjetival(ADJ), nombre(N,Gen,Num,Per,C).
g_nominal(gn(D, N),Gen,Num,Per,C) --> determinante(D,Gen,Num,Per,C), nombre(N,Gen,Num,Per,C).
g_nominal(gn(N),Gen,Num,Per,C) --> nombrePropio(N,Gen,Num,Per,C).
g_nominal(gn(N),Gen,Num,Per,C) --> nombre(N,Gen,Num,Per,C).

g_verbal(gv(V1,PR1,V2,PR2,V3,ADJ),Gen,Num,Per,C) --> verbo(V1,Gen,Num,Per,C), nexo(PR1), verbo(V2,Gen,Num,Per,C),nexo(PR2), verbo(V3,Gen,Num,Per,C), g_adjetival(ADJ).
g_verbal(gv(V1,PR,V2, ADJ),Gen,Num,Per,C) --> verbo(V1,Gen,Num,Per,C), nexo(PR), verbo(V2,Gen,Num,Per,C), g_adjetival(ADJ).
g_verbal(gv(V1,PR,V2),Gen,Num,Per,C) --> verbo(V1,Gen,Num,Per,C), nexo(PR), verbo(V2,Gen,Num,Per,C).

g_verbal(gv(V,GPR),Gen,Num,Per,C) --> verbo(V,Gen,Num,Per,C), g_preposicional(GPR,Gen,Num,Per,C).

g_verbal(gv(V,GN,ADV,ORSUB),Gen,Num,Per,C) --> verbo(V,Gen,Num,Per,C), g_nominal(GN,Gen,Num,Per,C), g_adverbial(ADV,Gen,Num,Per,C), orsubordinada(ORSUB,Gen,Num,Per,C).

g_verbal(gv(V,ADV,GPR),Gen,Num,Per,C) --> verbo(V,Gen,Num,Per,C), g_adverbial(ADV,Gen,Num,Per,C), g_preposicional(GPR,Gen,Num,Per,C).
g_verbal(gv(V,ADV),Gen,Num,Per,C) --> verbo(V,Gen,Num,Per,C), g_adverbial(ADV,Gen,Num,Per,C).

g_verbal(gv(V,ADJ,GPR),Gen,Num,Per,C) --> verbo(V,Gen,Num,Per,C), g_adjetival(ADJ), g_preposicional(GPR,Gen,Num,Per,C).
g_verbal(gv(V,ADJ),Gen,Num,Per,C) --> verbo(V,Gen,Num,Per,C), g_adjetival(ADJ).


g_verbal(gv(V,GN,GPR),Gen,Num,Per,C) --> verbo(V,Gen,Num,Per,C), g_nominal(GN,Gen,Num,Per,C), g_preposicional(GPR,Gen,Num,Per,C).
g_verbal(gv(V,GN),Gen,Num,Per,C) --> verbo(V,Gen,Num,Per,C), g_nominal(GN,Gen,Num,Per,C).

g_verbal(gv(V),Gen,Num,Per,C) --> verbo(V,Gen,Num,Per,C).

g_adjetival(gadj(ADJ),Gen,Num,Per,C) --> adjetivo(ADJ,Gen,Num,Per,C).
g_adjetival(gadj(ADV,ADJ),Gen,Num,Per,C) -->adverbio(ADV),adjetivo(ADJ,Gen,Num,Per,C).
g_adjetival(gadj(CJ,OR),Gen,Num,Per,C) --> conjunciones(CJ), orsubordinada(OR,Gen,Num,Per,C).

g_adverbial(gadv(ADV1,ADV2),_,_,_,_) --> adverbio(ADV1),adverbio(ADV2) .
g_adverbial(gadv(ADV,GN),Gen,Num,Per,C) --> adverbio(ADV), g_nominal(GN,Gen,Num,Per,C).
g_adverbial(gadv(ADV, ADJ),Gen,Num,Per,C) --> adverbio(ADV), adjetivo(ADJ,Gen,Num,Per,C).
g_adverbial(gadv(ADV),_,_,_,_) --> adverbio(ADV).
g_adverbial(gadj(CJ,OR),Gen,Num,Per,C) --> conjunciones(CJ),orsubordinada(OR,Gen,Num,Per,C).

%Diccionario
 		

%determinantes
determinante(det(X),Gen,Num,Per,C) --> [X],{det(X,Gen,Num,Per),C=true}.
determinante(det(X),_,_,_,C) --> [X],{det(X,_,_,_),C=false}.
det(el,m,s,_).
det(la,f,s,_).
det(mi,_,s,_).
det(un,m,s,_).
det(su,_,s,_).
det(una,f,s,_).

%nombres


nombre(n(X),Gen,Num,Per,C) --> [X],{n(X,Gen,Num,Per),C=true}.
nombre(n(X),_,_,_,C) --> [X],{n(X,_,_,_),C=false}.
n(hombre,m,s,_).
n(él,m,s,3).
n(ella,f,s,3).
n(mujer,f,s,_).
n(manzanas,f,p,_).
n(universidad,f,s,_).
n(manzana,f,s,_).
n(vida,f,s,_).
n(gato,m,s,_).
n(canario,m,s,_).
n(ratones,m,p,_).
n(raton,m,s,_).
n(niño,m,s,_).
n(pantalón,m,s,_).
n(tenedor,m,s,_).
n(cuchillo,m,s,_).
n(corbata,f,s,_).
n(alumno,m,s,_).
n(practica,f,s,_).
n(profesor,m,s,_).
n(vuelo,m,s,_).
n(paloma,f,s,_).
n(reflejos,m,p,_).
n(esperanza,f,s,_).
n(lugar,m,s,_).
n(nacimiento,m,s,_).
n(procesador,m,s,_).
n(textos,m,p,_).
n(herramienta,f,s,_).
n(documentos,m,p§,_).
n(mesa,f,s,_).
n(café,m,s,_).
n(patatas,f,p,_).
n(cerveza,f,s,_).

nombrePropio(np(X),Gen,Num,Per,C) --> [X],{np(X,Gen,Num,Per),C=true}.
nombrePropio(np(X),_,_,_,C) --> [_],{np(X,_,_,_),C=false}.
np(juan,m,s,_).
np(maria,f,s,_).
np(irene,f,s,_).
np(héctor,m,s,_).
np(madrid,m,s,_).

%verbos
verbo(v(X),Gen,Num,Per,C) --> [X],{v(X,Gen,Num,Per),C=true}.
verbo(v(X),_,_,_,C) --> [X],{v(X,_,_,_),C=false}.
v(ama_,s,3).
v(come_,s,3).
v(comen_,p,3).
v(cazó_,s,3).
v(estudia_,s,3).
v(depende_,s,3).
v(escribir_,_,_).
v(compré_,s,1).
v(canta_,s,3).
v(es_,s,3).
v(salta_,s,3).
v(sonrie_,s,3).
v(era_,s,3).
v(hace_,s,3).
v(habla_,s,3).
v(está_,s,3).
v(alzó_,s,3).
v(vimos_,p,1).
v(recoge_,s,3).
v(sirve_,s,3).
v(toma_,s,3).
v(beben_,p,3).

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
adjetivo(adj(X),Gen,Num,Per,C) --> [X],{adj(X,Gen,Num,Per),C=true}.
adjetivo(adj(X),_,_,_,C) --> [X],{adj(X,_,_,_),C=false}.
adj(roja,f,s,_). 
adj(rojas,f,p,_). 
adj(negro,m,s,_).
adj(blanca,f,s,_).
adj(potente,_,s,_).
adj(grande,_,s,_).
adj(moreno,m,s,_).
adj(delicado,m,s,_).
adj(alta,f,s,_).
adj(gris,_,s,_).
adj(fritas,f,_,_).
adj(negros,m,p,_).
adj(alegre,_,s,_).
adj(pequeño,m,s,_).	
adj(lento,m,s,_).	

%adverbios
adverbio(adv(X)) --> [X],{adv(X)}.
adv(muy).
adv(en).
adv(claramente).
adv(solamente).
adv(bastante).
adv(lejos).


%pronombres