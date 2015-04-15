% Reglas gramaticales

test:- consult(draw), oracionCompuesta(X,[el,gato,que,es,gris,ama,a,maria,que,es,negro],[]), draw(X). 

countV([X|L],P):-countV(L,P1), v(X,_,_,_), P is P1+1.
countV([_|L],P):-countV(L,P).
countV([],0).

test1:-analizar([el,hombre,grande,come,la,manzana,roja]).
test2:-analizar([el,hombre,con,un,tenedor,grande,come,la,manzana,roja]).
test3:-analizar([juan,y,maria,comen,la,manzana,roja,con,un,tenedor,y,un,cuchillo]).
test4:-analizar([ella,hace,la,practica,de,juan]).
test5:-analizar([el,canario,de,juan,y,maria,canta]).
test6:-analizar([la,blanca,paloma,alzo,el,vuelo]).
test7:-analizar([esta,muy,lejos,de,madrid]).
test8:-analizar([el,es,lento,de,reflejos]).
test9:-analizar([juan,habla,muy,claramente]).
test10:-analizar([la,esperanza,de,vida,de,un,nino,depende,de,su,lugar,de,nacimiento]).
test11:-analizar([el,hombre,que,vimos,en,la,universidad,era,mi,profesor]).
test12:-analizar([juan,que,es,muy,delicado,come,solamente,manzanas,rojas]).
test13:-analizar([el,procesador,de,textos,que,es,una,herramienta,muy,potente,sirve,para,escribir,documentos]).
test14:-analizar([juan,es,moreno,y,maria,es,alta]).
test15:-analizar([juan,recoge,la,mesa,mientras,maria,toma,un,cafe]).
test16:-analizar([compre,un,pantalon,y,una,corbata,negros]).
test17:-analizar([juan,y,hector,comen,patatas,fritas,y,beben,cerveza]).
test18:-analizar([irene,canta,y,salta,mientras,juan,estudia]).
test19:-analizar([irene,canta,y,salta,y,sonrie,alegre]).
test20:-analizar([el,procesador,de,textos,es,una,herramienta,muy,potente,que,sirve,para,escribir,documentos,pero,es,bastante,lento]).

antitest1:-analizar([el,hombre,grande,come,la,manzana,roja]).
antitest2:-analizar([el,hombre,con,un,tenedor,grande,come,la,manzana,roja]).
antitest3:-analizar([juan,y,maria,comen,la,manzana,roja,con,un,tenedor,y,un,cuchillo]).
antitest4:-analizar([ella,hace,la,practica,de,juan]).
antitest5:-analizar([el,canario,de,juan,y,maria,canta]).
antitest6:-analizar([la,blanca,paloma,alzo,el,vuelo]).
antitest7:-analizar([esta,muy,lejos,de,madrid]).
antitest8:-analizar([el,es,lento,de,reflejos]).
antitest9:-analizar([juan,habla,muy,claramente]).
antitest10:-analizar([la,esperanza,de,vida,de,un,nino,depende,de,su,lugar,de,nacimiento]).
antitest11:-analizar([el,hombre,que,vimos,en,la,universidad,era,mi,profesor]).
antitest12:-analizar([juan,que,es,muy,delicado,come,solamente,manzanas,rojas]).
antitest13:-analizar([el,procesador,de,textos,que,es,una,herramienta,muy,potente,sirve,para,escribir,documentos]).
antitest14:-analizar([juan,es,moreno,y,maria,es,alta]).
antitest15:-analizar([juan,recoge,la,mesa,mientras,maria,toma,una,cafe]).
antitest16:-analizar([compre,un,pantalon,y,una,corbata,negros]).
antitest17:-analizar([juan,y,hector,comen,patatas,fritas,y,beben,cerveza]).
antitest18:-analizar([irene,canta,y,salta,mientras,juan,estudia]).
antitest19:-analizar([irene,canta,y,salta,y,sonrie,alegre]).
antitest20:-analizar([el,procesador,de,textos,es,una,herramienta,muy,potente,que,sirve,para,escribir,documentos,pero,es,bastante,lento]).


analizar(ORACION):-consult(draw),countV(ORACION,V), V = 1, oracion(X,ORACION,[]), draw(X).
analizar(ORACION):- oracionCompuesta(X,ORACION,[]), draw(X).

oracion(o(GN,GV)) --> g_nominal(GN,_,_,_,_), g_verbal(GV).
oracion(o(GV)) --> g_verbal(GV).

oracionCompuesta(X) --> orsubordinada(X).
oracionCompuesta(X) --> orcoordinada(X).

orcoordinada((oc(OR1, PR, OR2))) --> oracion(OR1), nexo(PR), oracion(OR2).
orcoordinada((oc(OR1, PR1, OR2, PR2, OR3))) --> oracion(OR1), nexo(PR1), oracion(OR2), nexo(PR2), oracion(OR3).

orsubordinada(or(OR)) --> oracion(OR).

g_preposicional(gp(PR,GN)) --> preposicion(PR), nombre(GN,_,_,_,_).
g_preposicional(gp(PR,GN)) --> preposicion(PR),  g_nominal(GN,_,_,_,_).
g_preposicional(gp(PR,V, N,GPRP)) --> preposicion(PR),verbo(V,_,_,_,_), nombre(N,_,_,_,_), g_preposicional(GPRP) .
g_preposicional(gp(PR,GN, GPRP)) --> preposicion(PR), g_nominal(GN,_,_,_,_), g_preposicional(GPRP).
g_preposicional(gp(PR,GN)) --> preposicion(PR), g_nominal(GN,_,_,_,_).
g_preposicional(gp(PR,V, N)) --> preposicion(PR),verbo(V,_,_,_,_), nombre(N,_,_,_,_) .


g_nominal(gn(D, N, PREP, ORSUB_ADJ),Ge,Nu,Pe,C) --> determinante(D,Ge,Nu,Pe,C), nombre(N,Ge,Nu,Pe,C), g_preposicional(PREP), g_adjetival(ORSUB_ADJ,Ge,Nu,Pe,C),{C}. %frase 2, 13
g_nominal(gn(D, N, PREP),Ge,Nu,Pe,C) --> determinante(D,Ge,Nu,Pe,C), nombre(N,Ge,Nu,Pe,C), g_preposicional(PREP),{C}. %frase 5, 10(x2), 20
g_nominal(gn(N, ADJ),Ge,Nu,Pe,C) --> nombrePropio(N,Ge,Nu,Pe,C), g_adjetival(ADJ,Ge,Nu,Pe,C),{C}. %frase 12
g_nominal(gn(N1, PR, N2),_,p,_,true) --> nombrePropio(N1,_,_,_,_), nexo(PR), nombrePropio(N2,_,_,_,_). %frase 3, 5, 17
g_nominal(gn(D1, N1, PR, D2, N2),_,p,_,C) --> determinante(D1,Ge1,Nu1,_,C), nombre(N1,Ge1,Nu1,_,C), nexo(PR), determinante(D2,Ge2,Nu2,_,C), nombre(N2,Ge2,Nu2,_,C),{C}. %frase 3, 16
g_nominal(gn(D, ADJ, N),Ge,Nu,Pe,C) --> determinante(D,Ge,Nu,Pe,C), g_adjetival(ADJ,Ge,Nu,Pe,C), nombre(N,Ge,Nu,Pe,C),{C}. %frase 6
g_nominal(gn(D, N, ADJ),Ge,Nu,Pe,C) --> determinante(D,Ge,Nu,Pe,C), nombre(N,Ge,Nu,Pe,C), g_adjetival(ADJ,Ge,Nu,Pe,C),{C}. %frase 1, 3, 11, 20
g_nominal(gn(D, N),Ge,Nu,Pe,C) --> determinante(D,Ge,Nu,Pe,C), nombre(N,Ge,Nu,Pe,C),{C}. %frase 1, 2(x2), 6, 10, 11(x2), 15(x2)
g_nominal(gn(N),Ge,Nu,Pe,C) --> nombrePropio(N,Ge,Nu,Pe,C),{C}. %frase 4, 7, 9, 14(x2), 15(x2), 18, 19
g_nominal(gn(N),Ge,Nu,Pe,C) --> nombre(N,Ge,Nu,Pe,C),{C}. %frase 4, 8, 10, 12
													
g_verbal(gv(V1,PR1,V2,PR2,V3,ADJ)) --> verbo(V1,_,_,_,_), nexo(PR1), verbo(V2,_,_,_,_),nexo(PR2), verbo(V3,_,_,_,_), g_adjetival(ADJ,_,_,_,_). %frase 19
g_verbal(gv(V1,PR,V2)) --> verbo(V1,_,_,_,_), nexo(PR), verbo(V2,_,_,_,_). % frase 18
g_verbal(gv(V,GPR)) --> verbo(V,_,_,_,_), g_preposicional(GPR). % frase 10, 20
g_verbal(gv(V,GN,ADJ)) --> verbo(V,_,_,_,_), g_nominal(GN,Ge,Nu,_,C),g_adjetival(ADJ,Ge,Nu,_,C). %frase 13 
g_verbal(gv(V,ADV,GN,ADJ)) --> verbo(V,_,_,_,_), g_adverbial(ADV),g_nominal(GN,Ge,Nu,_,C), g_adjetival(ADJ,Ge,Nu,_,C). %frase 12, 20
g_verbal(gv(V,ADV,GPR)) --> verbo(V,_,_,_,_), g_adverbial(ADV), g_preposicional(GPR). %frase 7
g_verbal(gv(V,ADV)) --> verbo(V,_,_,_,_), g_adverbial(ADV). %frase 9
g_verbal(gv(V,ADJ,GPR)) --> verbo(V,_,_,_,_), g_adjetival(ADJ,_,_,_,_), g_preposicional(GPR). %frase 8
g_verbal(gv(V,ADJ)) --> verbo(V,_,_,_,_), g_adjetival(ADJ,_,_,_,_). % frase 14(dos veces) 
g_verbal(gv(V,GN,GPR)) --> verbo(V,_,_,_,_), g_nominal(GN,_,_,_,_), g_preposicional(GPR). %frase 3, 4
g_verbal(gv(V,GN)) --> verbo(V,_,_,_,_), g_nominal(GN,_,_,_,_). %frase 1,2,6, 11, 15 (x2), 16, 17(x2), 
g_verbal(gv(V)) --> verbo(V,_,_,_,_). %frase 5, 13, 18

g_adjetival(gadj(ADJ),Ge,Nu,_,C) --> adjetivo(ADJ,Ge,Nu,_,C).
g_adjetival(gadj(ADV,ADJ),Ge,Nu,_,C) --> adverbio(ADV), adjetivo(ADJ,Ge,Nu,_,C).
g_adjetival(gadj(CJ,OR),_,_,_,_) --> conjunciones(CJ), orsubordinada(OR).

g_adverbial(gadv(ADV1,ADV2)) --> adverbio(ADV1),adverbio(ADV2) .
g_adverbial(gadv(ADV,GN)) --> adverbio(ADV), g_nominal(GN,_,_,_,_).
g_adverbial(gadv(ADV)) --> adverbio(ADV).

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
n(el,m,s,3).
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
n(nino,m,s,_).
n(pantalon,m,s,_).
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
n(documentos,m,p,_).
n(mesa,f,s,_).
n(cafe,m,s,_).
n(patatas,f,p,_).
n(cerveza,f,s,_).

nombrePropio(np(X),Gen,Num,Per,C) --> [X],{np(X,Gen,Num,Per),C=true}.
nombrePropio(np(X),_,_,_,C) --> [_],{np(X,_,_,_),C=false}.
np(juan,m,s,_).
np(maria,f,s,_).
np(irene,f,s,_).
np(hector,m,s,_).
np(madrid,m,s,_).

%verbos
verbo(v(X),Gen,Num,Per,C) --> [X],{v(X,Gen,Num,Per),C=true}.
verbo(v(X),_,_,_,C) --> [X],{v(X,_,_,_),C=false}.
v(ama,_,s,3).
v(come,_,s,3).
v(comen,_,p,3).
v(cazo,_,s,3).
v(estudia,_,s,3).
v(depende,_,s,3).
v(escribir,_,_,_).
v(compre,_,s,1).
v(canta,_,s,3).
v(es,_,s,3).
v(salta,_,s,3).
v(sonrie,_,s,3).
v(era,_,s,3).
v(hace,_,s,3).
v(habla,_,s,3).
v(esta,_,s,3).
v(alzo,_,s,3).
v(vimos,_,p,1).
v(recoge,_,s,3).
v(sirve,_,s,3).
v(toma,_,s,3).
v(beben,_,p,3).

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
adj(pequeno,m,s,_). 
adj(lento,m,s,_).   

%adverbios
adverbio(adv(X)) --> [X],{adv(X)}.
adv(muy).
adv(en).
adv(claramente).
adv(solamente).
adv(bastante).
adv(lejos).