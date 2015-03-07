resta([X,Candidatos],Aeliminar, Resto):-
	guardar(X,Aeliminar,Resto,Resto_aux),
	resta(Candidatos,Aeliminar,Resto).
resta([],Aeliminar,Resto).

guardar(X,Aeliminar,Resto,Resto_aux):-
	union(Resto,[],Resto_aux),
	not(member(X,Aeliminar)),
	insertar(X,Resto_aux,Resto).