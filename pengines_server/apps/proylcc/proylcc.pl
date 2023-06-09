:- module(proylcc, 
	[  
		join/4

	]).

:- module(proylcc, 
	[  
		gravity/4

	]).

/*calcula la longitud de la lista pasada por parametro*/
longitud([],0).
longitud([_X|Xs],Z):-longitud(Xs,Y), Z is Y+1.

/*posicion en donde quiero poner un 0*/
obtenerIndice([Y|Ys],Columnas,Indice):- I=Y,J=Ys,Indice is I*Columnas+J.

/*obtiene el valor en la grilla que corresponde con el Indice*/
obtenerValor([],_Indice,_Cont,_Valor).
obtenerValor([X|Xs],Indice,Cont,Valor):- (( Indice is Cont, Valor=X);X is X),obtenerValor(Xs,Indice,Cont+1,Valor).

/*setear nuevo valor dado un indice de una grilla*/
setearValor([],_Indice,_Cont,_Valor,[]).
setearValor([X|Xs],Indice,Cont,Valor,[Z|Zs]):- ((Indice is Cont,Z=Valor);Z=X),setearValor(Xs,Indice,Cont+1,Valor,Zs).


/*join recursivo*/
joinRec(Grilla,Columnas,[Ultimo],Sumatoria,[W]):-
	obtenerIndice(Ultimo,Columnas,Indice),
	setearValor(Grilla,Indice,0,Sumatoria,NuevaGrilla),
	W=NuevaGrilla.
joinRec(Grilla,Columnas,[[Y|Ys]|Zs],Sumatoria,[R|Rs]):- obtenerIndice([Y|Ys],Columnas,Indice), 
	setearValor(Grilla,Indice,0,0,NuevaGrilla),
	R= NuevaGrilla,
	joinRec(NuevaGrilla,Columnas,Zs,Sumatoria,Rs).


join(Grid, NumOfColumns, Path, RGrids):-
	smallerPow2GreaterOrEqualThan(Grid,NumOfColumns,Path,Sumatoria), 
	joinRec(Grid,NumOfColumns,Path,Sumatoria,ListaGrillas),
	ultimo(ListaGrillas,Ultimo),
	gravity(Ultimo,NumOfColumns,GrillaNueva),
	append(ListaGrillas,[GrillaNueva],GrillaGravedad),
	rellenarGrilla(GrillaNueva,GrillaLlena),
	append(GrillaGravedad,[GrillaLlena],RGrids).


/*predicados para calcular ultimo bloque*/
sumatoria([],0).
sumatoria([X|Xs],Sum):- sumatoria(Xs,Aux), Sum is Aux+X.


/* recibe el path como primer parametro, y retorna una nueva lista con los Indices correspondientes a 
cada posicion del path*/
obtenerListaIndices([],_NumOfColumns,[]).
obtenerListaIndices([[X|Xs]|Zs],NumOfColumns,[Y|Ys]):-obtenerIndice([X|Xs],NumOfColumns,Index),
	obtenerListaIndices(Zs,NumOfColumns,Ys),
	Y=Index.


/*recibe una grilla y una lista de indices ; retorna una lista de valores correspondientes a cada indice de la lista*/
obtenerListaValores(_Grilla,[],[]).
obtenerListaValores(Grilla,[Y|Ys],[Z|Zs]):- obtenerValor(Grilla,Y,0,Z), obtenerListaValores(Grilla,Ys,Zs).


log2(N, Resultado) :- Resultado is log(N) / log(2).

piso(X,Resultado):- Resultado is floor(X).


potencia(_,0,1). % Caso base: cualquier número elevado a 0 es igual a 1
potencia(X,N,P) :- % Caso recursivo
   N > 0, % Si N es mayor que 0
   N1 is N - 1, % Restar 1 a N
   potencia(X,N1,P1), % Calcular X elevado a N-1
   P is P1 * X. % El resultado es X multiplicado por X elevado a N-1



smallerPow2GreaterOrEqualThan(_Grid,_NumOfColumns,[_U],_R).
smallerPow2GreaterOrEqualThan(Grid,NumOfColumns,Path,Resultado):- obtenerListaIndices(Path,NumOfColumns,Indices), 
	obtenerListaValores(Grid,Indices,Valores),
	sumatoria(Valores,Total),
	log2(Total,ResLog),
	piso(ResLog,ResPiso),
	potencia(2,ResPiso,ResPot),
	((ResPot is Total, Resultado is Total); potencia(2,ResPiso+1,Resultado)).
	


/*BAJO UNA POSICION TODOS LOE ELEMENTOS DE LA COLUMNA J*/
bajoElemento(Grilla,0,J,_Columnas,Nueva):- setearValor(Grilla,J,0,0,Nueva).
bajoElemento(Grilla,I,J,Columnas,Resultado):- IndiceAC is I*Columnas+J,
	P is I-1,
	IndiceR is P*Columnas+J,
	obtenerValor(Grilla,IndiceR,0,ValorR),
	setearValor(Grilla,IndiceAC,0,ValorR,N),
    setearValor(N,IndiceR,0,0,C),
	bajoElemento(C,P,J,Columnas,Resultado).

/* caso base fila=NumOfRows*/
recorrerFilas(Grilla,_Col,NumOfRows,_NumOfColumns,NumOfRows,Grilla).
recorrerFilas(Grilla,Col,Fila,NumOfColumns,NumOfRows,GNueva):-
	obtenerIndice([Fila,Col],NumOfColumns,Indice),
	obtenerValor(Grilla,Indice,0,Valor),
	((Valor is 0 -> bajoElemento(Grilla,Fila,Col,NumOfColumns,Nueva));Nueva=Grilla),
    F is Fila+1,
	recorrerFilas(Nueva,Col,F,NumOfColumns,NumOfRows,GNueva).

/* caso base col=0*/
recorrerColumnas(Grilla,NumOfColumns,_Fila,NumOfColumns,_NumOfRows,Grilla).
recorrerColumnas(Grilla,Col,Fila,NumOfColumns,NumOfRows,R):- 
	recorrerFilas(Grilla,Col,0,NumOfColumns,NumOfRows,GNueva),
	C is Col+1, 
	recorrerColumnas(GNueva,C,Fila,NumOfColumns,NumOfRows,R).


gravity(Grilla,NumOfColumns,GrillaG):- longitud(Grilla,Long),NumOfRows is Long/NumOfColumns,
	recorrerColumnas(Grilla,0,NumOfRows,NumOfColumns,NumOfRows,GrillaG).

/*calcula el valor absoluto del numero X*/
abs(X, Y) :- X < 0,Y is -X.
abs(X, X) :- X >= 0.

/*true si las posiciones son adyacentes A0=\=B0,A1=\=B1,*/
adyacente([A0|A1],[B0|B1]):- I0 is A0-B0, I1 is A1-B1, abs(I0,R0), abs(I1,R1), R0=<1, R1=<1.

/*calculo auxiliar para recorrer matriz*/
calcular(NumOfColumns,FILA,COL,F,C):-((COL is NumOfColumns-1, F is FILA+1, C is 0); (F is FILA,C is COL+1)).

/*devulve el ultimo elemento de una lista*/
ultimo([U],U).
ultimo([_|Xs],Res):-ultimo(Xs,Res).

/*true si la posicion [X,Xs] es distinta a [Y,Ys]*/
posDistinta([X|Xs],[Y|Ys]):- (X \= Y;Xs \= Ys).

/*true si Z pertenece a L, Z es distinto de Pos y Pos es adyacente a Z*/
cumpleCondiciones(L,Pos) :-
    findall(Z, (member(Z,L), posDistinta(Pos,Z), adyacente(Z,Pos)), Zs), \+ Zs = [].


/*retorna en Lista todas las posiciones adyacentes a la posicion Pos*/
buscarAdyacentes(_Grilla,_Elem,NumOfRows,_NumOfColumns,[NumOfRows|0],Listita,_Visitados,Listita).
buscarAdyacentes([X|Xs],Elem,NumOfRows,NumOfColumns,[FILA|COL],Listita,Visitados,R):-
    calcular(NumOfColumns,FILA,COL,F,C),
    ((notmember([FILA,COL],Visitados),X is Elem,cumpleCondiciones(Listita,[FILA,COL]),
    append(Listita,[[FILA,COL]],NuevaLista));NuevaLista=Listita),
    buscarAdyacentes(Xs,Elem,NumOfRows,NumOfColumns,[F|C],NuevaLista,Visitados,R).


/*true si X no pertenece a la lista L*/
notmember(X, L) :- \+ member(X, L).

/*caso base NumOfRows=FILA,*/
recorrerGrilla(_Grilla,GrillaAux,NumOfRows,_NumOfColumns,[NumOfRows|0],_Visitados,GrillaAux).
recorrerGrilla([X|Xs],GrillaAux,NumOfRows,NumOfColumns,[FILA|COLUMNA],Visitados,Resultado):-
    (notmember([FILA,COLUMNA],Visitados),
    buscarAdyacentes(GrillaAux,X,NumOfRows,NumOfColumns,[0|0],[[FILA,COLUMNA]],Visitados,Adyacentes),
    (longitud(Adyacentes,Long),Long>1,join(GrillaAux,NumOfColumns,Adyacentes,ListaGrillas),
    append(Visitados,Adyacentes,Visitados2),ultimo(ListaGrillas,GrillaNueva));
    GrillaNueva=GrillaAux,Visitados2=Visitados),
    calcular(NumOfColumns,FILA,COLUMNA,F,C),
    recorrerGrilla(Xs,GrillaNueva,NumOfRows,NumOfColumns,[F|C],Visitados2,Resultado).
    
   
booster(Grilla,NumOfColumns,Resultado):-
	longitud(Grilla,Long),
	NumOfRows is Long/NumOfColumns,
	recorrerGrilla(Grilla,Grilla,NumOfRows,NumOfColumns,[0|0],[-1],Resultado).


/*caso base ambas listas estan vacias*/
 rellenarGrilla([], []).
 /*el 1er elemento de la lista puede ser 0
  * entonces devolvemos una lista con su primer elemento un numero random
  */
 rellenarGrilla([0|Xs], [ValorA|Ys]) :-
     random(2, 8, Random),
     potencia(2, Random, ValorA),
     rellenarGrilla(Xs, Ys).

 /*el 1er elemento de la lista no es 0
  * entonces devolvemos una lista con su primer elemento igual al de la anterior
  */
 rellenarGrilla([X|Xs], [X|Ys]) :-
     X \= 0,
     rellenarGrilla(Xs, Ys).
