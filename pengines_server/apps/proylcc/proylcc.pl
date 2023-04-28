:- module(proylcc, 
	[  
		join/4

	]).

:- module(proylcc, 
	[  
		gravity/4

	]).


/*longitud de una lista*/
longitud([],0).
longitud([X|Xs],Z):-longitud(Xs,Y), Z is Y+1.

/*posicion en donde quiero poner un 0*/
obtenerIndice([Y|Ys],Columnas,Indice):- I=Y,J=Ys,Indice is I*Columnas+J.

obtenerValor([],_Indice,_Cont,_Valor).
obtenerValor([X|Xs],Indice,Cont,Valor):- (( Indice is Cont, Valor=X);X is X),obtenerValor(Xs,Indice,Cont+1,Valor).

/*setear nuevo valor dado un indice de una grilla*/
setearValor([],_Indice,_Cont,_Valor,[]).
setearValor([X|Xs],Indice,Cont,Valor,[Z|Zs]):- ((Indice is Cont,Z=Valor);Z=X),setearValor(Xs,Indice,Cont+1,Valor,Zs).



/*pongo un 0 en la posicion I,J de la grilla*/
ponerEnCero([],_,[],_Cont).
ponerEnCero([X|Xs],Indice,[Z|Zs],Cont):- ((Indice is Cont,Z=0);Z=X),ponerEnCero(Xs,Indice,Zs,Cont+1).


/*join recursivo*/
joinRec(Grilla,Columnas,[Ultimo],[W],Resultado):-obtenerIndice(Ultimo,Columnas,Indice),setearValor(Grilla,Indice,0,Resultado,NuevaGrilla),W=NuevaGrilla.
joinRec(Grilla,Columnas,[[Y|Ys]|Zs],[R|Rs],Resultado):- obtenerIndice([Y|Ys],Columnas,Indice), 
	ponerEnCero(Grilla,Indice,NuevaGrilla,0),
	R= NuevaGrilla,
	joinRec(NuevaGrilla,Columnas,Zs,Rs,Resultado).


join(Grid, NumOfColumns, Path, RGrids):-smallerPow2GreaterOrEqualThan(Grid,NumOfColumns,Path,Resultado), joinRec(Grid,NumOfColumns,Path,RGrids,Resultado).    


/*predicados para calcular ultimo bloque*/
sumatoria([],0).
sumatoria([X|Xs],Sum):- sumatoria(Xs,Aux), Sum is Aux+X.

obtenerListaIndices([],_NumOfColumns,[]).
obtenerListaIndices([[X|Xs]|Zs],NumOfColumns,[Y|Ys]):-obtenerIndice([X|Xs],NumOfColumns,Index),
	obtenerListaIndices(Zs,NumOfColumns,Ys),
	Y=Index.



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



smallerPow2GreaterOrEqualThan(_Grid,_NumOfColumns,[Ultimo],Resultado).
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
recorrerColumnas(Grilla,0,_Fila,_NumOfColumns,_NumOfRows,Grilla).
recorrerColumnas(Grilla,Col,Fila,NumOfColumns,NumOfRows,R):- C is Col-1, 
	recorrerFilas(Grilla,C,0,NumOfColumns,NumOfRows,GNueva),
	recorrerColumnas(GNueva,C,Fila,NumOfColumns,NumOfRows,R).


gravity(Grilla,NumOfColumns,GrillaG):- longitud(Grilla,Long),NumOfRows is Long/NumOfColumns,
	recorrerColumnas(Grilla,NumOfColumns,NumOfRows,NumOfColumns,NumOfRows,GrillaG).