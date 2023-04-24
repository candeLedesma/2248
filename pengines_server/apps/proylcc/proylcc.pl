:- module(proylcc, 
	[  
		join/4
	]).


/**
 * join(+Grid, +NumOfColumns, +Path, -RGrids) 
 * RGrids es la lista de grillas representando el efecto, en etapas, de combinar las celdas del camino Path
 * en la grilla Grid, con número de columnas NumOfColumns. El número 0 representa que la celda está vacía. 
 */ 

/*longitud de una lista*/
longitud([],0).
longitud([X|Xs],Z):-longitud(Xs,Y), Z is Y+1.

/*posicion en donde quiero poner un 0*/
obtenerIndice([Y|Ys],Columnas,Indice):- I=Y,J=Ys,Indice is I*Columnas+J.

/*pongo un 0 en la posicion I,J de la grilla*/
ponerEnCero([],Indice,[],_Cont).
ponerEnCero([X|Xs],Indice,[Z|Zs],Cont):- ((Indice is Cont,Z=0);Z=X),ponerEnCero(Xs,Indice,Zs,Cont+1).

/*setear nuevo valor dado un indice de una grilla*/
setearValor([],Indice,_Cont,_Valor,[]).
setearValor([X|Xs],Indice,Cont,Valor,[Z|Zs]):- ((Indice is Cont,Z=Valor);Z=X),setearValor(Xs,Indice,Cont+1,Valor,Zs).


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


obtenerValor([],_Indice,_Cont,_Valor).
obtenerValor([X|Xs],Indice,Cont,Valor):- (( Indice is Cont, Valor=X);X is X),obtenerValor(Xs,Indice,Cont+1,Valor).


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
	((ResPot is ResPiso, Resultado is ResPiso); potencia(2,ResPiso+1,Resultado)).
	
	
	