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


/*join recursivo*/
joinRec(_Grilla,_Columnas,[Ultimo],[]).
joinRec(Grilla,Columnas,[[Y|Ys]|Zs],[R|Rs]):- obtenerIndice([Y|Ys],Columnas,Indice), 
	ponerEnCero(Grilla,Indice,NuevaGrilla,0),
	R= NuevaGrilla,
	joinRec(NuevaGrilla,Columnas,Zs,Rs).


join(Grid, NumOfColumns, Path, RGrids):- joinRec(Grid,NumOfColumns,Path,RGrids).    





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


smallerPow2GreaterOrEqualThan(_Grid,_NumOfColumns,[Ultimo],Resultado).
smallerPow2GreaterOrEqualThan(Grid,NumOfColumns,Path,Resultado):- obtenerListaIndices(Path,NumOfColumns,Indices), 
	obtenerListaValores(Grid,Indices,Valores),
	sumatoria(Valores,Total).