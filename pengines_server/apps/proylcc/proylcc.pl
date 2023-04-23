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
obtenerPos([Y|Ys],I,J):- I=Y,J=Ys.

/*obtener indice*/
obtenerIndice(I,J,Columnas,Indice):- Indice is I*Columnas+J.

/*pongo un 0 en la posicion I,J de la grilla*/
ponerEnCero([],Indice,[],_Cont).
ponerEnCero([X|Xs],Indice,[Z|Zs],Cont):- ((Indice is Cont, Z=0); Z=X),ponerEnCero(Xs,Indice,Zs,Cont+1).

/*join recursivo*/
joinRec(_,_Columnas,[Ultimo],[]).
joinRec(Grilla,Columnas,[[Y|Ys]|Zs],[R|Rs]):- obtenerPos([Y|Ys],I,J), 
	obtenerIndice(I,J,Columnas,Indice),
	ponerEnCero(Grilla,Indice,NuevaGrilla,0),
	R= NuevaGrilla,
	joinRec(NuevaGrilla,Columnas,Zs,Rs).



join(Grid, NumOfColumns, Path, RGrids):- joinRec(Grid,NumOfColumns,Path,RGrids).      