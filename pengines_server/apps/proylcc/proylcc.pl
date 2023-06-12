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

log2(0,0):-!.
log2(N, Resultado) :- Resultado is log(N) / log(2).

piso(X,Resultado):- Resultado is floor(X).


potencia(_,0,1). % Caso base: cualquier número elevado a 0 es igual a 1
potencia(X,N,P) :- % Caso recursivo
   N > 0, % Si N es mayor que 0
   N1 is N - 1, % Restar 1 a N
   potencia(X,N1,P1), % Calcular X elevado a N-1
   P is P1 * X. % El resultado es X multiplicado por X elevado a N-1



smallerPow2GreaterOrEqualThan(_Grid,_NumOfColumns,[_U],0).%REVISAR ESE 0!!!!!!
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
adyacente([A0|A1],[B0|B1]):- posDistinta([A0|A1],[B0|B1]),I0 is A0-B0, I1 is A1-B1, abs(I0,R0), abs(I1,R1), R0=<1, R1=<1.

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




% segunda etapa:


%true si X=Elem o X es la sigueinte potencia de Elem
igualOsiguiente(X,Elem):- X is Elem*2; X is Elem.

/*retorna en Lista todas las posiciones adyacentes a la posicion Pos

NK:
Elem es el valor en Pos
FILA y COL no son constantes, por qué ponerlas en mayúscula?
Si estamos calculando adyacentes, por qué hay una variable Camino?

*/
obtenerListaAdyacentes(_Grilla,_Pos,_Elem,NumOfRows,_NumOfColumns,NumOfRows,0,Camino,Camino).
obtenerListaAdyacentes([X|Xs],Pos,Elem,NumOfRows,NumOfColumns,FILA,COL,Camino,R):-
    calcular(NumOfColumns,FILA,COL,F,C), %NK: calcular es un nombre poco descriptivo, qué hace?
    ((notmember([FILA,COL],Camino),igualOsiguiente(X,Elem),adyacente([FILA,COL],Pos),
    append(Camino,[[FILA,COL]],NuevaLista));
    NuevaLista=Camino),
    obtenerListaAdyacentes(Xs,Pos,Elem,NumOfRows,NumOfColumns,F,C,NuevaLista,R).

removerRepetidos(L1,L2,L3):- findall(Z, (member(Z,L1), notmember(Z, L2)), L3).


%CaminoMejor es el mejor entre el caminoA y el caminoB

/*mejorCamino(Grilla,NumOfColumns,caminoA,caminoB,caminoMejor):-
    writeln('en mejorCamino:'),
    smallerPow2GreaterOrEqualThan(Grilla,NumOfColumns,caminoA,S1),
    smallerPow2GreaterOrEqualThan(Grilla,NumOfColumns,caminoB,S2),
    ((S1>S2,caminoMejor=caminoA);caminoMejor=caminoB).*/

obtenerLista(Grilla,_NumOfRows,NumOfColumns,[],CaminoNuevo,MejorCamino,CaminoResultado):-
    smallerPow2GreaterOrEqualThan(Grilla,NumOfColumns,MejorCamino,S1),
    smallerPow2GreaterOrEqualThan(Grilla,NumOfColumns,CaminoNuevo,S2),
    ((S1>S2,CaminoResultado=MejorCamino);CaminoResultado=CaminoNuevo).
    %NK: La lista de vecinos es vacia, se terminó el camino
    %mejorCamino(Grilla,NumOfColumns,MejorCamino,CaminoNuevo,CaminoResultado),

    
obtenerLista(Grilla,NumOfRows,NumOfColumns,[X|Xs],CaminoActual,MejorActual,R):-
    %NK: La lista de vecinos no es vacia, hay que seguir
    obtenerMejor(Grilla,X,NumOfRows,NumOfColumns,CaminoActual,MejorActual,MejorNuevo), %NK: Seguimos el backtracking con X, el primer vecino
    %mejorCamino(Grilla,NumOfColumns,MejorActual,MejorNuevo,MejorDeTodos),%Cande: nuevo metodo mejorCamino
    smallerPow2GreaterOrEqualThan(Grilla,NumOfColumns,MejorNuevo,S1),
    smallerPow2GreaterOrEqualThan(Grilla,NumOfColumns,MejorActual,S2),
    ((S1>S2, CaminoResultado=MejorNuevo);CaminoResultado=MejorActual),
    obtenerLista(Grilla,NumOfRows,NumOfColumns,Xs,CaminoActual,CaminoResultado,R). %NK: Ignoramos el primer vecino X y seguimos con el resto
    %NK: Acá falta quedarnos con el mejor de las dos llamadas!!



%si caminoActual tiene unico elemento -> chequear que el valor de Pos sea igual -> append (camino valido)
obtenerMejor(Grilla,PosAct,NumOfRows,NumOfColumns,[UnicoElem|[]],MejorCamino,ResultadoFinal):- 
    obtenerIndice(PosAct,NumOfColumns,IndiceAct),
    obtenerValor(Grilla,IndiceAct,0,ValorAct),
    obtenerIndice(UnicoElem,NumOfColumns,Indice),
    obtenerValor(Grilla,Indice,0,Elem), 
    ((Elem is ValorAct,append([UnicoElem],[PosAct],CaminoNuevo),%camino valido
    obtenerListaAdyacentes(Grilla,PosAct,ValorAct,NumOfRows,NumOfColumns,0,0,[],Adyacentes), 
    removerRepetidos(Adyacentes,[UnicoElem],ListaSinRep));
    (ListaSinRep=[],CaminoNuevo=[UnicoElem])), %camino invalido
    obtenerLista(Grilla,NumOfRows,NumOfColumns,ListaSinRep,CaminoNuevo,MejorCamino,ResultadoFinal).

%caso CaminoActual con mas de un elmento -> ningun chequeo -> append 
obtenerMejor(Grilla,Pos,NumOfRows,NumOfColumns,[X|Xs],MejorCamino,ResultadoFinal):-
    obtenerIndice(Pos,NumOfColumns,Indice),
    obtenerValor(Grilla,Indice,0,Valor),
    append([X|Xs],[Pos],CaminoNuevo), %NK: Agrego POS al camino
    obtenerListaAdyacentes(Grilla,Pos,Valor,NumOfRows,NumOfColumns,0,0,[],Adyacentes), 
    removerRepetidos(Adyacentes,[X|Xs],ListaSinRep), 
    obtenerLista(Grilla,NumOfRows,NumOfColumns,ListaSinRep,CaminoNuevo,MejorCamino,ResultadoFinal).

obtenerMejor(Grilla,Pos,NumOfRows,NumOfColumns,[],MejorCamino,ResultadoFinal):-
    obtenerIndice(Pos,NumOfColumns,Indice),
    obtenerValor(Grilla,Indice,0,Valor),
    append([],[Pos],CaminoNuevo), %NK: Agrego POS al camino
    obtenerListaAdyacentes(Grilla,Pos,Valor,NumOfRows,NumOfColumns,0,0,[],Adyacentes), 
    removerRepetidos(Adyacentes,[],ListaSinRep), 
    obtenerLista(Grilla,NumOfRows,NumOfColumns,ListaSinRep,CaminoNuevo,MejorCamino,ResultadoFinal).
    
%NK: Creo que falta chequear que los dos primeros elemento sean iguales (dejar para el final)


%compara el valor maximo de cada camino y retorna el mayor con su camino correspondiente
obtenerCaminoMaximo(_Grilla,_GrillaAux,NumOfRows,_NumOfColumns,NumOfRows,0,CaminoR,CaminoR).
obtenerCaminoMaximo([_X|Xs],Grilla,NumOfRows,NumOfColumns,FILA,COL,CaminoAct,Resultado):-
    %NK: Itero por todas las casillas iniciales, calculo el mejor camino para cada una y me quedo con la mejor al final
     obtenerMejor(Grilla,[FILA,COL],NumOfRows,NumOfColumns,[],[],CaminoObt),
     longitud(CaminoObt,Long),
     %mejorCamino(Grilla,NumOfColumns,CaminoAct,CaminoObt,CaminoMejor),%Cande: nuevo metodo mejorCamino
     smallerPow2GreaterOrEqualThan(Grilla,NumOfColumns,CaminoObt,S1),
     smallerPow2GreaterOrEqualThan(Grilla,NumOfColumns,CaminoAct,S2),
     ((Long>1,S1>S2,CaminoMejor=CaminoObt);CaminoMejor=CaminoAct),
     calcular(NumOfColumns,FILA,COL,F,C),
     obtenerCaminoMaximo(Xs,Grilla,NumOfRows,NumOfColumns,F,C,CaminoMejor,Resultado).
    
ayudaMaxima(Grilla,NumOfColumns,Resultado,Suma):-
	longitud(Grilla,Long),
	NumOfRows is Long/NumOfColumns,
	obtenerCaminoMaximo(Grilla,Grilla,NumOfRows,NumOfColumns,0,0,[],Resultado),
    smallerPow2GreaterOrEqualThan(Grilla,NumOfColumns,Resultado,Suma).


/*?-obtenerMejor([2,2,
             4,16],[0,0],2,2,[],[],F).*/

/*ayudaMaxima([2,4,
             8,16],2,F).*/


/*Casos de prueba ayudaMaxima:
 * caso 1:
 ayudaMaxima([2,2,4,
             32,8,4,
             2,128,32],3,F).
  * caso 2:
  ayudaMaxima([2,2,8,
             16,32,16,
             4,2,8],3,F).
   * caso 3:
   ayudaMaxima([4,2,2,
             16,16,4,
             4,4,4],3,F).
             
    * caso 4:
    ayudaMaxima([2,2,4,
             16,16,32,
             8,2,2],3,F).
    * CASO 5:
    ayudaMaxima([2,4,8,
              16,16,32,
              4,4,8],3,F).
     * CASO 6:
     ayudaMaxima([2,4,16,
              16,4,8,
              8,8,32],3,F,S).
      * CASO 7:
      ayudaMaxima([2,4,8,32,
				16,4,8,16,
				2,2,32,32,
				4,4,2,8],4,F,S).
      * CASO 8:
      ayudaMaxima([2,4,8,32,
			16,4,8,16,
			2,2,32,32,
			4,4,64,8],4,F,S).
   
 * */