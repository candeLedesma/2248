:- module(proylcc, 
	[  
		join/4

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
explotar(Grilla,Columnas,[Ultimo],Sumatoria,[W]):-
	obtenerIndice(Ultimo,Columnas,Indice),
	setearValor(Grilla,Indice,0,Sumatoria,NuevaGrilla),
	W=NuevaGrilla.
explotar(Grilla,Columnas,[[Y|Ys]|Zs],Sumatoria,[R|Rs]):- obtenerIndice([Y|Ys],Columnas,Indice), 
	setearValor(Grilla,Indice,0,0,NuevaGrilla),
	R= NuevaGrilla,
	explotar(NuevaGrilla,Columnas,Zs,Sumatoria,Rs).


join(Grid, NumOfColumns, Path, RGrids):-
	smallerPow2GreaterOrEqualThan(Grid,NumOfColumns,Path,Sumatoria), 
	explotar(Grid,NumOfColumns,Path,Sumatoria,ListaGrillas),
	ultimo(ListaGrillas,Ultimo),
	gravity(Ultimo,NumOfColumns,GrillaNueva),
	append(ListaGrillas,[GrillaNueva],GrillaGravedad),
	rellenarGrilla(GrillaNueva,GrillaLlena),
	append(GrillaGravedad,[GrillaLlena],RGrids).


/*el mismo predicado que el join pero sin rellenar la grilla, se llama desde caso base 2 de obtenerLista*/
aplicarExplotarYGravedad(Grid, NumOfColumns, Path, GrillaGravedad):-
    smallerPow2GreaterOrEqualThan(Grid,NumOfColumns,Path,Sumatoria), 
	explotar(Grid,NumOfColumns,Path,Sumatoria,ListaGrillas),
	ultimo(ListaGrillas,Ultimo),
	gravity(Ultimo,NumOfColumns,GrillaNueva),
	append(ListaGrillas,[GrillaNueva],GrillaGravedad).



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


smallerPow2GreaterOrEqualThan(_Grid,_NumOfColumns,[],0).
smallerPow2GreaterOrEqualThan(_Grid,_NumOfColumns,[_U],0).
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
recorrerFilas(Grilla,[_Col,NumOfRows],_NumOfColumns,NumOfRows,Grilla).
recorrerFilas(Grilla,[Col,Fila],NumOfColumns,NumOfRows,GNueva):-
	obtenerIndice([Fila,Col],NumOfColumns,Indice),
	obtenerValor(Grilla,Indice,0,Valor),
	((Valor is 0 -> bajoElemento(Grilla,Fila,Col,NumOfColumns,Nueva));Nueva=Grilla),
    F is Fila+1,
	recorrerFilas(Nueva,[Col,F],NumOfColumns,NumOfRows,GNueva).

/* caso base col=0*/
recorrerColumnas(Grilla,[NumOfColumns,_Fila],NumOfColumns,_NumOfRows,Grilla).
recorrerColumnas(Grilla,[Col,Fila],NumOfColumns,NumOfRows,R):- 
	recorrerFilas(Grilla,[Col,0],NumOfColumns,NumOfRows,GNueva),
	C is Col+1, 
	recorrerColumnas(GNueva,[C,Fila],NumOfColumns,NumOfRows,R).


gravity(Grilla,NumOfColumns,GrillaG):- longitud(Grilla,Long),NumOfRows is Long/NumOfColumns,
	recorrerColumnas(Grilla,[0,NumOfRows],NumOfColumns,NumOfRows,GrillaG).

/*calcula el valor absoluto del numero X*/
abs(X, Y) :- X < 0,Y is -X.
abs(X, X) :- X >= 0.

/*true si las posiciones son adyacentes A0=\=B0,A1=\=B1,*/
adyacente([A0|A1],[B0|B1]):- posDistinta([A0|A1],[B0|B1]),I0 is A0-B0, I1 is A1-B1, abs(I0,R0), abs(I1,R1), R0=<1, R1=<1.

/*calculo auxiliar para recorrer matriz*/
calcularSiguientePosicion(NumOfColumns,Fila,Col,F,C):-((Col is NumOfColumns-1, F is Fila+1, C is 0); (F is Fila,C is Col+1)).

/*devulve el ultimo elemento de una lista*/
ultimo([U],U).
ultimo([_|Xs],Res):-ultimo(Xs,Res).

/*true si la posicion [X,Xs] es distinta a [Y,Ys]*/
posDistinta([X|Xs],[Y|Ys]):- (X \= Y;Xs \= Ys).

/*true si Z pertenece a L, Z es distinto de Pos y Pos es adyacente a Z*/
cumpleCondiciones(L,Pos) :-
    findall(Z, (member(Z,L), posDistinta(Pos,Z), adyacente(Z,Pos)), Zs), \+ Zs = [].


/*retorna en Lista todas las posiciones adyacentes a la posicion Pos con valor igual a elem*/
buscarAdyacentes(_Grilla,_Elem,NumOfRows,_NumOfColumns,[NumOfRows|0],Listita,_Visitados,Listita).
buscarAdyacentes([X|Xs],Elem,NumOfRows,NumOfColumns,[Fila|Col],Listita,Visitados,R):-
    calcularSiguientePosicion(NumOfColumns,Fila,Col,F,C),
    ((notmember([Fila,Col],Visitados),X is Elem,cumpleCondiciones(Listita,[Fila,Col]),
    append(Listita,[[Fila,Col]],NuevaLista));NuevaLista=Listita),
    buscarAdyacentes(Xs,Elem,NumOfRows,NumOfColumns,[F|C],NuevaLista,Visitados,R).


/*true si X no pertenece a la lista L*/
notmember(X, L) :- \+ member(X, L).

/*caso base NumOfRows=FILA,*/
recorrerGrilla(_Grilla,_GrillaAux,NumOfRows,_NumOfColumns,[NumOfRows|0],_Visitados,[]).
recorrerGrilla([X|Xs],GrillaAux,NumOfRows,NumOfColumns,[Fila|Col],Visitados,[G|Gs]):-
    (notmember([Fila,Col],Visitados),
    buscarAdyacentes(GrillaAux,X,NumOfRows,NumOfColumns,[0|0],[[Fila,Col]],Visitados,Adyacentes),
    (longitud(Adyacentes,Long),Long>1,G=Adyacentes,
    append(Visitados,Adyacentes,Visitados2));Visitados2=Visitados,G=[]),
    calcularSiguientePosicion(NumOfColumns,Fila,Col,F,C),
    recorrerGrilla(Xs,GrillaAux,NumOfRows,NumOfColumns,[F|C],Visitados2,Gs).
   

explotarGrupos(Grilla,_NumOfColumns,[],Grilla).
explotarGrupos(Grilla,NumOfColumns,[X|Xs],Resultado):-
    smallerPow2GreaterOrEqualThan(Grilla,NumOfColumns,X,Sumatoria),
    (longitud(X,Long),Long>0,
    explotar(Grilla,NumOfColumns,X,Sumatoria,ListaDeGrillas),
    ultimo(ListaDeGrillas,GrillaNueva),explotarGrupos(GrillaNueva,NumOfColumns,Xs,Resultado));
    explotarGrupos(Grilla,NumOfColumns,Xs,Resultado).


booster(Grilla,NumOfColumns,ResultadoFinal):-
	longitud(Grilla,Long),
	NumOfRows is Long/NumOfColumns,
	recorrerGrilla(Grilla,Grilla,NumOfRows,NumOfColumns,[0|0],[],Grupos),
    explotarGrupos(Grilla,NumOfColumns,Grupos,GrillaConCeros),
    gravity(GrillaConCeros,NumOfColumns,GrillaConGravedad),
    rellenarGrilla(GrillaConGravedad,GrillaLLena),
    ResultadoFinal=[GrillaConCeros,GrillaConGravedad,GrillaLLena].


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

%retorna en Lista todas las posiciones adyacentes a la posicion Pos
obtenerListaAdyacentesIguales(_Grilla,_Pos,_Elem,NumOfRows,_NumOfColumns,[NumOfRows,0],Camino,Camino).
obtenerListaAdyacentesIguales([X|Xs],Pos,Elem,NumOfRows,NumOfColumns,[Fila, Col],Camino,R):-
    calcularSiguientePosicion(NumOfColumns,Fila, Col,F,C), %NK: calcular es un nombre poco descriptivo, qué hace?
    ((notmember([Fila, Col],Camino),X is Elem,adyacente([Fila, Col],Pos),
    append(Camino,[[Fila,Col]],NuevaLista));
    NuevaLista=Camino),
    obtenerListaAdyacentesIguales(Xs,Pos,Elem,NumOfRows,NumOfColumns,[F,C],NuevaLista,R).

% elimina los elementos en comun que tenga L2 en L1 y retorna la lista sin repetidos L3
removerRepetidos(L1,L2,L3):- findall(Z, (member(Z,L1), notmember(Z, L2)), L3).

%CaminoMejor es el mejor entre el caminoA y el caminoB
mejorCamino(Grilla,NumOfColumns,CaminoA,CaminoB,CaminoMejor):-
    smallerPow2GreaterOrEqualThan(Grilla,NumOfColumns,CaminoA,S1),
    smallerPow2GreaterOrEqualThan(Grilla,NumOfColumns,CaminoB,S2),
    ((S1>S2,CaminoMejor=CaminoA);CaminoMejor=CaminoB).

obtenerListaAdyacentes(_Grilla,_Pos,_Elem,NumOfRows,_NumOfColumns,[NumOfRows,0],Camino,Camino).
obtenerListaAdyacentes([X|Xs],Pos,Elem,NumOfRows,NumOfColumns,[Fila, Col],Camino,R):-
    calcularSiguientePosicion(NumOfColumns,Fila, Col,F,C), 
    ((notmember([Fila, Col],Camino),igualOsiguiente(X,Elem),adyacente([Fila,Col],Pos),
    append(Camino,[[Fila,Col]],NuevaLista));
    NuevaLista=Camino),
    obtenerListaAdyacentes(Xs,Pos,Elem,NumOfRows,NumOfColumns,[F,C],NuevaLista,R).


% recibe la posicion de la suma actual y calcula la posicion en donde caera por el efecto gravedad
calcularPosicionSuma(CaminoActual,[XUlt,YUlt],[XReal,YReal]):- 
    findall([XPos,YPos],(member([XPos,YPos],CaminoActual),YPos is YUlt,XPos>XUlt),ListaColumnas),
    longitud(ListaColumnas,Long), YReal=YUlt, XReal is XUlt+Long.
    			
    
%Simula la gravedad en la grilla y verifica si en esta hay adyacentes iguales a la suma calculada del camino actual
verificarGrillaGravedad(_Grilla,_NumOfRows,_NumOfColumns,[],[]).
verificarGrillaGravedad(Grilla,NumOfRows,NumOfColumns,CaminoActual,Final):-	
    ultimo(CaminoActual,Ult),
    aplicarExplotarYGravedad(Grilla,NumOfColumns,CaminoActual,ListaDeGrillas),
    ultimo(ListaDeGrillas,GrillaGravedad),
    smallerPow2GreaterOrEqualThan(Grilla,NumOfColumns,CaminoActual,Suma),
    calcularPosicionSuma(CaminoActual,Ult,PosReal),
    obtenerListaAdyacentesIguales(GrillaGravedad,PosReal, Suma,NumOfRows, NumOfColumns,[0,0],[],AdyacentesIguales), %se fija si en la grilla con la gravedad aplicada hay adyacentes iguales a la suma del camino resultado
    ((longitud(AdyacentesIguales,Long2),Long2>0,Final=CaminoActual);
    Final=[]).



%caso base (Flag=1)
obtenerLista(Grilla,_NumOfRows,NumOfColumns,[],CaminoNuevo,MejorCamino,CaminoResultado,1):-
    mejorCamino(Grilla,NumOfColumns,MejorCamino,CaminoNuevo,CaminoResultado).

%caso base 2 (Flag=2)
obtenerLista(Grilla,NumOfRows,NumOfColumns,[],CaminoNuevo,MejorCamino,Final,2):-
    mejorCamino(Grilla,NumOfColumns,MejorCamino,CaminoNuevo,CaminoRes),
    verificarGrillaGravedad(Grilla,NumOfRows,NumOfColumns,CaminoRes,Final).

%caso rec
obtenerLista(Grilla,NumOfRows,NumOfColumns,[X|Xs],CaminoActual,MejorActual,R,Flag):-
    obtenerMejor(Grilla,X,NumOfRows,NumOfColumns,CaminoActual,MejorActual,MejorNuevo,Flag), %Seguimos el backtracking con X, el primer vecino
    mejorCamino(Grilla,NumOfColumns,MejorActual,MejorNuevo,CaminoResultado),
    obtenerLista(Grilla,NumOfRows,NumOfColumns,Xs,CaminoActual,CaminoResultado,R,Flag). %Ignoramos el primer vecino X y seguimos con el resto

%devuelve la lista sin repetidos y el camino nuevo
obtenerMejorAux(Grilla, Pos, NumOfColumns, NumOfRows, Remover, ListaSinRep, CaminoNuevo):-
    obtenerIndice(Pos, NumOfColumns, Indice),
    obtenerValor(Grilla, Indice, 0, Valor),
    obtenerListaAdyacentes(Grilla, Pos, Valor, NumOfRows, NumOfColumns, [0,0],[],Adyacentes),
    removerRepetidos(Adyacentes, Remover, ListaSinRep), 
    append(Remover, [Pos], CaminoNuevo).

%si caminoActual tiene unico elemento -> chequear que el valor de Pos sea igual -> append (camino valido)
obtenerMejor(Grilla,PosAct,NumOfRows,NumOfColumns,[UnicoElem|[]],MejorCamino,ResultadoFinal,Flag):- 
    obtenerIndice(PosAct,NumOfColumns,IndiceAct),
    obtenerValor(Grilla,IndiceAct,0,ValorAct),
    obtenerIndice(UnicoElem,NumOfColumns,Indice),
    obtenerValor(Grilla,Indice,0,Elem), 
    ((Elem is ValorAct,append([UnicoElem],[PosAct],CaminoNuevo),%camino valido
    obtenerListaAdyacentes(Grilla,PosAct,ValorAct,NumOfRows,NumOfColumns,[0,0],[],Adyacentes), 
    removerRepetidos(Adyacentes,[UnicoElem],ListaSinRep));
    (ListaSinRep=[],CaminoNuevo=[UnicoElem])), %camino invalido
    obtenerLista(Grilla,NumOfRows,NumOfColumns,ListaSinRep,CaminoNuevo,MejorCamino,ResultadoFinal,Flag).

%caso CaminoActual con mas de un elemento -> ningun chequeo -> append 
obtenerMejor(Grilla,Pos,NumOfRows,NumOfColumns,[X|Xs],MejorCamino,ResultadoFinal,Flag):-
    obtenerMejorAux(Grilla, Pos, NumOfColumns, NumOfRows, [X|Xs], ListaSinRep, CaminoNuevo),
    obtenerLista(Grilla,NumOfRows,NumOfColumns,ListaSinRep,CaminoNuevo,MejorCamino,ResultadoFinal,Flag).
%caso CaminoActual vacio -> ningun chequeo -> append 
obtenerMejor(Grilla,Pos,NumOfRows,NumOfColumns,[],MejorCamino,ResultadoFinal,Flag):-
    obtenerMejorAux(Grilla, Pos, NumOfColumns, NumOfRows, [], ListaSinRep, CaminoNuevo),
    obtenerLista(Grilla,NumOfRows,NumOfColumns,ListaSinRep,CaminoNuevo,MejorCamino,ResultadoFinal,Flag).
    
%Por cada elemento de la grilla busca el camino maximo que se puede obtener a partir de esta posicion
obtenerCaminoMaximo(_Grilla,_GrillaAux,NumOfRows,_NumOfColumns,[NumOfRows,0],CaminoR,CaminoR,_Flag).
obtenerCaminoMaximo([_X|Xs],Grilla,NumOfRows,NumOfColumns,[Fila,Col],CaminoAct,Resultado,Flag):-
    obtenerMejor(Grilla,[Fila,Col],NumOfRows,NumOfColumns,[],[],CaminoObt,Flag),
    mejorCamino(Grilla,NumOfColumns,CaminoAct,CaminoObt,CaminoMejor),
    calcularSiguientePosicion(NumOfColumns,Fila,Col,F,C),
    obtenerCaminoMaximo(Xs,Grilla,NumOfRows,NumOfColumns,[F,C],CaminoMejor,Resultado,Flag).
    

ayudaMaxima(Grilla,NumOfColumns,Resultado,Suma):-
    longitud(Grilla,Long),
    NumOfRows is Long/NumOfColumns,
    obtenerCaminoMaximo(Grilla,Grilla,NumOfRows,NumOfColumns,[0,0],[],Resultado,1),
    smallerPow2GreaterOrEqualThan(Grilla,NumOfColumns,Resultado,Suma).

ayudaMaximosIguales(Grilla,NumOfColumns,Resultado,Suma):-
    longitud(Grilla,Long),
    NumOfRows is Long/NumOfColumns,
    obtenerCaminoMaximo(Grilla,Grilla,NumOfRows,NumOfColumns,[0,0],[],Resultado,2),
    smallerPow2GreaterOrEqualThan(Grilla,NumOfColumns,Resultado,Suma).