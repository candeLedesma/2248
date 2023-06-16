:- module(init, [ init/2 ]).

/**
 * init(-Grid, -NumOfColumns).
 * 
 * Predicado especificando la grilla inicial, que será mostrada al comienzo del juego, donde
 * Grid es una lista con los números que conforman la grilla, y NumOfColumns es la cantidad de columnas, 
 * determinando las dimensiones de la misma.
 */

/*original*/
/*init([
	64,4,64,32,16,
	64,8,16,2,32,
	2,4,64,64,2,
	2,4,32,16,4,
	16,4,16,16,16,
	16,64,2,32,32,
	64,2,64,32,64,
	32,2,64,32,4
], 5).*/
/* ejemplo 4*/
init([
	16,64,256,32,256,
	8,64,8,256,256,
	256,64,16,256,256,
	256,256,256,8,8192,
	256,16,64,16,16,
	16,16,8,32,8,
	32,32,64,512,128,
	128,2,2,8,32
], 5).
/*
init([2,32,2,
	8,8,128,
	16,2,16], 3).*/