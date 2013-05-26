:- module(infect_matriz, [init_matriz/3]).

:- use_module(matrices).

init_matriz(X, Y, M) :-
		matrix(X, Y, vacio, M),
		set_cell(1,1,M,blanco),
		set_cell(X,Y,M,blanco),
		set_cell(1,Y,M,negro),
		set_cell(X,1,M,negro).