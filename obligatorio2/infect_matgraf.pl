:- module(infect_matgraf, [dibujar_matriz/2]).

:- use_module(graficos).
:- use_module(infect_matriz).
:- use_module(estadoJuego).

dibujar_matriz(Estado, Visual) :-
	(   
		obtener_matriz_estado(Estado,Matriz),
		obtener_dimension_matriz(Estado,Dim),
		between(1, Dim, FF),
	    between(1, Dim, CC),
		get_cell(FF, CC, Matriz, Val),
	    gr_ficha(Visual, FF, CC, Val),
	    fail
	;   true
	).