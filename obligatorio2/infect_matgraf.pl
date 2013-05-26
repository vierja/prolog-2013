:- module(infect_matgraf, [dibujar_matriz/2]).

:- use_module(graficos).
:- use_module(infect_matriz).

dibujar_matriz(Matriz, Visual) :-
	(   between(1, 7, FF),
	    between(1, 7, CC),
		get_cell(FF, CC, Matriz, Val),
	    gr_ficha(Visual, FF, CC, Val),
	    fail
	;   true
	).