:- use_module(graficos).
:- use_module(matrices).
:- use_module(infect_matriz).
:- use_module(infect_matgraf).

% Este archivo se provee como una guía para facilitar la implementación y 
% entender el uso de graficos.pl
% El contenido de este archivo se puede modificar.

% El predicado minimax_depth/1 define la recursión máxima a utilizar en el algoritmo minimax
minimax_depth(4).

% infectlog
infectlog :-
	gr_crear(Visual, [
		     boton('Reiniciar',reiniciar),
		     boton('Salir',salir)], % salir puede ser por el boton o por el click en la ventana
		 7,7),
	init_matriz(7,7, Matriz),
	dibujar_matriz(Matriz, Visual),
	
	sformat(Msg, 'Inicio de juego'),
	gr_estado(Visual, Msg),
	loop(Visual, Matriz),
	gr_destruir(Visual).

% loop(+Visual, +Matriz)
loop(Visual, Matriz) :-
	dibujar_matriz(Matriz, Visual),
	gr_evento(Visual,E),
	evento(E,Visual, Matriz).

% evento(+Event,+Visual)
evento(click(Fila,Columna),Visual, Matriz) :-
	gr_ficha(Visual,Fila,Columna,blanco),
	set_cell(Fila,Columna,Matriz,blanco),
	loop(Visual, Matriz).
evento(salir,Visual, Matriz) :-
	(   gr_opciones(Visual, '¿Seguro?', ['Sí', 'No'], 'Sí')
	->  true
	;   loop(Visual, Matriz)
	).
evento(reiniciar,Visual, Matriz) :-
	(   gr_opciones(Visual, '¿Seguro?', ['Sí', 'No'], 'Sí')
	->  % reiniciar el juego
		true
	;   loop(Visual, Matriz)
	).
