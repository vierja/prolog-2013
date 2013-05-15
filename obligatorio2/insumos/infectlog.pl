:- use_module(graficos).

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
	loop(Visual),
	gr_destruir(Visual).

% loop(+Visual)
loop(Visual) :-
	sformat(Msg, 'Mensaje de estado'),
	gr_estado(Visual, Msg),
	gr_evento(Visual,E),
	evento(E,Visual).

% evento(+Event,+Visual)
evento(click(Fila,Columna),Visual) :-
	gr_ficha(Visual,Fila,Columna,blanco),
	loop(Visual).
evento(salir,Visual) :-
	(   gr_opciones(Visual, '¿Seguro?', ['Sí', 'No'], 'Sí')
	->  true
	;   loop(Visual)
	).
evento(reiniciar,Visual) :-
	(   gr_opciones(Visual, '¿Seguro?', ['Sí', 'No'], 'Sí')
	->  % reiniciar el juego
		true
	;   loop(Visual)
	).
