:- use_module(graficos).

% Este archivo se provee como una gu�a para facilitar la implementaci�n y 
% entender el uso de graficos.pl
% El contenido de este archivo se puede modificar.

% El predicado minimax_depth/1 define la recursi�n m�xima a utilizar en el algoritmo minimax
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
	(   gr_opciones(Visual, '�Seguro?', ['S�', 'No'], 'S�')
	->  true
	;   loop(Visual)
	).
evento(reiniciar,Visual) :-
	(   gr_opciones(Visual, '�Seguro?', ['S�', 'No'], 'S�')
	->  % reiniciar el juego
		true
	;   loop(Visual)
	).
