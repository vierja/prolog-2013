:- module(infect_matriz, [init_matriz/3, get_adj_value/4, distance/5, infect_adj/4, get_adj_value_dist/6, get_adj_vacios_dist/6, get_adj_pos_oposite_value/5]).

:- use_module(matrices).
:- use_module(graficos).

init_matriz(X, Y, M) :-
	matrix(X, Y, vacio, M),
	set_cell(1,1,M,blanco),
	set_cell(X,Y,M,blanco),
	set_cell(1,Y,M,negro),
	set_cell(X,1,M,negro).
	
init_tablero(Visual,M,Dim) :-
	set_cell(1,1,M,blanco),
	gr_ficha(Visual, 1, 1, blanco),
	set_cell(Dim,Dim,M,blanco),
	gr_ficha(Visual, Dim, Dim, blanco),
	set_cell(1,Dim,M,negro),
	gr_ficha(Visual, 1, Dim, negro),
	set_cell(Dim,1,M,negro),
	gr_ficha(Visual, Dim, 1, negro).
	
% Obtiene un valor de adjacente a (X,Y) en la matriz M. (usar con findall)
get_adj_value(X, Y, M, L) :-
	% Obtengo el rango posible de filas
	succ(XminTmp, X),
	max_list([1, XminTmp], Xmin), % Obtengo el la posicion - 1 pero > 0.

	succ(X, Xmas1),
	get_num_rows(M, NRows),
	min_list([Xmas1, NRows], Xmax), % Obtengo la posicion + 1 pero <= Max

	% Obtengo el rango posibles de columas.
	succ(YminTmp, Y),
	max_list([1, YminTmp], Ymin),

	succ(Y, Ymas1),
	get_num_cols(M, NCols),
	min_list([Ymas1, NCols], Ymax),

	between(Xmin, Xmax, Xval),
	between(Ymin, Ymax, Yval),
	get_cell(Xval, Yval, M, L).

	%TODO: FALTA EL CHEQUEO PARA QUE NO DEVUELVA EL VALOR DE X,Y

% Obtiene un valor de adyacente a (X,Y) en la matriz M, a distancia Dist.

		
% Obtiene un valor de adjacente a (X,Y) en la matriz M. (usar con findall)
get_adj_pos_oposite_value(X, Y, M, Value, L) :-
	% Obtengo el rango posible de filas
	succ(XminTmp, X),
	max_list([1, XminTmp], Xmin), % Obtengo el la posicion - 1 pero > 0.

	succ(X, Xmas1),
	get_num_rows(M, NRows),
	min_list([Xmas1, NRows], Xmax), % Obtengo la posicion + 1 pero <= Max

	% Obtengo el rango posibles de columas.
	succ(YminTmp, Y),
	max_list([1, YminTmp], Ymin),

	succ(Y, Ymas1),
	get_num_cols(M, NCols),
	min_list([Ymas1, NCols], Ymax),

	between(Xmin, Xmax, Xval),
	between(Ymin, Ymax, Yval),

	get_cell(Xval, Yval, M, NotValue),

	Value \== NotValue,
	NotValue \== vacio,

	L =.. [pos, Xval, Yval].

	%TODO: FALTA EL CHEQUEO PARA QUE NO DEVUELVA EL VALOR DE X,Y

distance(X1, Y1, X2, Y2, Distance) :-
	Xdif = abs(X1 - X2),
	Ydif = abs(Y1 - Y2),
	max_list([Xdif, Ydif], Distance).


infect_adj(X, Y, M, Val) :-
	writeln('Infect adyacentes.'),
	findall(L, get_adj_pos_oposite_value(X, Y, M, Val, L), List),
	writeln('Lista de adyacentes:' + List),
	infect_list(List, M, Val).


infect_list([], _, _).
infect_list([H | T], M, Val) :-
	infect_list(T, M, Val),
	writeln('Infecto posicion: ' + H),
	H =.. [pos, Xval, Yval],
	writeln('Los valores X=' + Xval + ', Y=' + Yval),
	set_cell(Xval, Yval, M, Val).

/*get_pareja_valor(M, Val) :-
	findall(X, get_celdas_valor(M, Val, X), L).*/
	
get_adj_value_dist(X, Y, Matriz, Dim, Dist, L) :-
	Xmen is X-Dist,
	Xmas is X+Dist,
	Ymen is Y-Dist,
	Ymas is Y+Dist,
	between(Xmen,Xmas,Xval),
	between(Ymen,Ymas,Yval),
	Xval > 0,
	Yval > 0,
	Xval < Dim+1,
	Yval < Dim+1,
	distance(X,Y,Xval,Yval,Dist),
	get_cell(Xval,Yval,Matriz,Valor),
	L = (Xval,Yval,Valor).

get_adj_vacios_dist(X,Y,Matriz,Dim,Dist,L) :-
	Xmen is X-Dist,
	Xmas is X+Dist,
	Ymen is Y-Dist,
	Ymas is Y+Dist,
	between(Xmen,Xmas,Xval),
	between(Ymen,Ymas,Yval),
	Xval > 0,
	Yval > 0,
	Xval < Dim+1,
	Yval < Dim+1,
	distance(X,Y,Xval,Yval,Dist),
	get_cell(Xval,Yval,Matriz,vacio),
	L = (Xval,Yval).