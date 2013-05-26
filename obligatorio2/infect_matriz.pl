:- module(infect_matriz, [init_matriz/3, get_adj_value/4, distance/5, infect_adj/4]).

:- use_module(matrices).

init_matriz(X, Y, M) :-
		matrix(X, Y, vacio, M),
		set_cell(1,1,M,blanco),
		set_cell(X,Y,M,blanco),
		set_cell(1,Y,M,negro),
		set_cell(X,1,M,negro).

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
