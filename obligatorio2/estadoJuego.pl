:- module(estadoJuego, [estado/7, obtener_matriz_estado/2, obtener_dimension_matriz/2, get_turno/2, set_turno/2, actualizar_estado/1, termino_juego/1, obtener_ganador/2, get_count_cells/3, get_count_oponent_cells/3, siguiente_turno/2, actualizar_matriz/2, get_mensaje/2, set_mensaje/2]).
:- use_module(matrices).
:- use_module(infect_matriz).

%inicializa_la_estructura_estado
estado(M,Dim,Turno,Negras,Blancas, Msg,Estado) :-
    init_matriz(Dim,Dim, M),
    count_cells(M, negro, Negras),
    count_cells(M, blanco, Blancas),
    (
        Turno == negro;
        Turno == blanco;
        Turno == terminado
    ),
    Estado =.. [estado,M,Dim,Turno,Negras,Blancas,Msg].

%retorna_la_matriz_correspondiente_al_estado
obtener_matriz_estado(E,M) :-
    arg(1, E, M).

%retorna_la_dimension_de_la_matriz
obtener_dimension_matriz(E,Dim) :-
    arg(2, E, Dim).

%retorna_el_turno_correspondiente_al_estado
get_turno(E,Turno) :-   
    arg(3,E,Turno).

%setea_el_turno_del_estado_E_a_Turno
set_turno(E,Turno) :-
    setarg(3,E,Turno).

	
get_mensaje(E,Msg) :-
	arg(6,E,Msg).
	
set_mensaje(E,Msg) :-
	setarg(6,E,Msg).
	
%actualiza_el_estado_de_la_matriz
actualizar_estado(E) :-
    obtener_matriz_estado(E,M),
    count_cells(M, negro, Negras),
    count_cells(M, blanco, Blancas),
    setarg(4,E,Negras),
    setarg(5,E,Blancas),
    (
        termino_juego(E) -> set_turno(E,terminado);
        true
    ).

%verifica_si_termino_el_juego
termino_juego(E) :-
    arg(2,E,Dim),
    arg(4,E,Negras),
    arg(5,E,Blancas),
    (
        (Blancas + Negras) =:= (Dim * Dim);
        Negras =:= 0;
        Blancas =:= 0
    ).

%retorna_el_ganador_del_juego
obtener_ganador(E,Ganador) :-
    termino_juego(E),
    arg(4,E,Negras),
    arg(5,E,Blancas),
    (
        Negras > Blancas,
        Ganador = negro;
        Blancas > Negras,
        Ganador = blanco;
        Ganador = empate
    ).

get_count_cells(Estado, negro, Cantidad) :-
    arg(4, Estado, Cantidad).

get_count_cells(Estado, blanco, Cantidad) :-
    arg(5, Estado, Cantidad).

get_count_oponent_cells(Estado, negro, Cantidad) :-
    arg(5, Estado, Cantidad).

get_count_oponent_cells(Estado, blanco, Cantidad) :-
    arg(4, Estado, Cantidad).

% siguiente_turno(+Actual, ?Siguiente).
%   Actual: Turno actual
%   Siguiente: El otro turno.
siguiente_turno(Actual, Siguiente) :-
    (
		Actual == terminado -> writeln('************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n************************************\n');
        Actual == blanco,
        Siguiente = negro;
        Siguiente = blanco
    ).

% actualizar_matriz(+Estado, +Matriz)
%   Estado: Estado para el cual se quiere actualizar la matriz.
%   Matriz: Nueva matriz. 
actualizar_matriz(Estado, Matriz) :-
    setarg(1, Estado, Matriz),
    count_cells(Matriz, negro, Negras),
    count_cells(Matriz, blanco, Blancas),
    setarg(4, Estado, Negras),
    setarg(5, Estado, Blancas),
    (
        termino_juego(Estado) -> set_turno(Estado, terminado);
        true
    ).

