:- module(minimax, [minimax/6]).
:- use_module(matrices).
:- use_module(infect_matriz).

:- use_module(estadoJuego).



/*
    Realiza el algoritmo recursivo de minimax
*/

/*
    Esto nose si puede funcionar asi.
    Es como la manera de decir. Primero proba si recursion level
    devuelve true usando el predicado minimax_depth.
    Si devuelve true entonces el mejor estado es el estado actual
    y calculamos el valor de este.
*/
/*minimax(Estado, Maquina, NivelRecursion, Estado, Val) :-
    minimax_depth(NivelRecursion),
    evalEstado(Estado, Maquina, Val), /*writeln('minimax - fin de recusion. valor:' + Val),*/ !.*/

% minimax(-Estado, -Maquina, -NivelRecursion, +MejorEstado, +Val).
%   Estado: Estado del juego (Matriz, Turno, ...).
%   Maquina: Color de la maquina.
%   NivelRecursion: Nivel de recursion actual del algoritmo.
%   MejorEstado: Mejor estado calculado por el algoritmo.
%   Valor: Valor numerico del Estado devuelto.
%   RecursionLevel: Nivel de recusion.
minimax(Estado, Maquina, NivelRecursion, MejorEstado, Val, Depth) :-
    get_turno(Estado, TurnoEstado),
    %%writeln('**************************\nMinimax nivel recursion:' + NivelRecursion + ', Maquina: ' + Maquina + ', Turno: ' + TurnoEstado),
    (
        NivelRecursion == Depth,
        evalEstado(Estado, Maquina, Val), !%%writeln('minimax - FIN DE RECURSION (nivel: ' + NivelRecursion + '). valor:' + Val), !
        ;
        get_turno(Estado, TurnoActual),
        bagof(EstadoSiguiente, jugada_posible(Estado, TurnoEstado, EstadoSiguiente), ListaEstadoSiguiente),
        %%writeln('Se tiene el bag of de estados siguientes'),
        %%writeln('**************************\n'),
        %temp_check_estado(ListaEstadoSiguiente, TurnoSig),
        mejor_estado(ListaEstadoSiguiente, Maquina, TurnoEstado, MejorEstado, Val, NivelRecursion, Depth),
        !%%writeln('Nivel de recusion: ' + NivelRecursion), get_turno(MejorEstado, TurnoObtenido), writeln('Turno Mejor estado obtenido: ' + TurnoObtenido),!
    ).

/*
    Compara de forma recursiva todos los elementos de la lista.
*/

% mejor_estado(+ListaEstadoSiguiente, +Maquina, -MejorEstado, -Val, +NivelRecursion).
%   ListaEstadoSiguiente: Lista de estados de jugadas posibles siguientes.
%   Maquina: Color de la maquina.
%   MejorEstado: El mejor estado en las condiciones de Minimax (depende del color).
%   Val: El valor numero del mejor estado.
mejor_estado([MejorEstado], Maquina, TurnoEstado, MejorEstado, Val, NivelRecursion, Depth) :-
    (
        get_turno(MejorEstado, terminado) ->
            evalEstado(MejorEstado, Maquina, Val)
            ;
        % No importa el siguiente estado del siguiente estado del siguiente estado (etc..). Solo importa el valor.
        SigNivelRecursion is NivelRecursion + 1,
        minimax(MejorEstado, Maquina, SigNivelRecursion, _, Val, Depth), !
    ).

mejor_estado([Estado1 | ListaEstado], Maquina, TurnoEstado, MejorEstado, MejorVal, NivelRecursion, Depth) :-
    (
        get_turno(Estado1, terminado) ->
            evalEstado(Estado1, Maquina, Val1)
            ;

        SigNivelRecursion is NivelRecursion + 1,
        minimax(Estado1, Maquina, SigNivelRecursion, _, Val1, Depth)
    ),
    mejor_estado(ListaEstado, Maquina, TurnoEstado, Estado2, Val2, NivelRecursion, Depth),
    minimax_eval(Maquina, TurnoEstado, Estado1, Val1, Estado2, Val2, MejorEstado, MejorVal).
    %%writeln('mejor_estado - mejor estado! de mejor estado: ' + MejorVal).


mismo_turno(Estado1, Estado2) :-
    get_turno(Estado1, Turno1),
    get_turno(Estado2, Turno2),
    Turno1 == Turno2.

/*
    Evalua dos estados y elige el mejor teniendo en cuenta las condiciones de minimax
*/

% minimax_eval(+Estado1, +Val2, +Estado2, +Val2, -MejorEstado, -MejorVal).
%   Maquina: Color de la maquina.
%   Estado1, Val1: Estado de juego posible 1 y su valor correspondiente.
%   Estado2, Val2: Estado de juego posible 2 y su valor correspondiente.
%   MejorEstado, MejorVal: Mejor estado y mejor valor en las condiciones de Minimax (Depende del turno de los estados y el color de la maquina).
minimax_eval(Maquina, TurnoEstado, Estado1, Val1, Estado2, Val2, MejorEstado, MejorVal) :-
/*    get_turno(Estado1, terminado) ->
        (
            se_quiere_minimizar(Maquina, TurnoEstado),
            MejorEstado = Estado1,
            MejorVal = Val1
            ;
            MejorEstado = Estado2,
            MejorVal = Val2
        )
    ;
    get_turno(Estado2, terminado) ->
        (
            se_quiere_minimizar(Maquina, TurnoEstado),
            MejorEstado = Estado2,
            MejorVal = Val2
            ;
            MejorEstado = Estado1,
            MejorVal = Val1
        )
    ;*/

    %mismo_turno(Estado1, Estado2), % Control de que estamos en el mismo turno.
    (
        (
            se_quiere_minimizar(Maquina, TurnoEstado),
            %%writeln('Se quiere minimizar.' + TurnoEstado),
            minimo_val(Estado1, Val1, Estado2, Val2, MejorEstado, MejorVal)
        )
        ;
        (
            %%writeln('Se quiere maximizar.' + TurnoEstado),
            maximo_val(Estado1, Val1, Estado2, Val2, MejorEstado, MejorVal)
        )
    ).%; writeln('Otra vez este error')

/*
    Calcula un valor numerico a partir de el estado, el color de la maquina.
*/

% evalEstado(+Estado, +Maquina, -Val).
%   Estado: Estado del juego
%   Maquina: Color del turno para el cual se quiere evaluar.
%   Val: Valor del estado Estado para el jugador Maquina. 
evalEstado(Estado, Maquina, Val) :-
    % Posible calculo. Seguro se puede optimizar.
    ganador(Estado, Maquina) ->
        Val is 10000
    ;
    get_count_cells(Estado, Maquina, AFavor),
    get_count_oponent_cells(Estado, Maquina, EnContra),
    Val is AFavor - EnContra.

ganador(Estado, Maquina) :-
    get_turno(Estado, terminado),
    get_count_cells(Estado, Maquina, AFavor),
    AFavor > 0.

/*
    Compara y devuelve el minimo de dos Estados,Valores.
*/

% Estado1, Val1 son minimo si:
minimo_val(_, Val1, Estado2, Val2, Estado3, Val3) :-
    Val2 < Val1, !,
    Val3 = Val2, Estado3 = Estado2.
% else
minimo_val(Estado1, Val1, _, _, Estado1, Val1).

/*
    Compara y devuelve el maximo de dos Estados,Valores.
*/

% Estado1, Val1 son maximo si:
maximo_val(_, Val1, Estado2, Val2, Estado3, Val3) :-
    Val1 =< Val2, !, Val3 = Val2, Estado3 = Estado2.
% else
maximo_val(Estado1, Val1, _, _, Estado1, Val1).

/*
    Se quiere minimizar la maximizacion del oponente, no?
    Minimizo si el turno es de la maquina.
    Maximizo si el turno NO es de la maquina.
*/

% se_quiere_minimizar(?Maquina, ?TurnoActual)
%   Maquina: Color de la maquina
%   TurnoActual: Turno del actual.
se_quiere_minimizar(Maquina, TurnoActual) :-
    Maquina \== TurnoActual.

print_turno_estado(Nombre, Estado) :-
    get_turno(Estado, Turno),
    writeln(Nombre + Turno).

% jugada_posible(+Estado, -EstadoSiguiente)
jugada_posible(Estado, TurnoEstado, EstadoSiguiente) :-
    %%writeln('jugada_posible'),
    get_turno(Estado, TurnoActual),
    %%writeln('Turno del Estado: ' + TurnoActual + '. TurnoEstado: ' + TurnoEstado),

    obtener_matriz_estado(Estado, MatrizActual),
    obtener_dimension_matriz(Estado, Dimension),

    % Se copia el estado para no modificar el real.
    duplicate_term(Estado, EstadoSiguiente),

    % Cambia de turno.
    siguiente_turno(TurnoActual, TurnoSiguiente),
    set_turno(EstadoSiguiente, TurnoSiguiente),

    % Genera una matriz con una movida.
    get_turno(EstadoSiguiente, TurnoSig),
    TurnoActual \== TurnoSig,
    %%writeln('1------> El valor de la matriz creada para el turno ' + TurnoActual + ' es: ' + Val + '. El turno siguiente es:' + TurnoSig),
    jugada_posible_matriz(MatrizActual, TurnoActual, Dimension, MatrizSiguiente, EstadoSiguiente),
    /*writeln('jugada_posible - se obtiene matriz con jugada.'),*/
    actualizar_matriz(EstadoSiguiente, MatrizSiguiente),
    /*writeln('jugada_posible - se actualiza matriz'),*/
    %%writeln('2------> El valor de la matriz creada para el turno ' + TurnoActual + ' es: ' + Val + '. El turno siguiente es:' + TurnoSig),
    evalEstado(EstadoSiguiente, TurnoActual, Val).
    %%writeln('3------> El valor de la matriz creada para el turno ' + TurnoActual + ' es: ' + Val + '. El turno siguiente es:' + TurnoSig).


% jugada_posible_matriz(+Matriz, +Turno, -MatrizSiguiente)
jugada_posible_matriz(Matriz, Turno, Dimension, MatrizSiguiente, EstadoSiguiente) :-
    % uno u otro, no los dos.
    salto_posible(Matriz, Turno, Dimension, MatrizSiguiente, EstadoSiguiente);
    clonacion_posible(Matriz, Turno, Dimension, MatrizSiguiente, EstadoSiguiente).

salto_posible(Matriz, Turno, Dimension, MatrizSiguiente, EstadoSiguiente) :-
    /*writeln('salto_posible - Se busca salto posible.'),*/
    % obtengo una posicion X, Y con posible juego.
    get_val_matriz(Matriz, Turno, Dimension, X, Y),
    /*writeln('salto_posible - tengo posible jugador. Posicion:' + (X, Y)),*/
    % obtengo posicion L vacio distinto de 2.
    get_adj_vacios_dist(X, Y, Matriz, Dimension, 2, (Xsalto, Ysalto)),
    /*writeln('salto_posible - encuentro posible salto: ' + (Xsalto, Ysalto)),*/
    % Se copia la matriz para no modificar la real.
    duplicate_term(Matriz, MatrizSiguiente),
    set_cell(X, Y, MatrizSiguiente, vacio),
    set_cell(Xsalto, Ysalto, MatrizSiguiente, Turno),
	atomic_list_concat(['Jugador ', Turno, ' mueve de ', X, ',', Y, ' a ', Xsalto, ',', Ysalto],Msg),
	set_mensaje(EstadoSiguiente,Msg),
    infect_adj(Xsalto, Ysalto, MatrizSiguiente, Turno).%, writeln('salto_posible - uardo e infecto la matriz').


clonacion_posible(Matriz, Turno, Dimension, MatrizSiguiente, EstadoSiguiente) :-
    /*writeln('clonacion_posible - Se busca clonacion posible.'),*/
    % obtengo una posicion X, Y con posible juego.
    get_val_matriz(Matriz, Turno, Dimension, X, Y),
    /*writeln('clonacion_posible - tengo posible jugador. Posicion:' + (X, Y)),*/
    get_adj_vacios_dist(X, Y, Matriz, Dimension, 1, (Xclon, Yclon)),
    /*writeln('clonacion_posible - encuentro posible clon: ' + (Xclon, Yclon)),*/
    % Se copia la matriz para no modificar la real.
    duplicate_term(Matriz, MatrizSiguiente),
    set_cell(Xclon, Yclon, MatrizSiguiente, Turno),
	atomic_list_concat(['Jugador ', Turno, ' clona en ', Xclon, ',', Yclon],Msg),
	set_mensaje(EstadoSiguiente,Msg),
    infect_adj(Xclon, Yclon, MatrizSiguiente, Turno).%, writeln('clonacion_posible - Guardo e infecto la matriz').
