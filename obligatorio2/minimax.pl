:- module(minimax, [minimax/4]). 

:- use_module(estadoJuego).

minimax_depth(4).

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
minimax(Estado, Maquina, NivelRecusion, Estado, Val) :-
    minimax_depth(NivelRecusion),
    evalEstado(MejorEstado, Maquina, Val).

% minimax(-Estado, -Maquina, -NivelRecursion, +MejorEstado, +Val).
%   Estado: Estado del juego (Matriz, Turno, ...).
%   Maquina: Color de la maquina.
%   NivelRecursion: Nivel de recursion actual del algoritmo.
%   MejorEstado: Mejor estado calculado por el algoritmo.
%   Valor: Valor numerico del Estado devuelto.
%   RecursionLevel: Nivel de recusion.
minimax(Estado, Maquina, NivelRecusion, MejorEstado, Val) :-
    bagof(EstadoSiguiente, jugada_posible(Estado, EstadoSiguiente), ListaEstadoSiguiente),
    mejor_estado(ListaEstadoSiguiente, Maquina, MejorEstado, Val, NivelRecusion), !
    ;
    evalEstado(MejorEstado, Maquina, Val).     % Pos has no successors -> evaluate the positition

/*
    Compara de forma recursiva todos los elementos de la lista.
*/

% mejor_estado(+ListaEstadoSiguiente, +Maquina, -MejorEstado, -Val).
%   ListaEstadoSiguiente: Lista de estados de jugadas posibles siguientes.
%   Maquina: Color de la maquina.
%   MejorEstado: El mejor estado en las condiciones de Minimax (depende del color).
%   Val: El valor numero del mejor estado.
mejor_estado([MejorEstado], Maquina, MejorEstado, Val, NivelRecusion) :-
    % No importa el siguiente estado del siguiente estado del siguiente estado (etc..). Solo importa el valor.
    SigNivelRecursion is NivelRecusion + 1,
    minimax(MejorEstado, Maquina, SigNivelRecursion, _, Val), !. 

mejor_estado([Estado1 | ListaEstado], Maquina, MejorEstado, MejorVal) :-
    SigNivelRecursion is NivelRecusion + 1,
    minimax(Estado1, Maquina, SigNivelRecursion, _, Val1),
    mejor_estado(ListaEstado, Maquina, Estado2, Val2),
    minimax_eval(Maquina, Estado1, Val1, Estado2, Val2, MejorEstado, MejorVal).

/*
    Evalua dos estados y elige el mejor teniendo en cuenta las condiciones de minimax
*/

% minimax_eval(+Estado1, +Val2, +Estado2, +Val2, -MejorEstado, -MejorVal).
%   Maquina: Color de la maquina.
%   Estado1, Val1: Estado de juego posible 1 y su valor correspondiente.
%   Estado2, Val2: Estado de juego posible 2 y su valor correspondiente.
%   MejorEstado, MejorVal: Mejor estado y mejor valor en las condiciones de Minimax (Depende del turno de los estados y el color de la maquina).
minimax_eval(Maquina, Estado1, Val1, Estado2, Val2, MejorEstado, MejorVal) :-
    Estado1 == Estado2, % Control de que estamos en el mismo turno.
    (
        get_turno(Estado1, TurnoActual),
        se_quiere_minimizar(Maquina, TurnoActual),
        minimo_val(Estado1, Val1, Estado2, Val2, MejorEstado, MejorVal)
        ;
        maximo_val(Estado1, Val1, Estado2, Val2, MejorEstado, MejorVal)
    ).

/*
    Calcula un valor numerico a partir de el estado, el color de la maquina.
*/

% evalEstado(+Estado, +Maquina, -Val).
%   Estado: Estado del juego
%   Maquina: Color del turno para el cual se quiere evaluar.
%   Val: Valor del estado Estado para el jugador Maquina. 
evalEstado(Estado, Maquina, Val):-
    % Posible calculo. Seguro se puede optimizar.
    get_count_cells(Estado, Maquina, AFavor),
    get_count_oponent_cells(Estado, Maquina, EnContra),
    Val is AFavor - EnContra.

/*
    Compara y devuelve el minimo de dos Estados,Valores.
*/

% Estado1, Val1 son minimo si:
minimo_val(Estado1, Val1, _, Val2, Estado1, Val1) :-
    Val1 < Val2.
% else
minimo_val(_, _, Estado2, Val2, Estado2, Val2).

/*
    Compara y devuelve el maximo de dos Estados,Valores.
*/

% Estado1, Val1 son maximo si:
maximo_val(Estado1, Val1, _, Val2, Estado1, Val1) :-
    Val1 => Val2.
% else
maximo_val(_, _, Estado1, Val2, Estado2, Val2).

/*
    Se quiere minimizar la maximizacion del oponente, no?
    Minimizo si el turno es de la maquina.
    Maximizo si el turno NO es de la maquina.
*/

% se_quiere_minimizar(?Maquina, ?TurnoActual)
%   Maquina: Color de la maquina
%   TurnoActual: Turno del actual.
se_quiere_minimizar(Maquina, TurnoActual) :-
    Maquina == TurnoActual.


