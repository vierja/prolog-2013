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
    loop(Visual, Matriz, blanco),
    gr_destruir(Visual).

% loop(+Visual, +Matriz)
loop(Visual, Matriz, Turno) :-
    dibujar_matriz(Matriz, Visual),
    gr_evento(Visual,E),
    evento(E,Visual, Matriz, Turno).

% evento(+Event,+Visual)
evento(click(Fila,Columna),Visual, Matriz, Turno) :-
    /*gr_ficha(Visual,Fila,Columna,Turno),
    set_cell(Fila,Columna,Matriz,Turno),*/
    movimiento(Visual, Fila, Columna, Matriz, Turno, SigTurno),
    loop(Visual, Matriz, SigTurno).

evento(salir,Visual, Matriz, Turno) :-
    (   gr_opciones(Visual, '¿Seguro?', ['Sí', 'No'], 'Sí')
    ->  true
    ;   loop(Visual, Matriz, Turno)
    ).
evento(reiniciar,Visual, Matriz, Turno) :-
    (   gr_opciones(Visual, '¿Seguro?', ['Sí', 'No'], 'Sí')
    ->  % reiniciar el juego
        init_matriz(7,7, Matriz),
        loop(Visual, Matriz, blanco)
    ;   loop(Visual, Matriz, Turno)
    ).

siguiente_turno(Actual, Siguiente) :-
        ( Actual == blanco,
          Siguiente = negro;
          Siguiente = blanco
        ).

% Movimiento. Al recibir un click. Esta funcion se encarga de realizar el salto, o de clonar.
movimiento(Visual, Fila, Columna, Matriz, Turno, SigTurno) :-
        get_cell(Fila, Columna, Matriz, Val), writeln('Valor en click: ' + Val),
        ( Turno == Val -> % Si el valor donde se hizo click, es la ficha del turno. 
            gr_ficha(Visual,Fila,Columna, negro_selected), % La pinto de otro color.
            gr_evento(Visual, click(FilaDest,ColumnaDest)), writeln('Posicion segundo click: ' + (FilaDest, ColumnaDest)), % Espero otro click.
            jump(Visual, Fila, Columna, FilaDest, ColumnaDest, Matriz, Turno, SigTurno);
            (   Val == vacio ->
                    writeln('Se hace click en vacio.'),
                    clone(Visual, Fila, Columna, Matriz, Turno, SigTurno);
                % Turno /== Val Si se hizo click en una ficha de color incorrecto, error.
                    writeln('Se hace click en color del otro'),
                    sformat(Msg, 'Movimiento invalido.'),
                    gr_estado(Visual, Msg),
                    SigTurno = Turno
            )
        ).

jump(Visual, Fila, Columna, FilaDest, ColumnaDest, Matriz, Turno, SigTurno) :-
        writeln('jump'),
        get_cell(FilaDest, ColumnaDest, Matriz, DestVal),
        (       DestVal == vacio,
                distance(Fila, Columna, FilaDest, ColumnaDest, Distance),
                writeln('El segundo click se encuentra a una distancia de ' + Distance),
                Distance =< 2,
                set_cell(Fila, Columna, Matriz, vacio),
                set_cell(FilaDest, ColumnaDest, Matriz, Turno),
                infect_adj(FilaDest, ColumnaDest, Matriz, Turno),
                sformat(Msg, 'Jugador mueve de un lugar a otro'),
                gr_estado(Visual, Msg),
                siguiente_turno(Turno, SigTurno);

            sformat(Msg, 'Movimiento invalido.'),
            gr_estado(Visual, Msg),
            SigTurno = Turno
        ).


% Si es un movimiento valido, realiza una clonacion del color Turno, en la Fila y Columna especificada.
clone(Visual, Fila, Columna, Matriz, Turno, SigTurno) :-
        (
            writeln('Clonar'),
            get_cell(Fila, Columna, Matriz, vacio), % Chequeo que la fila este vacia.
            findall(X, get_adj_value(Fila, Columna, Matriz, X), Lista), % Obtengo la lista de las adjacentes. TODO:Obtiene tambien el valor X,Y aunque no cambia el comportamiento.
            member(Turno, Lista), % Chequeo que al menos una de las adjacentes tenga el valor.
            set_cell(Fila, Columna, Matriz, Turno), % Seteo el valor.
            infect_adj(Fila, Columna, Matriz, Turno),
            sformat(Msg, 'Jugador clona'),
            gr_estado(Visual, Msg),
            siguiente_turno(Turno, SigTurno);

            writeln('Clonacion invalida.'),
            sformat(Msg, 'Clonacion invalida.'),
            gr_estado(Visual, Msg),
            SigTurno = Turno
        ).