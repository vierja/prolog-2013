:- use_module(graficos).
:- use_module(matrices).
:- use_module(infect_matriz).
:- use_module(infect_matgraf).
:- use_module(estadoJuego).
:- use_module(minimax).

/*
    Este archivo se provee como una guía para facilitar la implementación y 
    entender el uso de graficos.pl
    El contenido de este archivo se puede modificar.
*/
	
main :-
    new(Frame, my_frame('Biencenido a Infectlog Grupo: 12')),
    gr_opciones(Frame, 'Elija el tamaño del tablero:', ['5x5', '6x6', '7x7', '8x8', '9x9'], Resp),
    (
        Resp == '5x5' -> infectBot(5);
        Resp == '6x6' -> infectBot(6);
        Resp == '7x7' -> infectBot(7);
        Resp == '8x8' -> infectBot(8);
        Resp == '9x9' -> infectBot(9)      
    ).

infectBot(Dim) :-
	new(Frame, my_frame('Biencenido a Infectlog Grupo: 12')),
    gr_opciones(Frame, 'Elija el nivel de dificultad de InfectBot:', ['Easy', 'Medium', 'Hard'], Resp),
    (
        Resp == 'Easy' -> infectlog(Dim,2);
        Resp == 'Medium' -> infectlog(Dim,3);
        Resp == 'Hard' -> infectlog(Dim,4)    
    ).
	
% infectlog
infectlog(Dim, Depth) :-
    gr_crear(Visual, [
             boton('Reiniciar',reiniciar),
             boton('Salir',salir)], % salir puede ser por el boton o por el click en la ventana
         Dim,Dim),
    estado(Matriz,Dim,blanco,Negras,Blancas,_,Estado),
    dibujar_matriz(Estado, Visual),
    sformat(Msg, 'Inicio de juego: Humano(B) vs InfectBot(N)'),
    gr_estado(Visual, Msg),
    loop(Visual, Estado, Depth),
    gr_destruir(Visual).

turno_maquina(Estado) :-
    get_turno(Estado, negro).

% loop(+Visual, +Estado, Depth)
loop(Visual, Estado, Depth) :-
    termino_juego(Estado) ->
        obtener_ganador(Estado,Ganador),
        dibujar_matriz(Estado, Visual),
		(
			Ganador == blanco ->
			sformat(Msg,'Juego terminado, Ganador: Humano!')
			;
			Ganador == negro ->
			sformat(Msg,'Juego terminado, Ganador: InfectBot!')
			;
			sformat(Msg,'Juego terminado, Resultado: Emapte!')
		),
        gr_estado(Visual, Msg),
        gr_evento(Visual,E),
        evento(E,Visual, Estado, Depth)
        ;
    dibujar_matriz(Estado, Visual),
    turno_maquina(Estado) ->
        minimax(Estado, negro, 0, SigEstado, Valor, Depth),
        % No se como reemplazar al estado.
		get_mensaje(SigEstado,Msg2),
		gr_estado(Visual, Msg2),
		obtener_matriz_estado(SigEstado,M),
		(
			findall(X,get_celdas_valor(M,blanco,X),Blancas),
			Blancas == [] -> true;
			tiene_movimiento(SigEstado,blanco)->true;
			completar_matriz(M),
			actualizar_estado(SigEstado)
		),
        loop(Visual, SigEstado, Depth)
    ;
    gr_evento(Visual,E),
    evento(E,Visual, Estado, Depth).

% evento(+Event,+Visual,+Estado)
evento(click(Fila,Columna),Visual, Estado, Depth) :-
    termino_juego(Estado) -> loop(Visual, Estado, Depth)
    ;    
    movimiento(Visual, Fila, Columna, Estado),
    loop(Visual, Estado, Depth).

evento(salir,Visual, Estado, Depth) :-
    (   
        gr_opciones(Visual, '¿Seguro?', ['Sí', 'No'], 'Sí') 
        ->
        true;
        loop(Visual, Estado, Depth)
    ).
    
evento(reiniciar,Visual, Estado, Depth) :-
    (   
        gr_opciones(Visual, '¿Seguro?', ['Sí', 'No'], 'Sí') 
        ->
        obtener_matriz_estado(Estado,Matriz),
        resetear_matriz(Matriz),
        dibujar_matriz(Estado, Visual),
        set_turno(Estado, blanco),
        actualizar_estado(Estado),
		sformat(Msg, 'Inicio de juego: Humano(B) vs InfectBot(N)'),
		gr_estado(Visual, Msg),
        loop(Visual, Estado, Depth)
        ;
        loop(Visual, Estado, Depth)
    ).


gr_ficha_lista(_,[],_).
gr_ficha_lista(Visual,[(X,Y)|T],Imagen) :-
    gr_ficha(Visual,X,Y, Imagen),gr_ficha_lista(Visual,T,Imagen).

movimiento(Visual, Fila, Columna, Estado) :-
	obtener_matriz_estado(Estado,Matriz),
    obtener_dimension_matriz(Estado,Dim),
    get_turno(Estado,Turno),
	tiene_movimiento(Estado,Turno) ->
	(
    get_cell(Fila, Columna, Matriz, Val), writeln('Valor en click: ' + Val),
    ( 
        Turno == Val ->
        (
            gr_ficha(Visual,Fila,Columna, negro_selected),
            (
                findall(X,get_adj_vacios_dist(Fila,Columna,Matriz,Dim,1,X),L1),
                gr_ficha_lista(Visual,L1,posible_clonar),
                findall(Y,get_adj_vacios_dist(Fila,Columna,Matriz,Dim,2,Y),L2),
                gr_ficha_lista(Visual,L2,posible_saltar)
            )
        ),
        (
            gr_evento(Visual, click(FilaDest,ColumnaDest)), 
            writeln('Posicion segundo click: ' + '(' + FilaDest +','+ ColumnaDest+')'),
            distance(Fila,Columna,FilaDest,ColumnaDest,Distancia),
            get_cell(FilaDest, ColumnaDest, Matriz, DestVal),
            (
                DestVal \= vacio -> 
                (                   
                    sformat(Msg, 'Movimiento invalido.'),
                    gr_estado(Visual, Msg)
                )
                ;
                (
                    Distancia == 1,
                    clone(Visual, Fila, Columna, FilaDest, ColumnaDest, Estado);
                    (
                        Distancia == 2,
                        jump(Visual, Fila, Columna, FilaDest, ColumnaDest, Estado);
                        (
                            sformat(Msg, 'Movimiento invalido.'),
                            gr_estado(Visual, Msg)
                        )
                    )
                )               
            )
        )
        ;
        (   
            Val == vacio -> writeln('Se hace click en vacio.')
            ;
            (
                sformat(Msg, 'Movimiento invalido.'),
                gr_estado(Visual, Msg)
            )
        )
    )
	)
	;
	(
		siguiente_turno(Turno, SigTurno),
        set_turno(Estado,SigTurno)
	)
	.

jump(Visual, Fila, Columna, FilaDest, ColumnaDest, Estado) :-
    writeln('jump'),
    obtener_matriz_estado(Estado,Matriz),
    get_turno(Estado,Turno),
    (   
        set_cell(Fila, Columna, Matriz, vacio),
        gr_ficha(Visual,Fila,Columna, vacio),
        set_cell(FilaDest, ColumnaDest, Matriz, Turno),
        gr_ficha(Visual,FilaDest,ColumnaDest, Turno),
        infect_adj(FilaDest, ColumnaDest, Matriz, Turno),
        atomic_list_concat(['Humano mueve de ', Fila, ',', Columna, ' a ', FilaDest, ',', ColumnaDest,'   Pensando...'],Msg),
        gr_estado(Visual, Msg),
        siguiente_turno(Turno, SigTurno),
        set_turno(Estado,SigTurno),
        actualizar_estado(Estado),
        (
            termino_juego(Estado) -> true
            ;
            tiene_movimiento(Estado,SigTurno) -> true
            ;
            siguiente_turno(SigTurno, SigTurno2),
            set_turno(Estado,SigTurno2),
            actualizar_estado(Estado)
        )
    ).


clone(Visual, Fila, Columna, FilaDest, ColumnaDest, Estado) :-
    writeln('clone'),
    obtener_matriz_estado(Estado,Matriz),
    get_turno(Estado,Turno),
    (       
        set_cell(FilaDest, ColumnaDest, Matriz, Turno),
        gr_ficha(Visual,FilaDest,ColumnaDest, Turno),
        infect_adj(FilaDest, ColumnaDest, Matriz, Turno),
        atomic_list_concat(['Humano clona en ', FilaDest, ',', ColumnaDest,'   Pensando...'],Msg),
        gr_estado(Visual, Msg),
        siguiente_turno(Turno, SigTurno),
        set_turno(Estado,SigTurno),
        actualizar_estado(Estado),
        (
            termino_juego(Estado) -> true;
            tiene_movimiento(Estado,SigTurno) -> true;
            siguiente_turno(SigTurno, SigTurno2),
            set_turno(Estado,SigTurno2),
            actualizar_estado(Estado)
        )
    ).

tiene_movimiento(Estado, Turno) :-
    obtener_matriz_estado(Estado,Matriz),
    obtener_dimension_matriz(Estado,Dim),
    (
        puedo_clonar(Matriz,Dim,Turno);
        puedo_saltar(Matriz,Dim,Turno)
    ).

obtener_ficha_turno(Matriz,Dim,Turno,Ficha) :-
    (
        between(1,Dim,FF),
        between(1,Dim,CC),
        get_cell(FF,CC,Matriz,Val),
        Val == Turno,
        Ficha = (FF,CC)
    ).

obtener_fichas_turno(Matriz,Dim,Turno,Fichas) :-
    (
        findall(X,obtener_ficha_turno(Matriz,Dim,Turno,X),Fichas)
    ).

puedo_clonar(Matriz,Dim,Turno) :-
    obtener_fichas_turno(Matriz,Dim,Turno,FichasTurno),
    member((X,Y),FichasTurno),
    puedo_clonar_ficha(X,Y,Matriz,Dim).

puedo_saltar(Matriz,Dim,Turno) :-
    obtener_fichas_turno(Matriz,Dim,Turno,FichasTurno),
    member((X,Y),FichasTurno),
    puedo_saltar_ficha(X,Y,Matriz,Dim).
    
puedo_clonar_ficha(Fila, Columna, Matriz,Dim) :-
    findall(X, get_adj_value_dist(Fila, Columna, Matriz, Dim, 1, X), ListaTrios),
    member((_,_,vacio),ListaTrios).

puedo_saltar_ficha(Fila, Columna, Matriz, Dim) :-
    findall(X, get_adj_value_dist(Fila, Columna, Matriz, Dim, 2, X), ListaTrios),
    member((_,_,vacio),ListaTrios).