% Predicados gráficos para la segunda tarea de 
% Programación Lógica 2013 - Infectlog
% 
% Este archivo no debe ser modificado, salvo para ayudar a depurar.
% A medida que se encuentren errores y se sugieran mejoras van a haber 
% nuevas versiones.
%
%


:- module(graficos,
[
 gr_crear/4, % -Ventana +Botones +Filas +Columnas
 % Devuelve un handle a la Ventana creada.
 % En botones viene una lista de boton(Nombre, Evento)
 % indicando los botones a crear.
 % Las dimensiones iniciales son Filas*Columnas.
 
 gr_destruir/1, % +Ventana
 % Cierra la ventana e invalida el handle.
 
 gr_dimensiones/3, % +Ventana +Filas +Columnas
 % Cambia el tamaño del tablero de la ventana Ventana,
 % poniendo todas las casillas vacías.

 gr_ficha/4, % +Ventana +Fila +Columna +Ficha
 % Pinta la ficha Ficha en la casilla (Fila, Columna) de la ventana
 % Ventana. Ficha puede ser 'blanco' o 'negro' para colocar una
 % ficha de ese color o 'vacio' para sacar la ficha.
 
 gr_evento/2, % +Ventana ?Evento
 % Devuelve en Evento la acción del usuario, que puede ser 
 % click(Fila, Col), salir o
 % el del boton accionado.

 gr_mensaje/2, % +Ventana +String
 % Muestra String en una ventanita auxiliar,
 % quedando la aplicación a la espera de que ésta se cierre.

 gr_pregunta/3, % +Ventana +Pregunta ?Respuesta
 % Muestra una ventanita conteniendo Pregunta
 % y un espacio para que el usuario ingrese Respuesta.
 % Se regresa del predicado cuando el usuario selecciona el botón.
 % El predicado falla si se cierra el dialogo.
 
 gr_opciones/4, % +Ventana +Pregunta +Opciones ?Respuesta
 % Muestra una ventanita conteniendo Pregunta
 % y un botón por cada elemento de Opciones,
 % para que elija el usuario.
 % Se regresa del predicado cuando el usuario selecciona un botón,
 % devolviendo el elegido en Respuesta
 % El predicado falla si se cierra el dialogo.

 gr_estado/2 , % +Ventana +NuevoEstado
 % Muestra el NuevoEstado de la partida en la parte inferior 
 % de la pantalla.

 gr_purgar/0
 % Cierra todas las ventanas que pueden haber quedado abiertas
 % por fallos del programa. Hack, usar solo en desarrollo.
]).


:- use_module(library(tabular)).
:- use_module(library(autowin)).
:- use_module(library(dragdrop)).

%La clase root.
:- pce_begin_class(my_frame, frame).

variable(queue, any, both).
variable(image, image, both).
variable(dialog, dialog, both).

%Desactivamos que el usuario la cierre.
%En su lugar, mandamos un mensaje salir.
wm_delete(Self) :->
	get(Self, queue, Queue),
	thread_send_message(Queue, salir).

:- pce_end_class.

tam_casilla(F, C) :-
	new(Vacio,image('vacio.gif')),
	get(Vacio, size, Size),
	get(Size, height, F),
	get(Size, width, C).

click_tablero(Q, Punto) :-
	get(Punto, y, FV),
	get(Punto, x, CV),
	tam_casilla(FC, CC),
	F is 1 + FV // FC,
	C is 1 + CV // CC,
	thread_send_message(Q, click(F, C)).

:- send(class(my_frame), record_instances).
gr_purgar :-
	get(class(my_frame), instances, I),
	send(I, for_all, message(@arg1, destroy)).

gr_crear(Frame, Botones, Filas, Columnas) :-
	message_queue_create(Q),
	new(Frame, my_frame('InfectLog')),
	new(W, auto_sized_dialog),

	send(Frame, can_resize,	@off),
	forall(member(boton(Txt, Val), Botones),
	       send(W, append, 
		    button(Txt, 
			   message(@prolog,
				   thread_send_message,
				   prolog(Q),
				   prolog(Val))))),
	send(W, max_size, size(1000, 1200)),
	new(I, image(kind := pixmap)),
	new(B, bitmap(I)),
	send(B, recogniser, 
	     click_gesture(left, '', single, 
			      message(@prolog,
				      click_tablero,
				      prolog(Q),
				      @event?position))),
	send(W, append, B),
	send(W, append, label(reporter)),
	send(Frame, append, W),
	send(Frame, queue, prolog(Q)),
	send(Frame, image, I),
	send(Frame, dialog, W),
	gr_dimensiones(Frame, Filas, Columnas),
	send(Frame, open).

gr_destruir(Ventana) :-
	get(Ventana, queue, Q),
	message_queue_destroy(Q),
	send(Ventana, destroy).

gr_dimensiones(Ventana, F, C) :-
	tam_casilla(SF, SC),
	MF is SF*F,
	MC is SC*C,
	get(Ventana, image, I),
	send(I, resize, MC, MF),
	get(Ventana, dialog, W),
	send(W, redraw),
	send(W, fit),
	(   between(1, F, FF),
	    between(1, C, CC),
	    gr_ficha(Ventana, FF, CC, vacio),
	    fail
	;   true
	),
	!.

gr_ficha(Ventana, F, C, Ficha) :-
	tam_casilla(SF, SC),
	F3 is (F-1)*SF,
	C3 is (C-1)*SC,
	atom_concat(Ficha, '.gif', Arch),
	new(ImgFicha,image(Arch)),
	get(Ventana, image, I),
	send(I, draw_in, bitmap(ImgFicha), point(C3, F3)),
	send(Ventana, flush),
	!.

gr_evento(Ventana, Input) :-
	get(Ventana, queue, Q),
	thread_get_message(Q, Aux),
	!,
	Input = Aux.

gr_mensaje(V, Texto) :-
	new(D, dialog('Mensaje')),
	send(D, transient_for, V),
	send(D, append, label(lab, Texto)),
	send(D, append, button(ok,
			       message(D, return, @nil))),
	send(D, default_button, ok), % Ok: default button
	(   get(D, confirm, _Answer) % This blocks!
	->  send(D, destroy)
	;   true
	).

gr_pregunta(V, Preg, Resp) :-
	new(D, dialog('Pregunta')),
	send(D, transient_for, V),
        send(D, append,
             label(lab, Preg)),
	send(D, append,
             new(TI, text_item('', ''))),
        send(D, append,
             button(ok, message(D, return,
                                TI?selection))),
        send(D, default_button, ok), % Ok: default button
        get(D, confirm, Answer),     % This blocks!
        send(D, destroy),
	Answer = Resp.

gr_opciones(V, Texto, Opciones, Resp) :-
	new(D, dialog('Opciones')),
	send(D, transient_for, V),
	send(D, append, label(lab, Texto)),
	forall(member(O, Opciones),
	       send(D, append, button(O,
			       message(D, return, O)))),
	get(D, confirm,Answer),
	send(D, destroy),
	Resp = Answer.

gr_estado(MV,NuevoEstado) :-
	send(MV, report, progress,'%s',NuevoEstado).

