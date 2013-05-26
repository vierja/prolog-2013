:- module(matrices, [matrix/4, get_cell/4, set_cell/4, count_cells/3]).

matrix(X, Y, Val, M) :- 
		row(Y, Val, R),
		lista(R, X, C),
		M =.. [matrix | C].

% Devuelve en R una fila de largo Y, con todos los valores seteados en Val.
row(Y, Val, R) :- lista(Val, Y, L), R =.. [row| L].

% Devuelve en L una lista de largo N
lista(_, 0, []).
lista(Val, N, L) :- 
		N > 0,
		M is N - 1,
		duplicate_term(Val, NewVal),
		L = [Val| X],
		lista(NewVal, M, X).


get_cell(X, Y, M, Val) :-
		arg(X, M, F), % Devuelve en F la Row X
		arg(Y, F, Val). % Devuelve en Val el elemento de la columna Y de F
		

set_cell(X, Y, M, Val) :-
		arg(X, M, F),
		setarg(Y, F, Val).


count_cells(M, Val, Count) :-
		M =.. [matrix | Lista], count_val_rows(Lista, Val, Count).
		
count_val_rows([], _, 0).
count_val_rows([R | T], Val, Count) :-
		count_val_rows(T, Val, Subcount),
		count_val_row(R, Val, X),
		Count is X + Subcount.


count_val_row(R, Val, Count) :-
		R =.. [row | Lista], count_val_list(Val, Lista, Count).

count_val_list(_, [], 0).
count_val_list(X, [X|T], N) :-
		count_val_list(X, T, N2),
		N is N2 + 1.
count_val_list(X, [H|T], N) :-
		X \= H,
		count_val_list(X, T, N).
