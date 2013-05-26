:- module(matrices, [matrix/4, get_cell/4, set_cell/4, count_cells/3]).

% Crea una matriz X,Y con todos sus valores seteados a Val.
matrix(X, Y, Val, M) :- 
        row(Y, Val, R),
        lista(R, X, C),
        M =.. [matrix | C].

% Devuelve el numero de filas.
get_num_rows(M, X) :-
        M =.. [matrix | R],
        length(R, X).

% Devuelve el numero de columnas.
get_num_cols(M, X) :-
        row(Y, Val, R),
        R =.. [row | C],
        length(C, X).

% Devuelve en R una fila de largo Y, con todos los valores seteados en Val.
row(Y, Val, R) :- 
        lista(Val, Y, L), 
        R =.. [row| L].

% Devuelve en L una lista de largo N
lista(_, 0, []).
lista(Val, N, L) :- 
        N > 0,
        M is N - 1,
        duplicate_term(Val, NewVal),
        L = [Val| X],
        lista(NewVal, M, X).

% Devuelve el valor de la posicion (X, Y) de la matriz M.
get_cell(X, Y, M, Val) :-
        arg(X, M, F), % Devuelve en F la Row X
        arg(Y, F, Val). % Devuelve en Val el elemento de la columna Y de F
        
% Setea el valor de la posicion (X, Y) de la matriz M.
set_cell(X, Y, M, Val) :-
        arg(X, M, F),
        setarg(Y, F, Val).

% Cuenta la cantidad de ocurrencias del valor Val en la matriz M.
count_cells(M, Val, Count) :-
        M =.. [matrix | Lista], 
        count_val_rows(Lista, Val, Count).
        
% Cuenta la cantidad de ocurrencias del valor Val en una lista de row(x,y,z).
count_val_rows([], _, 0).
count_val_rows([R | T], Val, Count) :-
        count_val_rows(T, Val, Subcount),
        count_val_row(R, Val, X),
        Count is X + Subcount.

% Cuenta la cantidad de ocurrencias del valor Val en una row(x,y,z).
count_val_row(R, Val, Count) :-
        R =.. [row | Lista],
        count_val_list(Val, Lista, Count).

% Cuenta la cantidad de ocurrencias del valor Val en una lista.
count_val_list(_, [], 0).
count_val_list(X, [X|T], N) :-
        count_val_list(X, T, N2),
        N is N2 + 1.
count_val_list(X, [H|T], N) :-
        X \= H,
        count_val_list(X, T, N).
