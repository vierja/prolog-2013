% Pablo Anzorena  - 4.649.265-3
% Bruno Olivera   - 4.255.728-9
% Javier Rey      - 4.549.396-3

:- module(grupo12, [nth/3, sublist_n/3, max/3, palindromo/1, merge/3, member_sorted/2, permutation/2, selection_sort/2, matrix/4, select_column/3, transpose/2, symmetric/1, get_cell/4, set_cell/5, count_cells/3]).

% Predicados simples sobre listas.

/*
nth(?L,+Pos,?E)
`E` es el elemento en la posicion `Pos` (contando a partir de 1) de la
lista `L`.
Ej. nth(2, [a,b,c], b)
Ej corregido: nth([a,b,c], 2, b)
*/

nth([X|_], 1, X).
nth([_|Xs], Pos, X) :-
        Pos > 0,
        Prev is Pos - 1,
        nth(Xs, Prev, X).


/*
sublist_n(?L,+N,?Sub)
`Sub` es una sublista de largo `N` de la lista `L`.
Ej. sublist(2, [5,2,3,1,7], [3,1])
Ej corregido: sublist_n([5,2,3,1,7], 2, [3,1])
*/
sublist_n(L,N,S) :- prefijo(L,P),sufijo(P,S),largo_lista(S,N).

/*
sufijo(?T1,?T2) devuelve en T2 los sufijos de T1.
*/
sufijo(T,T).
sufijo([_|T], T2) :- sufijo(T,T2).
/*
prefijo(?T1,?T2) devuelve en T2 los prefijos de T1.
*/
prefijo(_,[]).
prefijo([H|T1],[H|T2]) :- prefijo(T1,T2).
/*
largo_lista(+L,+N) N es el largo de la lista L
*/
largo_lista([],0).
largo_lista([_|Xs],N) :- succ(M,N), largo_lista(Xs,M).

/*
max(+L,?Max,?L1)
`Max` es el maximo de `L`, `L1` es la lista `L` sin Max.
Ej. max([2,4,3], 4, [2,3]).
*/

max([X], X, []).
max(X, Max, L1) :-
        max_list(X, Max),
        remove_elem(X, Max, L1).

/*
max_list(+L, ?Max).
`Max` es el maximo de `L`.
Ej. max_list([2,4,3], 4).
*/


max_list([X|Xs], Max) :-
        max_list(Xs, X, Max).
max_list([X|Xs], Cand, Rest):-
        X =< Cand, max_list(Xs, Cand, Rest);
        X > Cand, max_list(Xs, X, Rest).
max_list([], Cand, Cand).

/*
palindromo(+L)
La lista `L` es un palindromo.
Ej. palindromo([a,b,c,b,a])
*/

palindromo(X) :-
        reverse_aux(X, X).
/*
reverse_aux(X,L,Y) devuelve el reverso de la lista X en Y.
*/
reverse_aux(X, Y) :-
        reverse_aux(X, [], Y).

reverse_aux([X|Xs],Z,Rev) :- reverse_aux(Xs, [X|Z], Rev).
reverse_aux([], Z, Z).

% Predicados avanzados sobre listas.

/*
merge(+L1,+L2,?L)
`L` es la lista producto de combinar ordenadamente las listas ordenadas
`L1` y `L2`.
*/

merge([], [], []).
merge([X|Xs], [], [X|Xs]).
merge([], [X|Xs], [X|Xs]).
merge([X|Xs], [Y|Ys], [X|Zs]) :-
        X =< Y,
        merge(Xs, [Y|Ys], Zs).
merge([X|Xs], [Y|Ys], [Y|Zs]) :-
        X > Y,
        merge([X|Xs], Ys, Zs).

/*
member_sorted(+L,+X)
`X` es un elemento de la lista ordenada `L`, no se debe recorrer la lista
innecesariamente.
*/

member_sorted([X|Xs], X) :-
    	sorted(Xs).
member_sorted([X,Y|Xs], Elem) :-
        X =< Y,X\==Elem,
        member_sorted([Y|Xs], Elem).

/*
sorted(+L)
verifica que la lista L este ordenada de forma ascendente
*/
sorted([]).
sorted([_|[]]).		
sorted([X,Y|Xs]) :- 
		X =< Y, 
		sorted([Y|Xs]).

/*
permutation(+X,?Y)
La lista `Y` es una permutacion de los elementos de la lista `X`.

Se define como `permutacion` porque `permutation` ya existe.
*/

permutation([], []).
permutation(X, [Y|Ys]) :-
        remove_elem(X, Y, Zs),
        permutation(Zs, Ys).

/*
remove_elem(+L, +Elem, ?LsinElem)
`LsinElem` es una lista igual a `L` sin la primera ocurrencia del elemento `Elem`.
Ej. remove_elem([1,2,2],2,[1,2]).

*/

remove_elem([A|C], A, C).
remove_elem([A|C], B, [A|D]) :-
        remove_elem(C, B, D).

/*
selection_sort(+L,?S)
`S` es la lista ordenada de `L` utilizando el algoritmo de selection sort.
*/

selection_sort([], []).
selection_sort(X, [Y|Ys]) :-
    min(X, Y),
    remove_elem(X, Y, Zs),
    selection_sort(Zs, Ys).

/*
min(+L, ?Min)
`Min` es el minimo elemento de la lista `L`.
Ej. min([7,4,1,4], 1).
Utiliza un wrapper (min) para para utilizar un acumulador.
*/

min([X|Xs], Min) :-
        min(Xs, X, Min).
min([X|Xs], Cand, Rest):-
        X >= Cand, min(Xs, Cand, Rest);
        X < Cand, min(Xs, X, Rest).
min([], Cand, Cand).

/*
matrix(+X,+Y,+Val,?M)
M es una matriz de X filas e Y columnas. Cada celda debe tener el valor Val.
La matriz se representa mediante una lista de X filas, donde cada fila es una lista de Y celdas.
*/

matrix(0, _, _, []).
matrix(_, 0, _, []).

matrix(X, Y, Val, [H|T]) :-
        succ(W, X),
        member_length(Y, Val, H),
        matrix(W, Y, Val, T).
/*
member_length(+X, +Aux, +L).
Verifica que el largo de la lista L sea igual a X y que los elementos
de la lista sean Aux.
*/
member_length(0, _, []).

member_length(X, Aux, [Aux|T]) :-
        succ(Z, X),
        member_length(Z, Aux, T).

/*
select_column(?M,?C,?MRest)
C es la primera columna de la matriz M, MRest es la matriz M sin su primera columna.
*/

select_column([ [H | T] | R], [H | C], [T | Rest]) :- select_column(R, C, Rest).
select_column( [], [], []).

/*
transpose(+M1,?M2)
M1 y M2 son matrices transpuestas
*/
:- redefine_system_predicate(transpose(_,_)).

transpose([ H | T ], M2) :- select_column(M2, H, Mrest), transpose(T, Mrest).
transpose([], [[]|T]) :- transpose([],T).
transpose([], []).

/*
symmetric(+M)
M es una matriz simétrica.
*/

symmetric(M) :-
        transpose(M,M).

/*
get_cell(+X,+Y,+M,-Val)
Devuelve en Val el contenido de la celda en la fila X y la columna Y de la matrix M..
*/

get_cell(1, 1, [[H3|_]|_], H3).
get_cell(1, Y, [[_|T2]|T], Val) :- succ(Z,Y), get_cell(1,Z, [T2|T], Val).
get_cell(X, Y, [_|T], Val) :- succ(W,X), get_cell(W, Y, T, Val).

/*
count_cells(+M,+Val,-Count)
Devuelve en Count la cantidad de celdas de M cuyo valor es Val
*/

count_cells([], _, 0).
count_cells([[]|T], Val, Count) :- count_cells(T, Val, Count).
count_cells([[Val|R]|T], Val, Count) :- count_cells([R|T], Val, C),succ(C,Count).
count_cells([[H1|R]|T], Val, Count) :- H1\==Val,count_cells([R|T], Val, Count).

/*
set_cell(+X,+Y,+M1,+Val,-M2)
Devuelve en M2 una matriz igual a M1, pero cambiando el contenido de la celda (X,Y) por el valor
Val.
*/

set_cell(_, _, [] , _, [] ) .
set_cell(0, 0, [[H|T1]|T2], _, [[H|T3]|T4] ) :- set_cell(0, 0, [T1|T2], _, [T3|T4]).
set_cell(0, 0, [[]|T1] , Val, [[]|T2] ) :- set_cell(0, 0, T1 , Val, T2 ).
set_cell(1, 1, [[_|T1]|T2], Val, [[Val|T3]|T4]) :- set_cell(0, 0, [T1|T2], _, [T3|T4]).
set_cell(1, Y, [[H|T1]|T2], Val, [[H|T3]|T4] ) :- Y > 1, succ(W,Y), set_cell(1, W, [T1|T2], Val, [T3|T4]).
set_cell(X, Y, [[]|T1] , Val, [[]|T2] ) :- X > 1, succ(Z,X), set_cell(Z, Y, T1 , Val, T2 ).
set_cell(X, Y, [[H|T1]|T2], Val, [[H|T3]|T4] ) :- X > 1, Y > 1, set_cell(X, Y, [T1|T2], Val, [T3|T4]).



