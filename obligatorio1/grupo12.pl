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

sublist_n(_, 0, []).
sublist_n([_|Xs], N, Y) :-
        sublist_n(Xs, N, Y).
sublist_n([X|Xs], N, [X|Ys]) :-
        Z is N-1,
        prefix_n(Xs, Z, Ys).

prefix_n(_, 0, []).
prefix_n([X|Xs], N, [X|Ys]) :-
        Z is N-1,
        prefix_n(Xs, Z, Ys).

/*
    max(+L,?Max,?L1)
    `Max` es el maximo de `L`, `L1` es la lista `L` sin Max.
    Ej. max([2,4,3], 4, [2,3])
*/

max([X], X, []).
max(X, Max, L1) :-
        max_list(X, Max),
        remove_elem(X, Max, L1).

% max_list(+L, Max)

max_list([X|Xs], Max) :-
        max_list(Xs, X, Max).
max_list([X|Xs], Cand, Rest):-
        X =< Cand, max_list(Xs, Cand, Rest);
        X >  Cand, max_list(Xs, X, Rest).
max_list([], Cand, Cand).

/*
    palindromo(+L)
    La lista `L` es un palindromo.
    Ej. palindromo([a,b,c,b,a])
*/

palindromo(X) :-
        reverse(X, X).

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

    Se realiza merge_sort que funciona de forma recursiva y no recorre la lista
    de forma innecesaria.
    http://en.wikipedia.org/wiki/Merge_sort
*/

member_sorted([], []).
member_sorted([X], [X]).
member_sorted([X,Y|Zs], Sorted):-
        split_list([X,Y|Zs], First, Last),
        member_sorted(First, FirstSorted),
        member_sorted(Last, LastSorted),
        merge(FirstSorted, LastSorted, Sorted).

% split_list(+L, ?S1, ?S2)

split_list([], [], []).
split_list([A], [], [A]).
split_list([X,Y|Zs], [X|Xs], [Y|Ys]) :-
        split_list(Zs, Xs, Ys).

/*
    permutation(+X,?Y)
    La lista `Y` es una permutacion de los elementos de la lista `X`.

    Se define como `permutacion` porque `permutation` ya existe.
*/

% remove_elem(+L, +Elem, ?LsinElem)

remove_elem([A|C], A, C).
remove_elem([A|C], B, [A|D]) :-
        remove_elem(C, B, D).

permutacion([], []).
permutacion(X, [Y|Ys]) :-
        remove_elem(X, Y, Zs),
        permutacion(Zs, Ys).

/*
    selection_sort(+L,?S)
    `S` es la lista ordenada de `L` utilizando el algoritmo de selection sort.
*/

% min(+L, ?Min)

min([X|Xs], Min) :-
        min(Xs, X, Min).
min([X|Xs], Cand, Rest):-
        X >= Cand, min(Xs, Cand, Rest);
        X <  Cand, min(Xs, X, Rest).
min([], Cand, Cand).

selection_sort([], []).
selection_sort(X, [Y|Ys]) :-
    min(X, Y),
    remove_elem(X, Y, Zs),
    selection_sort(Zs, Ys).

/*
    matrix(+X,+Y,+Val,?M)
    M es una matriz de X filas e Y columnas. Cada celda debe tener el valor Val.
    La matriz se representa mediante una lista de X filas, donde cada fila es una lista de Y celdas. 
*/

matrix(0, _, _, []).
matrix(_, 0, _, []).

matrix(X, Y, Val, [H|T])  :- 
        succ(W, X),
        member_length(Y, Val, H),
        matrix(W, Y, Val, T).

member_length(0, _, []).

member_length(X, Aux, [Aux|T]) :- 
        succ(Z, X),
        member_length(Z, Aux, T).

/*
    select_column(?M,?C,?MRest)
    C es la primera columna de la matriz M, MRest es la matriz M sin su primera columna.
*/

select_column([],[],[]).
select_column([[A|R]|T], [H|Tc], [Hr|Tr]) :- 
        A==H,
        R==Hr,
        select_column(T, Tc, Tr).

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

set_cell(_, _, []         ,   _, []           ) .
set_cell(0, 0, [[H|T1]|T2],   _, [[H|T3]|T4]  ) :-                   set_cell(0, 0, [T1|T2],   _, [T3|T4]).
set_cell(0, 0, [[]|T1]    , Val, [[]|T2]      ) :-                   set_cell(0, 0, T1     , Val, T2     ).
set_cell(1, 1, [[_|T1]|T2], Val, [[Val|T3]|T4]) :-                   set_cell(0, 0, [T1|T2],   _, [T3|T4]).
set_cell(1, Y, [[H|T1]|T2], Val, [[H|T3]|T4]  ) :- Y > 1, succ(W,Y), set_cell(1, W, [T1|T2], Val, [T3|T4]).
set_cell(X, Y, [[]|T1]    , Val, [[]|T2]      ) :- X > 1, succ(Z,X), set_cell(Z, Y, T1     , Val, T2     ).
set_cell(X, Y, [[H|T1]|T2], Val, [[H|T3]|T4]  ) :- X > 1,     Y > 1, set_cell(X, Y, [T1|T2], Val, [T3|T4]).
