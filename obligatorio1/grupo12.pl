
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
%todo

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

merge([],[],[]).
merge([X|Xs],[],[X|Xs]).
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

member_sorted([],[]).
member_sorted([X],[X]).
member_sorted([X,Y|Zs], Sorted):-
        split_list([X,Y|Zs], First, Last),
        member_sorted(First, FirstSorted),
        member_sorted(Last, LastSorted),
        merge(FirstSorted, LastSorted, Sorted).

% split_list(+L, ?S1, ?S2)

split_list([],[],[]).
split_list([A],[],[A]).
split_list([X,Y|Zs], [X|Xs], [Y|Ys]) :-
        split_list(Zs, Xs, Ys).


/*
    permutation(+X,?Y)
    La lista `Y` es una permutacion de los elementos de la lista `X`.

    Se define como `permutacion` porque `permutation` ya existe.
*/

permutacion([], []).
permutacion(X, [Y|Ys]) :-
        remove_elem(X, Y, Zs),
        permutacion(Zs, Ys).

% remove_elem(+L, +Elem, ?LsinElem)

remove_elem([A|C], A, C).
remove_elem([A|C], B, [A|D]) :-
        remove_elem(C, B, D).




