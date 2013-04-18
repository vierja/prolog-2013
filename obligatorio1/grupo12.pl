/* 
    nth(?L,+Pos,?E)
    `E` es el elemento en la posicion `Pos` (contando a partir de 1) de la lista `L`.
    Ej. nth(2, [a,b,c], b)
    Ej corregido: nth([a,b,c], 2, b)
*/

/*nth([], 0, _).*/
nth([X|_], 1, X).
nth([_|Xs], Pos, X) :-
        Pos > 0,
        Next is Pos - 1,
        nth(Xs, Next, X).

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
    Max es el maximo de L, L1 es la lista L sin Max.
    Ej. max([2,4,3], 4, [2,3])
*/

max([X], X, []).
max([X|Xs], X, _) :- 
        max(Xs, Z, _),
        X >= Z. 
