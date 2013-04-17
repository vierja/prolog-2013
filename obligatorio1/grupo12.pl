/* 
    nth(?L,+Pos,?E)
    `E` es el elemento en la posicion `Pos` (contando a partir de 1) de la lista `L`.
    Ej. nth(2, [a,b,c], b)
    Ej corregido: nth([a,b,c], 2, b)
*/

nth(L, Pos, E) :- Z is Pos-1,
                  nth0(Z, L, E).

/*
    sublist_n(?L,+N,?Sub)
    `Sub` es una sublista de largo `N` de la lista `L`.
    Ej. sublist(2, [5,2,3,1,7], [3,1])
    Ej corregido: sublist_n([5,2,3,1,7], 2, [3,1])
*/

sublist_n(_, 0, []).
sublist_n([_|Xs], N, Y):- sublist_n(Xs, N, Y).
sublist_n([X|Xs], N, [X|Ys]):- Z is N-1,
                               sublist_n(Xs, Z, Ys).
