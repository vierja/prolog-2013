:- begin_tests(lists).
:- use_module(library(lists)).

% nth

test(nth_1) :-
        nth([a,b,c], 2, b), !.

test(nth_2, [fail]) :-
        nth([a,b,c], 2, a), !.

test(nth_3) :-
        findall(X, nth([a,b,c], 2, X), Xs),
        Xs = [b].

test(nth_4, [fail]) :-
        findall(X, nth([a,b,c], 2, X), Xs),
        Xs = [a].

test(nth_5) :-
        nth(X, 2, b),
        nth1(2, X, b).

% sublist_n

test(sublist_n_1) :-
        sublist_n([a,b,c], 2, [a,b]), !.

test(sublist_n_2, [fail]) :-
        sublist_n([a,b,c], 3, [a,b]), !.

test(sublist_n_3, [fail]) :-
        sublist_n([a,b,c], 2, [a,c]), !.

test(sublist_n_4) :-
        sublist_n([a,b,c], 0, []), !.

test(sublist_n_5) :-
        findall(X,  sublist_n([a,b,c], 2, X), Xs),
        writeln(Xs),
        Xs = [[b,c], [a,b]].


% palindromo

test(palindromo0) :-
        palindromo([]), !.

test(palindromo1) :-
        palindromo([1]), !.

test(palindromo2) :-
        palindromo([1,2,1]), !.

test(palindromo3) :-
        palindromo([1,2,4,5,2,1,2,5,4,2,1]), !.

test(palindromo3) :-
        palindromo([1,2,4,5,2,2,5,4,2,1]), !.

test(palindromo4, [fail]) :-
        palindromo([1,2]), !.

test(palindromo5, [fail]) :-
        palindromo([1,2,3]), !.


:- end_tests(lists).