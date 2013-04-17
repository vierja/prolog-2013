:- begin_tests(lists).
:- use_module(library(lists)).

%% nth - Determinista

test(nth_1) :-
        nth([a,b,c], 2, b), !.

test(nth_2, [fail]) :-
        nth([a,b,c], 2, a), !.

%% nth - No Determinista

test(nth_3) :-
        findall(X, nth([a,b,c], 2, X), Xs),
        Xs = [b].

test(nth_4, [fail]) :-
        findall(X, nth([a,b,c], 2, X), Xs),
        Xs = [a].

test(nth_5) :-
        nth(X, 2, b),
        nth0(1, X, b).

%% sublist_n - Determinista

test(sublist_n_1) :-
        sublist_n([a,b,c], 2, [a,b]), !. %pongo esto porque esta bien tenga choicepoint.

:- end_tests(lists).