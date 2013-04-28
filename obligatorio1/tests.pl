:- begin_tests(lists).
:- use_module(library(lists)).

% nth

test(nth_1) :-
        nth([a,b,c], 2, b), !.

test(nth_2, [fail]) :-
        nth([a,b,c], 2, a), !.

test(nth_3) :-
        findall(X, nth([a,b,c], 2, X), Xs),
        Xs = [b], !.

test(nth_4, [fail]) :-
        findall(X, nth([a,b,c], 2, X), Xs),
        Xs = [a], !.

test(nth_5) :-
        nth(X, 2, b),
        nth1(2, X, b), !.

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
        Xs = [[b,c], [a,b]], !.

% max_list

test(max_list0, [fail]) :-
        max_list([],[]), !.

test(max_list1) :-
        max_list([1,4,7], 7), !.

test(max_list2) :-
        max_list([7,4,1,4], 7), !.

test(max_list3) :-
        findall(X, max_list([9876,222,3435,23523], X), Xs),
        Xs = [23523].

% max (de la tarea)

test(max4) :-
        max([2,4,3], 4, [2,3]), !.

test(max5) :-
        max([2,4,3,4,7], 7, [2,4,3,4]), !.

test(max6) :-
        max([2,2,2,2,2,2,2], 2, [2,2,2,2,2,2]), !.

test(max7, [fail]) :-
        max([1,2,3], 2, [1,3]), !.

test(max7, [fail]) :-
        max([1,2,3], 2, [1,3]), !.

test(max8) :-
        findall(X, max([6,5,3,8,9,10,6,8], X, _), Xs),
        Xs = [10].

test(max9) :-
        findall(X, max([6,5,3,8,9,10,6,8], _, X), Xs),
        Xs = [[6,5,3,8,9,6,8]].

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

% merge

test(merge0) :-
        merge([],[],[]), !.

test(merge1) :-
        merge([A],[],[A]), !.

test(merge2) :-
        merge([],[A],[A]), !.

test(merge3) :-
        merge([1,2,3],[4],[1,2,3,4]), !.

test(merge4) :-
        merge([1,2,3,4,5,6],[1,2,3,4,5],[1,1,2,2,3,3,4,4,5,5,6]), !.

test(merge5) :-
        merge([2,4,6],[1,3,5,7,9],[1,2,3,4,5,6,7,9]), !.

test(merge6) :-
        merge([1,3,5,7,9],[2,4,6],[1,2,3,4,5,6,7,9]), !.

test(merge7) :-
        merge([1],[1],[1,1]), !.

test(merge8, [fail]) :-
        merge([1],[2],[2,1]), !.

test(merge9) :-
        findall(X,  merge([1,6,7], [6,8,3], X), Xs),
        Xs = [[1,6,6,7,8,3]], !.

% member_sorted

test(member_sorted0) :-
        member_sorted([1],1), !.

test(member_sorted1) :-
        member_sorted([1,2,3,4,5,6,6],3), !.

% permutation

test(permutation0) :-
        permutation([],[]), !.

test(permutation1) :-
        permutation([1,6,3,2,1,7,8],[7,1,3,1,6,8,2]), !.

test(permutation3, [fail]) :-
        permutation([1], [1,2]), !.

test(permutation4) :-
        findall(X, permutation([1,2], X), Xs),
        Xs = [[1,2], [2,1]].

test(permutation5) :-
        findall(X, permutation([1], X), Xs),
        Xs = [[1]].

test(permutation6) :-
        findall(X, permutation([1,2,3], X), Xs),
        Xs = [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]].

% remove_elem

test(remove_elem0) :-
        remove_elem([1,2],1,[2]), !.

test(remove_elem1) :-
        remove_elem([1,2],2,[1]), !.

test(remove_elem2) :-
        remove_elem([1,2,2],2,[1,2]), !.

test(remove_elem3) :-
        findall(X, remove_elem([1,2,3,4,5,6], 5, X), Xs),
        Xs = [[1,2,3,4,6]].

test(remove_elem4) :-
        findall(X, remove_elem([1,2,3,4,5,6], X, [1,3,4,5,6]), Xs),
        Xs = [2].

% min

test(min0, [fail]) :-
        min([],[]), !.

test(min1) :-
        min([1,4,7], 1), !.

test(min2) :-
        min([7,4,1,4], 1), !.

test(min3) :-
        findall(X, min([9876,222,3435,23523], X), Xs),
        Xs = [222].

% selection_sort

test(selection_sort0) :-
        selection_sort([1,2,3,4], [1,2,3,4]), !.

test(selection_sort1) :-
        selection_sort([8,7,6,5,4,3], [3,4,5,6,7,8]), !.

test(selection_sort2) :-
        selection_sort([],[]), !.

test(selection_sort3) :-
        findall(X, selection_sort([1,6,4,2,5,3], X), Xs),
        Xs = [[1,2,3,4,5,6]].

test(selection_sort4) :-
        selection_sort([1,1,1,1,1], [1,1,1,1,1]), !.

test(selection_sort5) :-
        findall(X, selection_sort([1], X), Xs),
        Xs = [[1]].

% matrix

test(matrix0) :-
        matrix(1,1,1,[[1]]), !.

test(matrix1) :-
        matrix(5,2,2,[[2,2],[2,2],[2,2],[2,2],[2,2]]), !.

test(matrix2) :-
        findall(X, matrix(5,2,2,X), Xs),
        Xs = [[[2,2],[2,2],[2,2],[2,2],[2,2]]].

test(matrix3) :-
        matrix(2,1,0,[[0],[0]]), !.

test(matrix4) :-
        findall(X, matrix(5,1,6,X), Xs),
        Xs = [[[6],[6],[6],[6],[6]]].

% member_length

test(member_length0) :-
        member_length(0,0,[]), !.

test(member_length1) :-
        member_length(10,0,[0,0,0,0,0,0,0,0,0,0]), !.

test(member_length2) :-
        findall(X, member_length(5,2,X), Xs),
        Xs = [[2,2,2,2,2]].

% select_column

test(select_column0) :-
        select_column([],[],[]).

test(select_column1) :-
        select_column([[1],[1],[1]],[1,1,1],[[],[],[]]).

test(select_column2) :-
        select_column([[1,1],[2,1],[3,1]],[1,2,3],[[1],[1],[1]]).

test(select_column3) :-
        select_column([[1,1,1],[2,2,2]],[1,2],[[1,1],[2,2]]).

test(select_column4) :-
        select_column([[1,1,1],[2,2,2]],[1,2],[[1,1],[2,2]]).

test(select_column5) :-
        findall(X, select_column([[1,1],[2,2],[3,3]],X,[[1],[2],[3]]), Xs),
        Xs = [[1,2,3]].

test(select_column6) :-
        findall(X, select_column([[1,1],[2,2],[3,3]],[1,2,3],X), Xs),
        Xs = [[[1],[2],[3]]].

test(select_column7) :-
        findall(X, select_column(X,[1,2,3],[[1],[2],[3]]), Xs),
        Xs = [[[1,1],[2,2],[3,3]]].

% symmetric

test(symmetric0) :-
        symmetric([[0,0],[0,0]]), !.

test(symmetric1) :-
        symmetric([[1,0,0],[0,1,0],[0,0,1]]), !.

test(symmetric2) :-
        symmetric([[1,0,0,0],[0,1,0,0],[0,0,1,0],[0,0,0,1]]), !.

% get_cell

test(get_cell0) :-
        get_cell(1,1,[[X]],X), !.

test(get_cell1) :-
        get_cell(3,3,[[1,1,1,1],[1,1,1,1],[1,1,X,1],[1,1,1,1]],X), !.

test(get_cell2) :-
        findall(X, get_cell(2,2,[[1,1,1,1],[1,3,1,1],[1,1,1,1],[1,1,1,1]],X), Xs),
        Xs = [3].

% count_cells

test(count_cells0) :-
        count_cells([], _, 0), !.

test(count_cells1) :-
        count_cells([[1,1,1,1],[1,1,1,1],[1,1,1,1],[1,1,1,1],[1,1,1,1]], 2, 0), !.

test(count_cells2) :-
        count_cells([[1,1,3,1],[1,1,1,1],[1,3,1,1],[1,1,1,1],[1,1,1,3]], 3, 3), !.

test(count_cells3) :-
        findall(X, count_cells([[1,1,3,1],[1,1,1,3]], 3, X), Xs),
        Xs = [2].

% set_cell

test(set_cell0) :-
        set_cell(1,1,[[1,1,1,1],[1,1,1,1]],9,[[9,1,1,1],[1,1,1,1]]), !.

test(set_cell1) :-
        set_cell(2,4,[[1,1,1,1],[1,1,1,1]],9,[[1,1,1,1],[1,1,1,9]]), !.

test(set_cell2) :-
        findall(X, set_cell(4,2,[[1,1],[1,1],[1,1],[1,1],[1,1]],9,X), Xs),
        Xs = [[[1,1],[1,1],[1,1],[1,9],[1,1]]].

:- end_tests(lists).