:- use_module(library(chr)).
% TODO: ask if transpose is allowed
:- use_module(library(clpfd), [ transpose/2 ]).

:- chr_constraint solve/0, solve/1, sudoku/1, print_board/1, print_numbers/1,
                  diff/2, list_diff/1, list_diff/2, rows_different/1, enum/1,
                  enum_board/1, upto/2, domain/2, makedomains/1.

:- op(700, xfx, in).
:- op(700, xfx, le).
:- op(700, xfx, eq).
:- op(600, xfx, '..').
:- chr_constraint le/2, eq/2, in/2, add/3.

solve <=> solve(1).

solve(ProblemName) <=>
    problem(ProblemName, Board),
    print_board(Board),
    sudoku(Board),
    % labeling(Board),
    writeln("Result:"),
    print_board(Board).


sudoku(Board) <=>
    rows_different(Board),
    % transpose(Board, Board2),
    % rows_different(Board2),
    enum_board(Board),
    true.


diff(X,Y) <=> nonvar(X), nonvar(Y) | X \== Y.


list_diff([]) <=> true.
list_diff([ Val | Vals ]) <=>
    list_diff(Val, Vals),
    list_diff(Vals).


list_diff(_, []) <=> true.
list_diff(Val, [ Val2 | Vals ]) <=>
    diff(Val, Val2),
    list_diff(Val, Vals).


rows_different([]) <=> true.
rows_different([ Row | Rows ]) <=>
    makedomains(Row),
    list_diff(Row),
    rows_different(Rows).


% enum(N, R): fill list with values from 1..N
enum([])                <=> true.
enum([ X | R ])         <=> number(X) | enum(R).
enum([ X | R ]), X in L <=> member(X, L), enum(R).


% enum_board(Board): fill board with values from 1..N
enum_board([]) <=> true.
enum_board([ Row | Rows ]) <=>
    enum(Row),
    enum_board(Rows).


% upto(N,L): L=[1..N]
upto([], 0).
upto([ N | L ], N) :-
    N > 0,
    N1 is N-1,
    upto(L, N1).


% domain(L,D): create 'X in D' constraints for all X from L
domain([], _) <=> true.
domain([ Val | Tail ], D) <=>
     Val in D,
     domain(Tail, D).


% makedomains(R): R is an N-elem. list, create 'X in [1..N]' constraints
makedomains(List) <=>
    length(List, N),
    upto(DomainList, N),
    domain(List, DomainList).


print_numbers([]) <=> writeln("").
print_numbers([ Number | Tail ]) <=>
    nonvar(Number) |
    write(" "),
    write(Number),
    print_numbers(Tail).
print_numbers([ _ | Tail ]) <=>
    write(" _"),
    print_numbers(Tail).


print_board([]) <=> writeln("").
print_board([ Row | Tail ]) <=>
    print_numbers(Row),
    print_board(Tail).

% alternative version to store numbers: problem(ProblemName, X, Y, Number)
problem(1, [ [_, _, 2, _, _, 5, _, 7, 9],
             [1, _, 5, _, _, 3, _, _, _],
             [_, _, _, _, _, _, 6, _, _],
             [_, 1, _, 4, _, _, 9, _, _],
             [_, 9, _, _, _, _, _, 8, _],
             [_, _, 4, _, _, 9, _, 1, _],
             [_, _, 9, _, _, _, _, _, _],
             [_, _, _, 1, _, _, 3, _, 6],
             [6, 8, _, 3, _, _, 4, _, _] ]).

problem(2, [ [_, _, 3, _, _, 8, _, _, 6],
             [_, _, _, 4, 6, _, _, _, _],
             [_, _, _, 1, _, _, 5, 9, _],
             [_, 9, 8, _, _, _, 6, 4, _],
             [_, _, _, _, 7, _, _, _, _],
             [_, 1, 7, _, _, _, 9, 5, _],
             [_, 2, 4, _, _, 1, _, _, _],
             [_, _, _, _, 4, 6, _, _, _],
             [6, _, _, 5, _, _, 8, _, _] ]).

problem(12, [ [_] ]).

problem(13, [ [2, 1],
	          [_, _] ]).

problem(14, [ [1, _, 3],
      	      [3, _, _],
	          [2, _, _] ]).

problem(15, [ [1, _, 3, _],
	          [3, _, _, 4],
	          [2, _, _, _],
	          [4, _, 2, _] ]).

problem(16, [ [_, _, _, _],
		      [_, _, _, _],
	      	  [_, _, _, _],
		      [_, _, _, _] ]).

problem(17, [ [1, _, 3, _, _, 5, 4, 8],
	          [3, 5, 2, 4, _, 6, 7, _],
	          [2, _, 6, 3, _, 4, _, _],
	          [4, 2, 5, _, 7, _, 1, _],
	          [5, 3, 7, _, 6, 2, _, 4],
	          [6, _, _, 5, _, 7, _, _],
              [7, 4, _, 6, _, _, 3, _],
              [8, _, _, _, _, _, 6, _] ]).
