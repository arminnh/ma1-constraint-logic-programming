:- use_module(library(chr)).

:- chr_constraint main/1, hello_world/0, solve/1.

main([X | Tail]) <=> writeln('hello world'), writeln(X), writeln(Tail).

solve(ProblemName) <=> problem(ProblemName, Board), print_board(Board).

% solve(ProblemName) :-
	% problem(ProblemName, Board),
	% print_board(Board)
    % find_chr_constraint(),
	% sudoku(Board),
	% labeling(Board),
	% print_board(Board).
    %.

print_numbers([]).
print_numbers([ Number | Tail ]) :-
    var(Number),
    write(" _"),
    print_numbers(Tail).
print_numbers([ Number | Tail ]) :-
    write(" "),
    write(Number),
    print_numbers(Tail).

print_board([]).
print_board([ Row | Tail ]) :-
    print_numbers(Row),
    writeln(""),
    print_board(Tail).

/*
sudoku(Board) :-
	dim(Board, [N2,N2]),
	N is integer(sqrt(N2)),
	Board[1..N2,1..N2] :: 1..N2,

	( for(I,1,N2), param(Board,N2) do
	    Row is Board[I,1..N2],
	    alldifferent(Row),
	    Col is Board[1..N2,I],
	    alldifferent(Col)
	),

	( multifor([I,J],1,N2,N), param(Board,N) do
	    ( multifor([K,L],0,N-1), param(Board,I,J), foreach(X,SubSquare) do
		X is Board[I+K,J+L]
	    ),
	    alldifferent(SubSquare)
	).
*/

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
