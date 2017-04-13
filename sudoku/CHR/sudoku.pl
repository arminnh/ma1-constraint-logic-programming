:- use_module(library(chr)).

:- chr_constraint main/0.
:- chr_constraint hello_world/0.


main <=> hello_world.


/*
solve(ProblemName) :-
	problem(ProblemName, Board),
	print_board(Board),
	sudoku(Board),
	labeling(Board),
	print_board(Board).

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