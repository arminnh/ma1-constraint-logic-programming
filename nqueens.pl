%
% Two programs for the N queens problem
% one using a list model and one using an array model
%

:- lib(ic).



queens_lists(N, Board) :-

	length(Board, N),
	Board :: 1..N,

	( fromto(Board, [Q1|Cols], Cols, []) do
	    ( foreach(Q2, Cols), param(Q1), count(Dist,1,_) do
		Q2 #\= Q1,
		Q2 - Q1 #\= Dist,
		Q1 - Q2 #\= Dist
	    )
	),

	labeling(Board).



queens_arrays(N, Board) :-

	dim(Board, [N]),
	Board[1..N] :: 1..N,

	( for(I,1,N), param(Board,N) do
	    ( for(J,I+1,N), param(Board,I) do
	    	Board[I] #\= Board[J],
	    	Board[I] #\= Board[J]+J-I,
	    	Board[I] #\= Board[J]+I-J
	    )
	),

	Board =.. [_|Vars],
	labeling(Vars).
