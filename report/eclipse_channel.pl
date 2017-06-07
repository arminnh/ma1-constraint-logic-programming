channel(NumbersPositions, Board):-
	dim(Board, [N, N]),
	dim(NumbersPositions, [N, N]),
	( multifor([Number, X, Y], 1, N), param(NumbersPositions, Board) do
		#=(Board[X, Y], Number, B),
		#=(NumbersPositions[Number, X], Y, B)
    ).
