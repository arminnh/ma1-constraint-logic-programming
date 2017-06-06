channel(NumbersPositions, Board):-
    dim(Board, [N, N]),
    dim(NumbersPositions, [N, N, 2]),
    (multifor([Number, Position, Y], 1, N), param(NumbersPositions, Board, N) do
        #=(Board[Position, Y], Number, B),
        #=(NumbersPositions[Number, Position, 2], Y, B)
    ).
