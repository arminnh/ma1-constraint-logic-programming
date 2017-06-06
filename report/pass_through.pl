( X > 1    -> N #= Board[X-1,   Y, 4] ; N = 0 ),
( Y < YMax -> E #= Board[  X, Y+1, 5] ; E = 0 ),
( X < XMax -> S #= Board[X+1,   Y, 2] ; S = 0 ),
( Y > 1    -> W #= Board[  X, Y-1, 3] ; W = 0 ),
