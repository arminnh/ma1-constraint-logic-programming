( X > 1    -> N #= Board[X-1,   Y, 4] ; N = 0 ),
 ( Y < YMax -> E #= Board[  X, Y+1, 5] ; E = 0 ),
 ( X < XMax -> S #= Board[X+1,   Y, 2] ; S = 0 ),
 ( Y > 1    -> W #= Board[  X, Y-1, 3] ; W = 0 ),
 
 % if this position requires an amount of bridges,
 % make the sum of all bridges equal this amount
 ( Amount > 0 ->
 % (Amount == 8 ->
 % Not needed since if the amount is 8
 % Eclipse knows that everything needs to be 2
 [N, E, S, W] #:: 0..2,
 N + E + S + W #= Amount
 ; % else make sure bridges don't cross each other
 N = S, E = W,
 (N #= 0) or (E #= 0)
 ),
