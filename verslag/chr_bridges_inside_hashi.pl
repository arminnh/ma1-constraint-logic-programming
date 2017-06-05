% bridges cannot go outside of the board
bridge_constraints, board(1, _, _, N, _, _, _)                ==> N = 0.
bridge_constraints, board(_, YMax, _, _, E, _, _), ymax(YMax) ==> E = 0.
bridge_constraints, board(XMax, _, _, _, _, S, _), xmax(XMax) ==> S = 0.
bridge_constraints, board(_, 1, _, _, _, _, W)                ==> W = 0.
