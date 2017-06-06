% bridges going one way == bridges going the other way
bridge_constraints, board(X, Y, _, N, _, _, _),
    board(X2, Y, _, _, _, S, _) ==> X > 1, X2 is X-1 |

    N eq S.

bridge_constraints, board(X, Y, _, _, E, _, _),
    board(X, Y2, _, _, _, _, W) ==> Y2 is Y+1|

    E eq W.
