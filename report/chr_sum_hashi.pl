% amount of bridges equals island's amount
bridge_constraints, board(_, _, Amount, N, E, S, W) ==> Amount > 0 |
    Sum in 0..4,
    Sum2 in 0..4,
    add(N, E, Sum),
    add(S, W, Sum2),
    add(Sum, Sum2, Amount).
