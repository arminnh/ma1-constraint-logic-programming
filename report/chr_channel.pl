% the search variable for board is Value
channel, board(X, Y, BlockIndex, Value), board_viewpoint_2(Value, X, Y2, B2)
    ==> number(Value), var(Y2), var(B2) |
        Y2 = Y,
        B2 = BlockIndex.

% the search variable for board_other_viewpoint is Y
channel, board_viewpoint_2(Value, X, Y, BlockIndex), board(X, Y, BlockIndex, V2)
    ==> var(V2), number(Y), number(BlockIndex) |
        V2 = Value.
