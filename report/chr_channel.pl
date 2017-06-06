% Viewpoint 1 (thus board) is  board(X,Y, BlockIndex, Value),
% Viewpoint 2 (thus board_other_viewpoint) is board(Value, X,Y, BlockIndex),

% The search variable for board is Value1
channel, board(X,Y, BlockIndex, Value)
    , board_other_viewpoint(Value, X, Y2, B2) ==> number(Value), var(Y2), var(B2) |
        Y2 is Y,
        B2 is BlockIndex.

% The search variable for board_other_viewpoint is the Y index
channel,board_other_viewpoint(Value, X, Y, BlockIndex),
    board(X,Y, BlockIndex, V2) ==> var(V2), number(Y), number(BlockIndex) |
        V2 is Value.
