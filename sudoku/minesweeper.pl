:- lib(ic).
:- import sumlist/2 from ic_global.
:- import nth1/3 from listut.

solve(X) :-
    % find board
    board(X, Board),

    writeln("Given board"),
    print_board(Board),

    % set rules and fill in results
    minesweeper(Board),
    % labeling(Board),

    writeln("Solution"),
    print_board(Board).

minesweeper(Board) :-
    dim(Board, [XMax, YMax]),
    % declare the domain of the values on the given board
    Board[1..XMax, 1..YMax] :: -1..8,

    % declare a board of mines
    dim(Mines, [XMax, YMax]),
    % declare the domain of the values on the board of mines
    Mines[1..XMax, 1..YMax] :: 0..1, % 0 -> No mine, 1 -> Yes mine on the position

    % for each position where there is a value on the given board => Mines = 0 on this position
    ( multifor([X, Y], 1, [XMax, YMax]), param(Mines, Board) do
        Val is Board[X, Y],
        (var(Val) ->
            true
        ;
            (Val = '*' ->
                Mines[X, Y] #= 1
            ;
                Mines[X, Y] #= 0
            )
        )
    ),

    % For each position where there is a value Val on the given board,
    % Val must equal the amount of neighbors on which there are mines
    ( multifor([X, Y], 1, [XMax, YMax]), param(Mines, Board) do
        Val #= Board[X, Y],
        (var(Val) ->
            true
        ;
            neighbors(Mines, X, Y, MineNeighbors),
            sumlist(MineNeighbors, Sum),
            Val #= Sum
        )
    ),

    % do a search for the mines
    labeling(Mines),

    % on every position where there is a mine, give the input board the value -1
    ( multifor([X, Y], 1, [XMax, YMax]), param(Mines, Board) do
        Val #= Mines[X, Y],
        (Val = 0 ->
            true
        ;
            Board[X, Y] #= -1
        )
    ),

    % fill in the remaining values on the board
    ( multifor([X, Y], 1, [XMax, YMax]), param(Mines, Board) do
        Val #= Board[X, Y],
        (var(Val) ->
            neighbors(Mines, X, Y, Neighbors),
            sumlist(Neighbors, Sum),
            Val #= Sum
        ;
            true
        )
    ),

    true.

neighbor(_, X, Y, NX, Y):-
    NX is X-1,
    NX > 0.

neighbor(Board, X, Y, NX, Y):-
    dim(Board, [XMax, _]),
    NX is X+1,
    NX =< XMax.

neighbor(_, X, Y, X, NY) :-
    NY is Y-1,
    NY > 0.

neighbor(Board, X, Y, X, NY) :-
    dim(Board, [_, YMax]),
    NY is Y+1,
    NY =< YMax.

neighbor(_, X, Y, NX, NY) :-
    NX is X-1,
    NY is Y-1,
    NX > 0,
    NY > 0.

neighbor(Board, X, Y, NX, NY) :-
    dim(Board, [_, YMax]),
    NX is X-1,
    NY is Y+1,
    NX > 0,
    NY =< YMax.

neighbor(Board, X, Y, NX, NY) :-
    dim(Board, [XMax, _]),
    NX is X+1,
    NY is Y-1,
    NX =< XMax,
    NY > 0.

neighbor(Board, X, Y, NX, NY) :-
    dim(Board, [XMax, YMax]),
    NX is X+1,
    NY is Y+1,
    NX =< XMax,
    NY =< YMax.

% Neighbors is a list of the values of the neighbors of Board[X, Y]
neighbors(Board, X, Y, Neighbors) :-
    findall([NX, NY], neighbor(Board, X, Y, NX, NY), NeighborsPositions),
    length(NeighborsPositions, N),

    ( for(I, 1, N), param(Board, Neighbors, NeighborsPositions) do
        nth1(I, NeighborsPositions, [X, Y]),
        Val is Board[X, Y],
        nth1(I, Neighbors, Val)
    ).


print_board(Board) :-
    dim(Board, [XMax, YMax]),

    ( for(X, 1, XMax), param(YMax, Board) do
        ( for(Y, 1, YMax), param(X, Board) do
            Val is Board[X, Y],
            ( var(Val) ->
                write(" ")
            ;
                (Val = -1 -> write('*') ; write(Val))
            ),
            write(" ")
        ),
        nl
    ),
    nl.

board(1,  [](
    [](1, _)
)).

board(2, [](
    [](1, _),
    [](_, _)
)).

board(3, [](
    [](_, _, 2, _, 3, _),
    [](2, _, _, _, _, _),
    [](_, _, 2, 4, _, 3),
    [](1, _, 3, 4, _, _),
    [](_, _, _, _, _, 3),
    [](_, 3, _, 3, _, _)
)).
