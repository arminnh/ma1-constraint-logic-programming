:- lib(ic).
% :- import alldifferent/1, sorted/2 from ic_global.
% :- coroutine.
% :- lib(lists).
% :- import nth1/3 from listut.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hashiwokakero, also called Bridges is a logic puzzle in which different islands
% have to be connected by bridges. A bridges puzzle consists of a square grid
% in which some numbers are placed. Squares on which a number is placed are
% referred to as islands. The goal of the puzzle is to draw bridges between
% islands subject to the following restrictions.
%     Bridges can only run horizontally or vertically.
%     Bridges run in one straight line.
%     Bridges cannot cross other bridges or islands.
%     At most two bridges connect a pair of islands.
%     The number of bridges connected to each island must match the number on that island.
%     The bridges must connect the islands into a single connected group.
%
% Solution started from http://stackoverflow.com/questions/20337029/hashi-puzzle-representation-to-solve-all-solutions-with-prolog-restrictions/20364306#20364306
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve(Number) :-
    ( puzzle(Number, _, _) ->
        % Each puzzle(Id, S, Islands) fact defines the input of one problem:
        % its identier Id, the size S (width and height), and the list of islands Islands.
        puzzle(Number, Size, Islands),

        % create a board with the islands on it
        islands_board(Islands, Size, Board)
    ;
        % create a board from a matrix that contains the islands
        board(Number, Matrix),
        matrix_board(Matrix, Board)
    ),

    writeln("Given board:"),
	print_board(Board),

    % create bridges and set constraints
    hashiwokakero(Board),

    % do search on variables
	% search(naive, Board),
    labeling(Board),

    % print results
    writeln("Search done:"),
    print_board(Board).

% The board can be viewed as a matrix in which each position contains an array
% of 5 variables: The amount of bridges that need to be connected to the position,
% and the amounts of briges going North, East, South, or West from the position
hashiwokakero(Board) :-
    dim(Board, [XMax, YMax, 6]), % 5 variables: Amount, N, E, S, W for each position
    Board[1..XMax, 1..YMax, 6] #:: 0..1,

    ( multifor([X, Y], 1, [XMax, YMax]), param(Board, XMax, YMax) do
        Amount is Board[X, Y, 1],
        N is Board[X, Y, 2],
        E is Board[X, Y, 3],
        S is Board[X, Y, 4],
        W is Board[X, Y, 5],

        % if this position is not on the edges of the board, then the amount of bridges
        % going in one direction needs to equals the amount in the other direction
        ( X > 1    -> N #= Board[X-1,   Y, 4] ; N = 0 ),
        ( Y < YMax -> E #= Board[  X, Y+1, 5] ; E = 0 ),
        ( X < XMax -> S #= Board[X+1,   Y, 2] ; S = 0 ),
        ( Y > 1    -> W #= Board[  X, Y-1, 3] ; W = 0 ),

        % if this position requires an amount of bridges,
        % make the sum of all bridges equal this amount
        ( Amount > 0 ->
            [N, E, S, W] #:: 0..2,
            N + E + S + W #= Amount
        ; % else make sure bridges don't cross each other
            N = S, E = W,
            (N #= 0) or (E #= 0)
        )
    ),

    board_islands(Board, Islands),
    length(Islands, L1),
    writeln(["Islands: ", L1, Islands]),
    writeln(["Sets: ", Sets]),
    board_connected_sets(Board, Islands, Sets),
    length(Sets, L2),
    writeln(["Connected sets: ", L2, Sets]),

    true.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELPER PROCEDURES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% create a usable board from an array of Islands
% each island takes the form (X, Y, N) where X is the row number, Y is the column
% number and N the number of bridges that should arrive in this island.
islands_board(Islands, Size, Board) :-
    dim(Board, [Size, Size, 6]),

    % fill in the island bridge amounts first
    ( foreacharg(Island, Islands), param(Board) do
        X is Island[1],
        Y is Island[2],
        Amount is Island[3],
        Board[X, Y, 1] #= Amount
    ),

    % then fill in zeros
    ( foreacharg(Row, Board) do
        ( foreacharg(Position, Row) do
            Amount is Position[1],
            ( var(Amount) -> Position[1] #= 0 ; true )
        )
    ).

% create a usable board from a matrix that contains the islands
matrix_board(Matrix, Board) :-
    dim(Matrix, [XMax, YMax]),
    dim(Board, [XMax, YMax, 6]),
    Board[1..XMax, 1..YMax, 6] #:: 1..1,

    % fill in the island bridge amounts first
    ( multifor([X, Y], 1, [XMax, YMax]), param(Matrix, Board) do
        Board[X, Y, 1] #= Matrix[X, Y]
    ).

board_islands(Board, Islands) :-
    dim(Board, [XMax, YMax, 6]),

    ( multifor([X, Y], 1, [XMax, YMax]), param(Board, Islands) do
        Amount is Board[X, Y, 1],
        ( Amount > 0 ->
            member([X, Y], Islands)
        ;
            true
        )
    ).

% board_connected_sets(Board, Islands, Sets): Board is an N*N*5 array representing the hashi board,
% Islands is a list of positions on which there are islands on the board
% Sets is a list of lists of islands that belong to the same connected set
board_connected_sets(_, [], _).
board_connected_sets(Board, [ [X, Y] | Islands ], [ Set | Sets ]) :-
    Position is Board[X, Y],
    writeln(["board_connected_sets  ", [X, Y], " --- ", Position]),

    % let island be member of current set
    member([X, Y], Set),
    writeln(["  member of set:", Set]),

    % set position to visited
    Board[X, Y, 6] #= 1,

    % travel to the neighbors of the current position and fill the current set
    N #= Board[X, Y, 2],
    E #= Board[X, Y, 3],
    S #= Board[X, Y, 4],
    W #= Board[X, Y, 5],

    ( N #> 0 -> write("[    visiting N"), next_pos([X, Y], 2, Pos2), write(", got pos: "), writeln([Pos2]), fill_set(Board, Pos2, 2, Set), writeln(["    N visited: ", Set]) ; true ),
    ( E #> 0 -> write("[    visiting E"), next_pos([X, Y], 3, Pos3), write(", got pos: "), writeln([Pos3]), fill_set(Board, Pos3, 3, Set), writeln(["    E visited: ", Set]) ; true ),
    ( S #> 0 -> write("[    visiting S"), next_pos([X, Y], 4, Pos4), write(", got pos: "), writeln([Pos4]), fill_set(Board, Pos4, 4, Set), writeln(["    S visited: ", Set]) ; true ),
    ( W #> 0 -> write("[    visiting W"), next_pos([X, Y], 5, Pos5), write(", got pos: "), writeln([Pos5]), fill_set(Board, Pos5, 5, Set), writeln(["    W visited: ", Set]) ; true ),

    length(Set, _),
    % find the remaining islands and create next sets
    writeln(["  subtracting from islands: ", Islands, Set]),
    subtract(Islands, Set, RemainingIslands),
    writeln(["\n  remaining islands: ", RemainingIslands]),
    board_connected_sets(Board, RemainingIslands, Sets),
    true.

fill_set(Board, [X, Y], Direction, Set) :-
    Position is Board[X, Y],
    writeln(["         fill_set --- ", [X, Y], " --- ", Position, " --- ", Set]),
    Visited is Board[X, Y, 6],
    ( nonvar(Visited) ->
        writeln(["         already visited: "])
    ;
        Board[X, Y, 6] #= 1,
        writeln(["         visited: ", [X, Y]]),
        Amount is Board[X, Y, 1],
        ( Amount > 0 ->
            member([X, Y], Set),

            N #= Board[X, Y, 2],
            E #= Board[X, Y, 3],
            S #= Board[X, Y, 4],
            W #= Board[X, Y, 5],

            ( N #> 0 -> write("[             visiting N"), next_pos([X, Y], 2, Pos2), write(", got pos: "), writeln([Pos2]), fill_set(Board, Pos2, 2, Set), writeln(["             N visited: ", Set]) ; true ),
            ( E #> 0 -> write("[             visiting E"), next_pos([X, Y], 3, Pos3), write(", got pos: "), writeln([Pos3]), fill_set(Board, Pos3, 3, Set), writeln(["             E visited: ", Set]) ; true ),
            ( S #> 0 -> write("[             visiting S"), next_pos([X, Y], 4, Pos4), write(", got pos: "), writeln([Pos4]), fill_set(Board, Pos4, 4, Set), writeln(["             S visited: ", Set]) ; true ),
            ( W #> 0 -> write("[             visiting W"), next_pos([X, Y], 5, Pos5), write(", got pos: "), writeln([Pos5]), fill_set(Board, Pos5, 5, Set), writeln(["             W visited: ", Set]) ; true )
        ;
            next_pos([X, Y], Direction, Pos),
            fill_set(Board, Pos, Direction, Set)
        )
    ).

next_pos([X, Y], 2, [X2, Y]) :- X2 is X-1. % north
next_pos([X, Y], 4, [X2, Y]) :- X2 is X+1. % south
next_pos([X, Y], 3, [X, Y2]) :- Y2 is Y+1. % east
next_pos([X, Y], 5, [X, Y2]) :- Y2 is Y-1. % west

print_board(Board) :-
    ( foreacharg(Row, Board) do
        ( foreacharg(Position, Row) do
            Amount is Position[1],
            ( Amount > 0 ->
                write(Amount)
            ;
                NS is Position[2],
                EW is Position[3],
                ( nonvar(NS), nonvar(EW) ->
                    symbol(NS, EW, Char),
                    write(Char)
                ;
                    true
                )
            ),
            write(' ')
        ),
        nl
    ),
    nl, nl.

symbol(0, 0, ' ').
symbol(0, 1, '-').
symbol(0, 2, '=').
symbol(1, 0, '|').
symbol(2, 0, '"').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SOME SEARCH STRATEGIES TAKEN FROM SLIDES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

search(naive, List) :-
    search(List, 0, input_order, indomain, complete,  []).

search(middle_out, List) :-
    middle_out(List, MOList),
    search(MOList, 0, input_order, indomain, complete, []).

search(first_fail, List) :-
    search(List, 0, first_fail, indomain, complete, []).

search(moff, List) :-
    middle_out(List, MOList),
    search(MOList, 0, first_fail, indomain, complete, []).

search(moffmo, List) :-
    middle_out(List, MOList),
    search(MOList, 0, first_fail,  indomain_middle, complete, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SAMPLE PROBLEMS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% puzzle 1, easy
% http://en.wikipedia.org/wiki/File:Val42-Bridge1n.png
% solution: http://en.wikipedia.org/wiki/File:Val42-Bridge1.png
puzzle(1, 7, [](
    [](1,1,2),  [](1,2,3), [](1,4,4), [](1,6,2),
    [](2,7,2),
    [](3,1,1), [](3,2,1), [](3,5,1), [](3,6,3), [](3,7,3),
    [](4,1,2), [](4,4,8), [](4,6,5), [](4,7,2),
    [](5,1,3), [](5,3,3), [](5,7,1),
    [](6,3,2), [](6,6,3), [](6,7,4),
    [](7,1,3), [](7,4,3), [](7,5,1), [](7,7,2)
)).

% puzzle 2, moderate
% http://en.wikipedia.org/wiki/File:Bridges-example.png
% solution: http://upload.wikimedia.org/wikipedia/en/1/10/Bridges-answer.PNG

puzzle(2, 13, [](
    [](1,1,2),  [](1,3,4),  [](1,5,3),   [](1,7,1),   [](1,9,2),   [](1,12,1),
    [](2,10,3), [](2,13,1),
    [](3,5,2),  [](3,7,3),  [](3,9,2),
    [](4,1,2),  [](4,3,3),  [](4,6,2),   [](4,10,3),  [](4,12,1),
    [](5,5,2),  [](5,7,5),  [](5,9,3),   [](5,11,4),
    [](6,1,1),  [](6,3,5),  [](6,6,2),   [](6,8,1),   [](6,12,2),
    [](7,7,2),  [](7,9,2),  [](7,11,4),  [](7,13,2),
    [](8,3,4),  [](8,5,4),  [](8,8,3),   [](8,12,3),
    [](10,1,2), [](10,3,2), [](10,5,3),  [](10,9,3),  [](10,11,2), [](10,13,3),
    [](11,6,2), [](11,8,4), [](11,10,4), [](11,12,3),
    [](12,3,1), [](12,5,2),
    [](13,1,3), [](13,6,3), [](13,8,1),  [](13,10,2), [](13,13,2)
)).

% puzzle 3
% http://www.conceptispuzzles.com/index.aspx?uri=puzzle/hashi/techniques
puzzle(3, 6, [](
    [](1,1,1), [](1,3,4), [](1,5,2),
    [](2,4,2), [](2,6,3),
    [](3,1,4), [](3,3,7), [](3,5,1),
    [](4,4,2), [](4,6,5),
    [](5,3,3), [](5,5,1),
    [](6,1,3), [](6,4,3), [](6,6,3)
)).

% puzzle 4
% http://www.conceptispuzzles.com/index.aspx?uri=puzzle/euid/010000008973f050f28ceb4b11c74e73d34e1c47d885e0d8449ab61297e5da2ec85ea0804f0c5a024fbf51b5a0bd8f573565bc1b/play
puzzle(4, 8, [](
    [](1,1,2), [](1,3,2), [](1,5,5), [](1,7,2),
    [](2,6,1), [](2,8,3),
    [](3,1,6), [](3,3,3),
    [](4,2,2), [](4,5,6), [](4,7,1),
    [](5,1,3), [](5,3,1), [](5,6,2), [](5,8,6),
    [](6,2,2),
    [](7,1,1), [](7,3,3), [](7,5,5), [](7,8,3),
    [](8,2,2), [](8,4,3), [](8,7,2)
)).

% http://stackoverflow.com/questions/20337029/hashi-puzzle-representation-to-solve-all-solutions-with-prolog-restrictions/20364306#20364306
board(5, [](
    [](3, 0, 6, 0, 0, 0, 6, 0, 3),
    [](0, 0, 0, 0, 0, 0, 0, 0, 0),
    [](0, 1, 0, 0, 0, 0, 0, 0, 0),
    [](0, 0, 0, 0, 0, 0, 0, 0, 0),
    [](2, 0, 0, 0, 0, 1, 0, 0, 0),
    [](0, 0, 0, 0, 0, 0, 0, 0, 0),
    [](0, 0, 0, 0, 0, 0, 0, 0, 0),
    [](1, 0, 3, 0, 0, 2, 0, 0, 0),
    [](0, 3, 0, 0, 0, 0, 4, 0, 1)
)).

% same as puzzle 2
% https://en.wikipedia.org/wiki/Hashiwokakero#/media/File:Bridges-example.png
board(6, [](
    [](2, 0, 4, 0, 3, 0, 1, 0, 2, 0, 0, 1, 0),
    [](0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 1),
    [](0, 0, 0, 0, 2, 0, 3, 0, 2, 0, 0, 0, 0),
    [](2, 0, 3, 0, 0, 2, 0, 0, 0, 3, 0, 1, 0),
    [](0, 0, 0, 0, 2, 0, 5, 0, 3, 0, 4, 0, 0),
    [](1, 0, 5, 0, 0, 2, 0, 1, 0, 0, 0, 2, 0),
    [](0, 0, 0, 0, 0, 0, 2, 0, 2, 0, 4, 0, 2),
    [](0, 0, 4, 0, 4, 0, 0, 3, 0, 0, 0, 3, 0),
    [](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    [](2, 0, 2, 0, 3, 0, 0, 0, 3, 0, 2, 0, 3),
    [](0, 0, 0, 0, 0, 2, 0, 4, 0, 4, 0, 3, 0),
    [](0, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0),
    [](3, 0, 0, 0, 0, 3, 0, 1, 0, 2, 0, 0, 2)
)).

board(7, [](
    [](1, 0, 1, 0, 2),
    [](0, 0, 0, 0, 0),
    [](0, 0, 0, 0, 2)
)).
