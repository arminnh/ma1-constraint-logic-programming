:- lib(ic).
% :- import alldifferent/1, sorted/2 from ic_global.
% :- coroutine.
:- lib(lists).
:- import nth1/3 from listut.

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

% solve a given game board
solve(Number) :-
    % find the game board
    puzzle_board(Number, Board),

    writeln("Given board:"),
	print_board(Board),

    % create bridges and set constraints
    hashiwokakero(Board),
    writeln("kk"),

    % do search on variables
    search(naive, Board),
    % Check that everything is connected
    %board_islands(Board, AllIslands),
    %length(AllIslands, N),
    writeln("connected"),
    board_connected_set(Board),

    % print results
    writeln("Search done:"),
    print_board(Board),
    true.


% find all solutions
findall(Number) :-
    % puzzle_board(Number, Board),
    findall(_, solve(Number), Sols),
    length(Sols, N),
    write(N), writeln(" solutions.").
    % print_connected_sets(Board, Sols).

% The board can be viewed as a matrix in which each position contains an array
% of 5 variables: The amount of bridges that need to be connected to the position,
% and the amounts of briges going North, East, South, or West from the position
hashiwokakero(Board) :-
    dim(Board, [XMax, YMax, 5]), % 6 variables: Amount, N, E, S, W, visited for each position
    %Board[1..XMax, 1..YMax, 5] #:: 0..1,

    var(FirstIsland),
    ( multifor([X, Y], 1, [XMax, YMax]), param(Board, XMax, YMax, FirstIsland) do
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
            N + E + S + W #= Amount,
            ( var(FirstIsland) -> FirstIsland = [X, Y] ; true)
        ; % else make sure bridges don't cross each other
            N = S, E = W,
            (N #= 0) or (E #= 0)
        )
    ),

    true.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELPER PROCEDURES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% load the board from a puzzle fact
puzzle_board(Number, Board) :-
    % Each puzzle(Id, S, Islands) fact defines the input of one problem:
    % its identier Id, the size S (width and height), and the list of islands Islands.
    puzzle(Number, Size, Islands),

    % create a board with the islands on it
    islands_board(Islands, Size, Board).

% load the board from a matrix fact
puzzle_board(Number, Board) :-
    % create a board from a matrix that contains the islands
    board(Number, Matrix),
    matrix_board(Matrix, Board).

print_connected_sets(_, []).

print_connected_sets(Board, [ Sol | Sols ]) :-
    board_connected_set(Board, [1, 1], Sol),
    writeln("Connected set:"),
    print_board(Board),
    print_connected_sets(Board, Sols).

print_connected_sets(Board, [ _ | Sols ]) :-
    print_connected_sets(Board, Sols).

% create a usable board from an array of Islands
% each island takes the form (X, Y, N) where X is the row number, Y is the column
% number and N the number of bridges that should arrive in this island.
islands_board(Islands, Size, Board) :-
    dim(Board, [Size, Size, 5]),

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
    dim(Board, [XMax, YMax, 5]),

    % fill in the island bridge amounts first
    ( multifor([X, Y], 1, [XMax, YMax]), param(Matrix, Board) do
        Board[X, Y, 1] #= Matrix[X, Y]
    ).

% board_islands_count(Board, Count)
board_islands_count(Board, Count) :-
    dim(Board, [XMax, YMax, _]),

    Amounts1 is Board[1..XMax, 1..YMax, 1],
    flatten(Amounts1, Amounts2),
    array_list(Amounts3, Amounts2),

    ( foreachelem(Amount, Amounts3), fromto(List, Out, In, []) do
        ( Amount > 0 -> Out = [Amount | In] ; Out = In )
    ),
    length(List, Count).

board_islands(Board, Islands) :-
    dim(Board, [XMax, YMax, 5]),
    board_islands(Board, 1, 1, XMax, YMax, 1, Islands),
    board_islands_count(Board, Count),
    length(Islands, Count).

board_islands(Board, X, Y, X, Y, Count, Islands):-
    Amount is Board[X, Y, 1],
    ( Amount > 0 ->
        nth1(Count, Islands, [X, Y])
    ;
        true
    )
    .
board_islands(Board, XNext, YNext, XMax, YMax, Count, Islands) :-
    XNext =< XMax,
    YNext =< YMax,
    Amount is Board[XNext, YNext, 1],
    ( YNext = YMax ->
        YNext2 is 1,
        XNext2 is XNext + 1
    ;
        YNext2 is YNext + 1,
        XNext2 is XNext
    ),
    % TODO: PROBLEM HERE
    % Uncomment this below, to get endless loop
    % XNext =< XMax,
    %writeln(["XNext", XNext2, "YNext", YNext2]),
    ( Amount > 0 ->
        nth1(Count, Islands, [XNext, YNext]),
        CountNext is Count + 1
    ;
        CountNext is Count
    ),
    board_islands(Board, XNext2, YNext2, XMax, YMax, CountNext, Islands).

% Create list Set, set its length, fill the set by visiting the given island's neighbors
board_connected_set(Board) :-
    %dim(Board, [XMax, YMax, 6]), % 6 variables: Amount, N, E, S, W, visited for each position
    writeln("board_connected_set"),
    board_islands(Board, Islands),
    writeln(Islands),
    length(Islands, N),
    length(Visited, N),
    %writeln(["Amount of islands: ", N]),

    % make the island be member of current set
    nth1(1, Islands, [X, Y]),
    %writeln(["         board_connected_set --- getting position: ", [X, Y], " --- ", Islands]),

    % set position to visited
    nth1(1, Visited, 1),

    % travel to the neighbors of the current position and fill the current set
    % DFS
    %writeln("Start"),

    fill_set_visit(Board, X, Y, Islands, Visited),
    %writeln("finished visiting"),
    count_nonvars(Visited, Count),
    writeln(Count),
    Count = N,
    true.

fill_set_visit(Board, X, Y, Islands, Visited) :-
    island_neighbors(Board, X,Y, Neighbors),
    %writeln(Neighbors),
    length(Neighbors, N),
    writeln(["         fill_set_visit --- getting position: ", [X, Y], " --- ", Neighbors]),

    % TODO
    % PROBLEM IS HERE SOMEWHERE, AFTER FAILURE IT BACKTRACKS UNTILL HERE AND ADD'S A NEW VAR????
    % LOOK FOR THE finished visiting STATEMENT, AFTER THIS STATEMENT IT COMES BACK TO HERE
    % ONLY IF THE COUNT IS NOT THE SAME AS THE AMOUNT OF ISLANDS (board_connected_set)
    %writeln(["Pos: ", X, Y, " has neighbors ",Neighbors]),

    ( for(I,1,N), param(Board, Islands, Visited, Neighbors) do
        %writeln(["I", I]),
        nth1(I, Neighbors, [X1,Y1]),

        nth1(Pos, Islands, [X1,Y1]),
        nth1(Pos, Visited, HasVisited),
        %member([X1,Y1], Neighbors),
        writeln(["Checking neighbor: ", X1, Y1, "Which has Index: ", Pos, " in visited and has been visited: ", HasVisited, "Visited set: ", Visited ]),
        %writeln(["Neighbors ",Neighbors]),

        % If it is still a var, we haven't visited this islands yet so let's go and visit it :D
        (var(HasVisited) ->
            HasVisited is 1,
            %writeln(["         fill_set_visit --- Travelling to: ", [X1, Y1], " --- ", Visited]),

            fill_set_visit(Board, X1, Y1, Islands, Visited)
            ;
            true
        )
    ),
    %http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse43
    !
    .

neighbors_amount([], 0):-
    true.

neighbors_amount([H|T], Count):-
    H > 0,
    neighbors_amount(T, C2),
    Count is C2 + 1.


neighbors_amount([H|T], Count):-
    neighbors_amount(T, C2),
    Count is C2
    .

% Neighbors is a list of neighbors of Pos on the Board
island_neighbors(Board, X,Y, Neighbors) :-
    %writeln(["Checking island_neighbors for :", X, Y]),
    List is Board[X,Y, 2..5],
    neighbors_amount(List,Count),
    length(Neighbors, Count),
    ( foreachelem(Direction, [](2, 3, 4, 5)), param(Board, X,Y, Neighbors) do
        Val is Board[X,Y,Direction],
        (Val > 0 ->
            next_pos([X,Y], Direction, NextPos),
            find_neighbor(Board, NextPos, Direction, Neighbor),
            member(Neighbor, Neighbors)
            ;
            true
        )
    ),
    true.

% Neighbor is a possible neighbor in a certain direction from position (X, Y) on the Board
find_neighbor(Board, [X, Y], Direction, Neighbor) :-
        dim(Board, [XMax, YMax, _]),

        X > 0,
        X =< XMax,
        Y > 0,
        Y =< YMax,

        Amount is Board[X, Y, 1],
        ( Amount > 0 ->
            Neighbor = [X, Y]
        ;
            next_pos([X, Y], Direction, NextPos),
            find_neighbor(Board, NextPos, Direction, Neighbor)
        ).

% count the nonvars of a list, assuming that all of the nonvars are at the end of the list
count_nonvars([], 0).
count_nonvars([ Head | _ ], 0) :-
    var(Head).
count_nonvars([ Head | Tail ], Count) :-
    nonvar(Head),
    count_nonvars(Tail, Count2),
    Count is Count2 + 1.

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
                    write(' ')
                )
            )
        ),
        nl
    ),
    nl, nl.

symbol(0, 0, ' ').
symbol(0, 1, '-').
symbol(0, 2, '=').
symbol(1, 0, '|').
symbol(2, 0, '"').

next_pos([X, Y], 2, [X2, Y]) :- X2 is X-1. % north
next_pos([X, Y], 4, [X2, Y]) :- X2 is X+1. % south
next_pos([X, Y], 3, [X, Y2]) :- Y2 is Y+1. % east
next_pos([X, Y], 5, [X, Y2]) :- Y2 is Y-1. % west

direction(2, "North").
direction(3, "East").
direction(4, "South").
direction(5, "West").

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

% board that cannot be solved
board(7, [](
    [](1, 0, 1, 0, 2),
    [](0, 0, 0, 0, 0),
    [](0, 0, 0, 0, 2)
)).


board(8, [](
    [](1, 0, 2, 0, 3),
    [](0, 0, 0, 0, 0),
    [](0, 0, 0, 0, 2)
)).

board(9, [](
    [](2, 0, 0, 0, 2),
    [](0, 0, 0, 0, 0),
    [](2, 0, 0, 0, 2)
)).
