% http://tidel.mie.utoronto.ca/pubs/explorations.pdf
% https://www.google.be/search?q=constraint+programming+connectivity&ie=UTF-8&sa=Search&channel=fe&client=browser-ubuntu&hl=en&gws_rd=cr,ssl&ei=BtkaWbw8wrBp6LeukAg

:- lib(ic).
:- lib(grasper).
% :- import alldifferent/1, sorted/2 from ic_global.
% :- coroutine.
:- lib(lists).
:- import nth1/3 from listut.
:- import sumlist/2 from ic_global.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hashiwokakero, also called Bridges is a logic puzzle in which different islands
% have to be connected by bridges. A bridges puzzle consists of a square grid
% in which some numbers are placed. Squares on which a number is placed are
% referred to as islands. The goal of the puzzle is to draw bridges between
% islands subject to the following restrictions.
%     Bridges can only run horizontally or vertically.
%     Bridges run in one straight line.
%     Bridges cannot cross other bridges or islands.
%     At most two bridges connect a pair of islands.
%     The number of bridges connected to each island must match the number on that island.
%     The bridges must connect the islands into a single connected group.
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


incr_list([], _).
incr_list([J|T], J) :-
       H is J+1,
       incr_list(T, H).

% The board can be viewed as a matrix in which each position contains an array
% of 5 variables: The amount of bridges that need to be connected to the position,
% and the amounts of briges going North, East, South, or West from the position
hashiwokakero(Board) :-
    dim(Board, [XMax, YMax, 5]), % 6 variables: Amount, N, E, S, W for each position

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
    board_islands_count(Board, IslandCount),

    % 4 variables: X, Y, Value = degree , list of possible neighbors
    % The degree of a vertex is the number of edges incident on it
    dim(Vertices, [IslandCount, 4]),

    dim(Edges, [IslandCount, IslandCount]),
    Edges #:: 0..2,

    % fill in the Vertices array
    ( for(I, 1, IslandCount), param(Board, Islands, Vertices) do
        % Take i'th item from islands
        nth1(I, Islands, [X, Y]),
        Amount is Board[X, Y, 1],
        % Get the neighbors of an islands
        island_neighbors(Board, [X, Y], Neighbors),

        VX is Vertices[I, 1],
        VY is Vertices[I, 2],
        VDegree is Vertices[I, 3],
        VPossbileNeighbors is Vertices[I, 4],
        VX = X,
        VY = Y,
        VDegree = Amount,
        VPossbileNeighbors = Neighbors
    ),

    writeln(["IslandCount: ", IslandCount, "Islands: ", Islands]),
    writeln(["Vertices: ", Vertices]),
    writeln(["Edges: ", Edges]),

    ( for(I, 1, IslandCount), param(Edges, Islands, Vertices, IslandCount) do
        writeln(["I: ", I]),
        A is Vertices[I],
        writeln(["A: ", A]),
        PossibleNeighbors is Vertices[I, 4],
        writeln(["PossibleNeighbors: ", PossibleNeighbors]),

        % in adjacency matrix, set the edges to other nodes that cannot be neighbors to 0
        % => this means that edges are only possible between possible neighbors on the board
        ( for(J, 1, IslandCount), param(Edges, Islands, PossibleNeighbors, I) do
            writeln(["J: ", J]),
            writeln([J, Islands]),
            nth1(J, Islands, [JX, JY]),
            writeln(["[JX, JY]: ", JX, JY]),
            ( member([JX, JY], PossibleNeighbors) ->
                writeln(["[JX, JY]: is member"]),
                true
            ;
                writeln(["[JX, JY]: no member"]),
                Edge is Edges[I, J],
                writeln(["Edge is", Edge]),
                Edge #= 0
            )
        ),

        % the amount of edges leaving this vertex equals the degree of this vertex
        Row is Edges[I, 1..IslandCount],
        writeln(["sum of row: ", Row]),
        sumlist(Row, Sum),
        writeln(["sum is: ", Sum]),
        Degree is Vertices[I, 3],
        Sum #= Degree
    ),

    % set constraints on the Edges array
    ( multifor([I, J], [1, 1], [IslandCount, IslandCount]), param(Edges) do
        % no edge from a node to itsel
        SelfLoop is Edges[I, I],
        SelfLoop = 0,

        % same amount of edges in both directions
        Edges[I, J] #= Edges[J, I],

        true
        % transitivity of connectivity, BUT THIS DOESN'T WORK!!!!!!!!!
        % Edges[X, Y] > 0 && Edges[Y, Z] > 0 -> Edges[X, Z],
    ),

    % connectivity
    length(V, IslandCount),
    % I dedicate this to armin because he always 1 ups me :smirk:
    incr_list(V, 1),


    % search for possible Edges
    search(naive, Edges),

    print_matrix(Edges),

    true.

% Neighbors is a list of possible neighbors of Pos on the Board
island_neighbors(Board, Pos, Neighbors) :-
    ( foreachelem(Direction, [](2, 3, 4, 5)), param(Board, Pos, Neighbors) do
        next_pos(Pos, Direction, NextPos),
        find_neighbor(Board, NextPos, Direction, Neighbor),
        member(Neighbor, Neighbors)
    ),
    length(Neighbors, _).

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

find_neighbor(_, _, _, _).

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

% prints a matrix that is in an array variable
print_matrix(Matrix) :-
    ( foreacharg(Row, Matrix) do
        ( foreachelem(Edge, Row) do
            write(Edge), write(" ")
        ), nl
    ).

connected(Board, Islands1, Islands2) :-
    X \= Islands1,
    X \= Islands2,
    connected(Board, Islands1, X), connected(Board, X, Islands2).

connected(_, [X, Y], [X, Y]) :-
    true.

connected(Board, Islands1, Islands2) :-
    bridge(Board, Islands1, Islands2).


% We had problems with internal xor so created our own
xor_bool( 0 , 0 , 0).
xor_bool( 0 , 1 , 1).
xor_bool( 1 , 0 , 1).
xor_bool( 1 , 1 , 0).

%The bridge function is to check if there is an immediate bridge between two Islands
% This bridge can only move into the NSEW direction and not diagonal!
bridge(Board, [StartX,StartY], [EndX,EndY]) :-
        XPos is StartX - EndX,
        YPos is StartY - EndY,

        IsZeroX is (XPos =\= 0),
        IsZeroY is (YPos =\= 0),

        xor_bool(IsZeroX, IsZeroY, Answer),
        % One of them is not zero
        %writeln(Answer),
        % N is Board[X, Y, 2],
        % E is Board[X, Y, 3],
        % S is Board[X, Y, 4],
        % W is Board[X, Y, 5],

        %Answer is (IsZeroX+IsZeroY) * (\+ (IsZeroX) + \+ (IsZeroY)),

        (Answer =:= 1 ->
            % Oke so our XPos is pos, this means that we have an islands on our right
            ( XPos > 0 ->
                ( for(X, 1, XPos), param(Board, StartX, StartY, EndX) do
                    % If there is a bridge the value is not zero
                    CurPos is StartX - X,
                    % Check if we are in our destination
                    (CurPos == EndX ->
                        % if we are then it's ok, we have a connection
                        true
                        ;
                        % if not check that there is a bridge
                        Board[CurPos, StartY, 2] \= 0
                        )
                )
                ;
                (XPos < 0 ->
                    NewXPos is XPos * -1,
                    ( for(X, 1, NewXPos), param(Board, StartX, StartY, EndX) do
                        % If there is a bridge the value is not zero
                        CurPos is StartX + X,
                        % Check if we are in our destination
                        (CurPos == EndX ->
                            % if we are then it's ok, we have a connection
                            true
                            ;
                            % if not check that there is a bridge
                            Board[CurPos, StartY, 4] \= 0
                            )

                    )

                ;(YPos > 0 ->
                    ( for(Y, 1, YPos), param(Board, StartX, StartY, EndY) do
                        % If there is a bridge the value is not zero
                        CurPos is StartY - Y,
                        % Check if we are in our destination
                        (CurPos == EndY ->
                            % if we are then it's ok, we have a connection
                            true
                            ;
                            % if not check that there is a bridge
                            Board[StartX, CurPos, 5] \= 0
                            )
                    )
                    ;
                (YPos < 0 ->
                    NewYPos is YPos * -1,
                    ( for(Y, 1, NewYPos), param(Board, StartX, StartY, EndY) do
                        % If there is a bridge the value is not zero
                        CurPos is StartY + Y,
                        % Check if we are in our destination
                        (CurPos == EndY ->
                            % if we are then it's ok, we have a connection
                            true
                            ;
                            % if not check that there is a bridge
                            Board[StartX, CurPos, 3] \= 0
                            )
                    )
                    ;
                    false)
                    )
                )
            )
        ;
        false
        ).


    % Als islands 2 links Islands1 => x = 0, YPos positief (N)
    % Als islands 2 rechts => YPos neg (S)
    % Als islands2 boven dan Xpos pos (N)
    % Als islands2 onder dan XPos neg (S)




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
    board_islands(Board, 1, 0, XMax, YMax, 1, Islands).

board_islands(_, X, Y, X, Y, _, _).
board_islands(Board, X, Y, XMax, YMax, Count, Islands) :-
    X =< XMax,
    Y =< YMax,
    ( Y = YMax ->
        YNext is 1,
        XNext is X + 1
    ;
        YNext is Y + 1,
        XNext is X
    ),
    Amount is Board[XNext, YNext, 1],
    ( Amount > 0 ->
        nth1(Count, Islands, [XNext, YNext]),
        CountNext is Count + 1
    ;
        CountNext is Count
    ),
    board_islands(Board, XNext, YNext, XMax, YMax, CountNext, Islands).

% Create list Set, set its length, fill the set by visiting the given island's neighbors
board_connected_set(Board, Set) :-
    board_islands_count(Board, IslandCount),
    writeln(["Amount of islands: ", IslandCount]),
    length(Set, IslandCount),

    % make the island be member of current set
    nth1(1, Set, [X, Y]),
    writeln(["  member of set:", Set]),

    % set position to visited
    Board[X, Y, 6] #= 1,

    % travel to the neighbors of the current position and fill the current set
    % DFS
    fill_set_visit(Board, X, Y, Set),

    array_list(SetArray, Set),
    ( foreachelem(Island, SetArray) do
        nonvar(Island)
    ).

fill_set_visit(Board, X, Y, Set) :-
    ( foreachelem(Direction, [](2, 3, 4, 5)), param(Board, X, Y, Set) do
        Val #= Board[X, Y, Direction],
        ( Val #> 0 ->
            direction(Direction, Word),
            write("[    visiting "), write(Word),
            next_pos([X, Y], Direction, Pos),
            write(", got pos: "), writeln([Pos]),
            % Amount of islands you already visited
            count_nonvars(Set, Count),

            % We visited this islands so +1
            SetIndex is Count + 1,

            fill_set(Board, Pos, Direction, Set, SetIndex),
            writeln(["    ", Word, " visited: ", Set]),
            true
        ;
            true
        )
    ).

fill_set(Board, [X, Y], Direction, Set, SetIndex) :-
    writeln(["         fill_set --- getting position: ", [X, Y], " --- ", Set]),
    Vars is Board[X, Y],
    writeln(["         fill_set --- vars at position:", Vars]),
    Visited is Vars[6],
    ( nonvar(Visited) ->
        writeln(["         already visited: "]),
        true
    ;
        Visited #= 1,
        writeln(["         visited: ", [X, Y]]),
        Amount is Vars[1],
        writeln(["         amount of bridges: ", Amount]),
        ( Amount > 0 ->
            % Add islands to set
            nth1(SetIndex, Set, [X, Y]),
            writeln(["         member of set: ", Set]),
            % Then start viting next islands?
            fill_set_visit(Board, X, Y, Set)
        ;
            next_pos([X, Y], Direction, Pos),
            writeln(["         moving on to: ", Pos]),
            fill_set(Board, Pos, Direction, Set, SetIndex)
        )
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
