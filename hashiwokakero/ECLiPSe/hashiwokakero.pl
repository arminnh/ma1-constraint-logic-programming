% http://tidel.mie.utoronto.ca/pubs/explorations.pdf
% https://www.google.be/search?q=constraint+programming+connectivity&ie=UTF-8&sa=Search&channel=fe&client=browser-ubuntu&hl=en&gws_rd=cr,ssl&ei=BtkaWbw8wrBp6LeukAg

:- lib(ic).
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
    %writeln("kk"),

    % do search on variables
    search(naive, Board),

    % Check that everything is connected
    board_islands(Board, AllIslands),

    length(AllIslands, N),

    (for(I,1, N), param(AllIslands, Board, N) do
        (for(J, 1, N), param(AllIslands, Board) do
            nth1(I, AllIslands, Islands1),
            nth1(J, AllIslands, Islands2),
            connected(Board, Islands1, Islands2)
        )
    ),

    % print results
    writeln("Search done:"),
    print_board(Board).

% find all solutions
findall(Number) :-
    findall(_, solve(Number), Sols),
    length(Sols, N),
    write(N),
    writeln(" solution(s).").


incr_list([], _).
incr_list([J|T], J) :-
       H is J+1,
       incr_list(T, H).

% The board can be viewed as a matrix in which each position contains an array
% of 5 variables: The amount of bridges that need to be connected to the position,
% and the amounts of briges going North, East, South, or West from the position
hashiwokakero(Board) :-
    dim(Board, [XMax, YMax, 6]), % 6 variables: Amount, N, E, S, W, visited for each position
    Board[1..XMax, 1..YMax, 6] #:: 0..1,

    var(FirstIsland),

    ( multifor([X, Y], 1, [XMax, YMax]), param(Board, XMax, YMax, FirstIsland) do
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

    print_board(Board),
    %board_connected_set(Board, FirstIsland, Set),
    %writeln(Set),

    ( foreacharg(Row, Board) do
        ( foreacharg(Vars, Row) do
            Visited is Vars[6],
            ( nonvar(Visited) -> true ; Visited #= 0)
        )
    ),

    true.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOADING BOARDS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% load the Board from a puzzle fact
puzzle_board(Number, Board) :-
    % Each puzzle(Id, S, Islands) fact defines the input of one problem:
    % its identifier Id, the size S (width and height), and the list of islands Islands.
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


connected(Board, Islands1, Islands2) :-
    X \= Islands1,
    X \= Islands2,
    connected(Board, Islands1, X), connected(Board, X, Islands2).

connected(Board, [X, Y], [X, Y]) :-
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ISLANDS PROCEDURES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Count is the amount of islands on the Board
board_islands_count(Board, Count) :-
    dim(Board, [XMax, YMax, _]),

    Amounts1 is Board[1..XMax, 1..YMax, 1],
    flatten(Amounts1, Amounts2),
    array_list(Amounts3, Amounts2),

    ( foreachelem(Amount, Amounts3), fromto(List, Out, In, []) do
        ( Amount > 0 -> Out = [Amount | In] ; Out = In )
    ),
    length(List, Count).

% Islands is a list of islands that are on the given Board. Done by passing over the entire board. nth1 is used to prevent permutations causing many solutions. TODO: try with 1 simple procedure using cuts instead?
board_islands(Board, Islands) :-
    dim(Board, [XMax, YMax, 6]),
    board_islands(Board, 1, 0, XMax, YMax, 1, Islands).

% Islands is a list of islands that are on the given Board. Done by passing over the entire board. nth1 is used to prevent permutations causing many solutions. TODO: try with 1 simple procedure using cuts instead?
board_islands(Board, X, Y, X, Y, Count, Islands):-
    Amount is Board[X, Y, 1],
    ( Amount > 0 ->
        nth1(Count, Islands, [X, Y])
    ;
        true
    ).

% Islands is a list of islands that are on the given Board. Done by passing over the entire board. nth1 is used to prevent permutations causing many solutions. TODO: try with 1 simple procedure using cuts instead?
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

    ( Amount > 0 ->
        nth1(Count, Islands, [XNext, YNext]),
        CountNext is Count + 1
    ;
        CountNext is Count
    ),
    board_islands(Board, XNext2, YNext2, XMax, YMax, CountNext, Islands).

% Islands is a list of islands on the Board that form a connected set starting from a certain island. Visited is used to prevent visiting islands multiple times.
fill_set_visit(Board, X, Y, Islands, Visited) :-
    island_neighbors(Board, X,Y, Neighbors),
    %writeln(Neighbors),
    length(Neighbors, N),
    % writeln(["         fill_set_visit --- getting position: ", [X, Y], " --- ", Neighbors]),

    ( for(I,1,N), param(Board, Islands, Visited, Neighbors) do
        %writeln(["I", I]),
        nth1(I, Neighbors, [X1,Y1]),

        nth1(Pos, Islands, [X1,Y1]),
        nth1(Pos, Visited, HasVisited),
        % member([X1,Y1], Neighbors),
        % writeln(["Checking neighbor: ", X1, Y1, "Which has Index: ", Pos, " in visited and has been visited: ", HasVisited, "Visited set: ", Visited ]),
        % writeln(["Neighbors ",Neighbors]),

        % If it is still a var, we haven't visited this islands yet so let's go and visit it :D yaayyyy :D :D :D i agree!
        (var(HasVisited) ->
            HasVisited is 1,
            fill_set_visit(Board, X1, Y1, Islands, Visited)
        ;
            true
        )
    ),
    !.

% TODO pls fill in
neighbors_count([], 0):-
    true.

% TODO pls fill in
neighbors_count([H | T], Count):-
    H > 0,
    neighbors_count(T, C2),
    Count is C2 + 1.

% TODO pls fill in
neighbors_count([_ | T], Count):-
    neighbors_count(T, C2),
    Count is C2.

% Neighbors is a list of neighboring islands (not just positions) of position (X, Y) on the Board
island_neighbors(Board, X, Y, Neighbors) :-
    List is Board[X, Y, 2..5],
    neighbors_count(List, Count),
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

% TODO: was this wrong?
% find_neighbor(_, _, _, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELPER PROCEDURES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

board(10, [](
    [](2, 0, 3, 0, 0, 0, 4, 0, 0, 0, 2, 0, 0),  
    [](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
    [](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2),
    [](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    [](1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 3, 0, 3),
    [](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    [](2, 0, 0, 0, 0, 0, 8, 0, 0, 0, 5, 0, 2),
    [](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    [](3, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 1),
    [](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    [](0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 3, 0, 4),
    [](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    [](3, 0, 0, 0, 0, 0, 3, 0, 1, 0, 0, 0, 2)
)).

board(11, [](
    [](0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 3, 0, 2, 0),  
    [](4, 0, 0, 0, 4, 0, 0, 3, 0, 0, 0, 4, 0, 4, 0, 0, 2, 0, 0, 0, 1),  
    [](0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 4, 0),  
    [](4, 0, 0, 0, 0, 0, 0, 0, 2, 0, 3, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0),  
    [](0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),  
    [](0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0),  
    [](0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 3, 0, 0, 0, 0, 0),  
    [](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0),  
    [](0, 4, 0, 0, 0, 0, 0, 0, 2, 0, 0, 3, 0, 0, 0, 0, 0, 4, 0, 1, 0),  
    [](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 3, 0, 1, 0, 0, 0, 0, 0),  
    [](0, 0, 3, 0, 6, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 2, 0),  
    [](0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 4, 0, 6, 0, 0, 0, 0, 0, 0, 5),  
    [](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),  
    [](0, 0, 2, 0, 4, 0, 1, 0, 2, 0, 0, 3, 0, 4, 0, 0, 0, 0, 2, 0, 0),  
    [](0, 5, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6),  
    [](0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 4, 0, 0, 0, 0, 0, 0, 1, 0),  
    [](3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),  
    [](0, 3, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 5),  
    [](0, 0, 0, 1, 0, 0, 0, 4, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0),  
    [](0, 3, 0, 0, 0, 4, 0, 0, 0, 2, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 2),  
    [](1, 0, 2, 0, 3, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 4, 0, 2, 0)
)).
    % do search on variables
    search(naive, Board),

    % Check that everything is connected
    %writeln("connected"),
    board_connected_set(Board),

    dim(Board, [XMax, YMax, 5]), % 5 variables: Amount, N, E, S, W, visited for each position
    ( multifor([X, Y], 1, [XMax, YMax]), param(Board, XMax, YMax) do
        % if this position requires an amount of bridges,
        % make the sum of all bridges equal this amount
        ( Amount > 0 ->
            [N, E, S, W] #:: 0..2,
            N + E + S + W #= Amount
        ; % else make sure bridges don't cross each other
            N = S, E = W,
            (N #= 0) or (E #= 0)
        )
    ).
% checks whether the islands on the Board form a connected set. Done by starting on an island and visiting all of its neighbors and checking whether all islands have been visited.
board_connected_set(Board) :-
    board_islands(Board, Islands),
    length(Islands, N),
    length(Visited, N),
    % make the island be member of current set
    nth1(1, Islands, [X, Y]),
    % set position to visited
    nth1(1, Visited, 1),
    % travel to the neighbors of the current position and fill the Islands/Visited set
    fill_set_visit(Board, X, Y, Islands, Visited),
    % if all free variables in Visited have been bound, then all islands form a connected set
    count_nonvars(Visited, N).


% create a usable Board from an array of Islands
% create a usable Board from a matrix that contains the islands
matrix_board(Matrix, Board) :-
    dim(Board, [XMax, YMax, 5]),
    board_islands(Board, 1, 1, XMax, YMax, 1, Islands),
    board_islands_count(Board, Count),
    length(Islands, Count).
        next_pos([X, Y], Direction, NextPos),
        find_neighbor(Board, NextPos, Direction, Neighbor)