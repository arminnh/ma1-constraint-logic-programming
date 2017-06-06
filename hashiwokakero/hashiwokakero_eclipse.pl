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
% Solver was started from http://stackoverflow.com/questions/20337029/hashi-puzzle-representation-to-solve-all-solutions-with-prolog-restrictions/20364306#20364306
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- lib(ic).
:- lib(lists).
:- import nth1/3 from listut.
:- [boards].

% solve a given game board
solve(Number) :-
    % find the game board
    puzzle_board(Number, Board),
    writeln("Given board:"),
    print_board(Board),

    % create constraints
    hashiwokakero(Board),
    writeln("Board before search:"),
    print_board(Board),

    % do search on variables
    search(naive, Board),
    % Check that the islands form a connected set
    board_connected_set(Board),
    writeln("Board after search:"),
    print_board(Board).

% find all solutions for a given game board
findall(Number) :-
    findall(_, solve(Number), Sols),
    length(Sols, N),
    write(N),
    writeln(" solution(s).").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MAIN CONSTRAINTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The board can be viewed as a matrix in which each position contains an array
% of 5 variables: The amount of bridges that need to be connected to the position,
% and the amounts of briges going North, East, South, or West from the position
hashiwokakero(Board) :-
    % 5 variables: Amount, N, E, S, W, visited for each position
    dim(Board, [XMax, YMax, 5]),

    ( multifor([X, Y], 1, [XMax, YMax]), param(Board, XMax, YMax) do
        Amount is Board[X, Y, 1],
        N is Board[X, Y, 2],
        E is Board[X, Y, 3],
        S is Board[X, Y, 4],
        W is Board[X, Y, 5],

        % the amount of bridges going in one direction equals the amount of bridges going in the
        % opposite directio from the next position in the original direction. If the position is on an
        % edge of the board, the amount of bridges in the direction that would go outside of the board is zero.
        ( X > 1    -> N #= Board[X-1,   Y, 4] ; N = 0 ),
        ( Y < YMax -> E #= Board[  X, Y+1, 5] ; E = 0 ),
        ( X < XMax -> S #= Board[X+1,   Y, 2] ; S = 0 ),
        ( Y > 1    -> W #= Board[  X, Y-1, 3] ; W = 0 ),

        % if the current position requires an amount of bridges, make the sum of all bridges equal this amount
        ( Amount > 0 ->
            [N, E, S, W] #:: 0..2,
            N + E + S + W #= Amount
        ;
            % else make sure that bridges going in one directoin equals the amount of
            % bridges going in the opposite direction and that bridges don't cross each other
            N = S, E = W,
            (N #= 0) or (E #= 0)
        ),

        % add some improvements
        improve(Board, X, Y, Amount),
        true
    ).

% verifies whether the islands on the Board form a connected set. Done by stating that
% from a certain island all other islands on the board can be visited.
board_connected_set(Board) :-
    board_islands(Board, Islands),
    length(Islands, N),
    % Visited is a list of free variables, a bound variable in the list means that a certain island has been visited
    length(Visited, N),

    % get the first island in Islands
    nth1(1, Islands, [X, Y]),
    % set position to visited
    nth1(1, Visited, 1),

    % travel to the neighbors of the selected island and update the Visited set.
    visit_islands(Board, X, Y, Islands, Visited),

    % if all free variables in Visited have been bound, then all islands form a connected set
    count_nonvars(Visited, N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ADDITIONAL IMPROVEMENT CONSTRAINTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

improve(Board, X, Y, Amount) :-
    no_one_to_one_isolation(Board, X, Y, Amount),
    no_two_to_two_isolation(Board, X, Y, Amount),
    no_two_with_three_neighbors_isolation(Board, X, Y, Amount),
    no_three_with_three_neighbors_isolation(Board, X, Y, Amount),
    true.

% There cannot be a bridge between two islands with 1.
no_one_to_one_isolation(Board, X, Y, 1):-
    possible_island_neighbors(Board, [X, Y], Neighbors),
    length(Neighbors, Count),

    (Count > 1 ->
        ( for(I, 1, Count), param(Board, X, Y, Neighbors) do
            nth1(I, Neighbors, Neigbor),
            nth1(3, Neigbor, NeighborAmount),

            (NeighborAmount =:= 1 ->
                nth1(4, Neigbor, Dir),
                D is Board[X, Y, Dir],
                D = 0
            ;
                true
            )

            %% % if the amount of the neighbor equals 1,
            %% %then the amount of bridges going to that neighbor must be zero
            %% nth1(3, Neigbor, 1),
            %% nth1(4, Neigbor, Dir),
            %% D is Board[X, Y, Dir],
            %% D = 0
        )
    ;
        true
    ).
no_one_to_one_isolation(_, _, _, _).

% There cannot be two bridges between two islands with 2.
no_two_to_two_isolation(Board, X, Y, 2):-
    possible_island_neighbors(Board, [X, Y], Neighbors),
    length(Neighbors, Count),

    (Count > 1 ->
        ( for(I, 1, Count), param(Board, X, Y, Neighbors) do
            nth1(I, Neighbors, Neigbor),
            nth1(3, Neigbor, NeighborAmount),

            (NeighborAmount =:= 2 ->
                nth1(4,Neigbor,Dir),
                D is Board[X,Y, Dir],
                D \== 2
            ;
                true
            )

            %% % if the amount of the neighbor equals 2,
            %% %then the amount of bridges going to that neighbor cannot be 2
            %% nth1(3, Neigbor, 2),
            %% nth1(4, Neigbor, Dir),
            %% D is Board[X, Y, Dir],
            %% D \== 2
        )
    ;
        true
    ).
no_two_to_two_isolation(_, _, _, _).

% Board 21
% If an island with 2 has 3 neighbors, of which two are a 1,
% then the amount of bridges going to the third neighbor must be larger than zero (#\= 0).
no_two_with_three_neighbors_isolation(Board, X, Y, 2):-
    possible_island_neighbors(Board, [X, Y], Neighbors),
    length(Neighbors, 3),

    look_for_value(Neighbors, 1, I1, _, 3),
    (I1 > 0 ->
        nth1(I1, Neighbors, N1),
        delete(N1, Neighbors, R1),
        look_for_value(R1, 1, I2, _, 2),

        (I2 > 0 ->
            nth1(I2, R1, N2),
            delete(N2, R1, [Neighbor]),
            nth1(4, Neighbor, D),
            Board[X, Y, D] #\= 0
        ;
            true
        )
    ;
        true
    ).
no_two_with_three_neighbors_isolation(_, _, _, _).

% Board 21
% If an island with 3 has 3 neighbors, of which one is a 1 and another is a 2,
% then the amount of bridges going to the third neighbor must be larger than zero (#\= 0)
no_three_with_three_neighbors_isolation(Board, X, Y, 3):-
    possible_island_neighbors(Board, [X, Y], Neighbors),
    length(Neighbors, 3),

    look_for_value(Neighbors, 1, I1, _, 3),
    look_for_value(Neighbors, 2, I2, _, 3),

    (I1 > 0, I2 > 0 ->
        subtract([1, 2, 3], [I1, I2], [I]),
        nth1(I, Neighbors, Neighbor),
        nth1(4, Neighbor, D),
        Board[X, Y, D] #\= 0
    ;
        true
    ).
no_three_with_three_neighbors_isolation(_, _, _, _).

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

% Islands is a list of islands that are on the given Board. Done by passing over the entire board.
% nth1 is used to prevent permutations causing many solutions. Maybe this could have been done in a
% much simpler way using cuts instead, but no time left to try that approach.
board_islands(Board, Islands) :-
    dim(Board, [XMax, YMax, 5]),
    board_islands(Board, 1, 1, XMax, YMax, 1, Islands),
    board_islands_count(Board, Count),
    length(Islands, Count).
board_islands(Board, X, Y, X, Y, Count, Islands):-
    Amount is Board[X, Y, 1],
    ( Amount > 0 -> nth1(Count, Islands, [X, Y]) ; true ).
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

% Visited is a list which represents which of the islands in the Islands list can be visited
% if the N-th variable in Visited is a 1, then the N-th island in Islands can be visited
visit_islands(Board, X, Y, Islands, Visited) :-
    island_neighbors(Board, X, Y, Neighbors),
    length(Neighbors, N),

    ( for(I,1,N), param(Board, Islands, Visited, Neighbors) do
        nth1(I, Neighbors, [X1, Y1, _, _]),
        nth1(Pos, Islands, [X1, Y1]),
        nth1(Pos, Visited, HasVisited),

        % if HasVisited is a var, the neighbor has not been visited yet, so it can be visited now
        (var(HasVisited) -> HasVisited is 1, visit_islands(Board, X1, Y1, Islands, Visited) ; true )
    ),
    !.

% Neighbors is a list of neighboring islands (not just positions) of position (X, Y) on the Board
island_neighbors(Board, X, Y, Neighbors) :-
    List is Board[X, Y, 2..5],
    count_nonzero_nonvars(List, Count),
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
    ).

% Neighbors is a list a list of possible neighbors of position Pos on the board
% a neighbor is a possible neighbor when it is an island which can be connected by a bridge
possible_island_neighbors(Board, Pos, Neighbors) :-
    ( foreachelem(Direction, [](2, 3, 4, 5)), param(Board, Pos, Neighbors) do
        next_pos(Pos, Direction, NextPos),
        find_neighbor(Board, NextPos, Direction, Neighbor),
        member(Neighbor, Neighbors)
    ),
    length(Neighbors, _),
    !.

% Neighbor is a possible neighbor in a certain direction from position (X, Y) on the Board
find_neighbor(Board, [X, Y], Direction, Neighbor) :-
    dim(Board, [XMax, YMax, _]),
    X > 0, X =< XMax,
    Y > 0, Y =< YMax,

    Amount is Board[X, Y, 1],
    ( Amount > 0 ->
        Neighbor = [X, Y, Amount, Direction]
    ;
        next_pos([X, Y], Direction, NextPos),
        find_neighbor(Board, NextPos, Direction, Neighbor)
    ).
find_neighbor(_, _, _, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELPER PROCEDURES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% load the Board from a puzzle fact
% Each puzzle(Id, S, Islands) fact defines the input of one problem:
% its identifier Id, the size S (width and height), and the list of islands Islands.
puzzle_board(Number, Board) :-
    puzzle(Number, Size, Islands),

    % create the board array
    dim(Board, [Size, Size, 5]),

    % put the islands on the board
    islands_board(Islands, Board),

    % then fill in zeros for the rest of the board
    ( foreacharg(Row, Board) do
        ( foreacharg(Position, Row) do
            Amount is Position[1],
            ( var(Amount) -> Position[1] #= 0 ; true )
        )
    ).

% load the board from a matrix (prolog list of lists) fact that contains the islands
puzzle_board(Number, Board) :-
    board(Number, Matrix),
    matrix_board(Matrix, Board).

% fill in a list of islands on a Board array
islands_board([], _).
islands_board([ (X, Y, Amount) | Islands ], Board) :-
    Board[X, Y, 1] #= Amount,
    islands_board(Islands, Board).

% create a usable Board from a matrix (prolog list of lists) that contains the islands
matrix_board(Matrix, Board) :-
    length(Matrix, XMax),
    nth1(1, Matrix, FirstRow),
    length(FirstRow, YMax),
    dim(Board, [XMax, YMax, 5]),

    % fill in the island bridge amounts first
    ( multifor([X, Y], 1, [XMax, YMax]), param(Matrix, Board) do
        nth1(X, Matrix, Row),
        nth1(Y, Row, Amount),
        Board[X, Y, 1] #= Amount
    ).

% count the nonvars of a list, assuming that all of the nonvars are at the end of the list
count_nonvars([], 0).
count_nonvars([ Head | _ ], 0) :-
    var(Head).
count_nonvars([ Head | Tail ], Count) :-
    nonvar(Head),
    count_nonvars(Tail, Count2),
    Count is Count2 + 1.

% counts the nonzero nonvars in a list
count_nonzero_nonvars([], 0).
count_nonzero_nonvars([H | T], Count):-
    nonvar(H),
    H > 0,
    count_nonzero_nonvars(T, C2),
    Count is C2 + 1.
count_nonzero_nonvars([_ | T], Count):-
    count_nonzero_nonvars(T, C2),
    Count is C2.

% Index is the position of the value we looked for
% Index2 is the index passed from a smaller part of the list
% Counter is to count which index we are now
% Length is the length of the list
look_for_value([], _, 0, Length, Length).
look_for_value([ [_, _, Am, _] | T ], Val, Index, Counter, Length):-
    look_for_value(T, Val, Index2, C2, Length),

    (Am =:= Val -> Index is C2 ; Index is Index2 ),
    Counter is C2 - 1.

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
