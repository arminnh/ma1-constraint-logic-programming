:- lib(ic).
% :- import alldifferent/1, sorted/2 from ic_global.
% :- coroutine.
% :- lib(lists).
% :- import nth1/3 from listut.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Hashiwokakero, also called Bridges is a logic puzzle in which dierent islands
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
    % Each puzzle(Id, S, Islands) fact denes the input of one problem:
    % its identier Id, the size S (width and height), and the list of islands Islands.
    puzzle(Number, Size, Islands),

    % create a board with the islands on it
    hashiwokakero_board(Islands, Size, IslandBoard),
    writeln("Given board:"),
	print_board(IslandBoard),

    % create bridges and set constraints
    hashiwokakero(IslandBoard, Bridges),

    % do search on variables
	search(naive, Bridges),

    % print results
    writeln("Search done:"),
    print_board(IslandBoard, Bridges).

% IslandBoard is an square of zeros and island values,
% Bridges is an array of arrays of 4 variables North, East, South, West
% Those variables represent how many bridges are going in a certain direction
hashiwokakero(IslandBoard, Bridges) :-
    dim(IslandBoard, [XMax, YMax]),
    dim(Bridges, [XMax, YMax, 4]),   % 4 variables N, E, S, W for each field

    ( foreachindex([X, Y], IslandBoard), param(IslandBoard, Bridges, XMax, YMax) do
        Amount is IslandBoard[X, Y],
        N is Bridges[X, Y, 1],
        E is Bridges[X, Y, 2],
        S is Bridges[X, Y, 3],
        W is Bridges[X, Y, 4],

        % if this position is not on the edges of the board, then the amount of bridges
        % going in one direction needs to equals the amount in the other direction
        ( X > 1    -> N #= Bridges[X-1,   Y, 3] ; N = 0 ),
        ( X < XMax -> S #= Bridges[X+1,   Y, 1] ; S = 0 ),
        ( Y > 1    -> W #= Bridges[  X, Y-1, 2] ; W = 0 ),
        ( Y < YMax -> E #= Bridges[  X, Y+1, 4] ; E = 0 ),

        % if this position requires an amount of bridges,
        % make the sum of all bridges equal this amount
        ( Amount > 0 ->
            [N, E, S, W] #:: 0..2,
            N + E + S + W #= Amount
        ; % else make sure bridges don't cross each other
            N = S, E = W,
            (N #= 0) or (E #= 0)
        ),
        true
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELPER PROCEDURES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Create a usable board from the given Islands
% Each island takes the form (X, Y,  N) where X is the row number, Y is the column
% number and N the number of bridges that should arrive in this island.
hashiwokakero_board(Islands, Size, IslandBoard) :-
    dim(IslandBoard, [Size, Size]),

    % fill in the island values first
    ( foreacharg(Island, Islands), param(IslandBoard) do
        X is Island[1],
        Y is Island[2],
        Amount is Island[3],
        IslandBoard[X, Y] #= Amount
    ),

    % then fill in zeros
    ( foreachelem(Position, IslandBoard) do
        ( var(Position) -> Position #= 0 ; true )
    ).

print_board(IslandBoard) :-
    ( foreachindex([X, Y], IslandBoard), param(IslandBoard) do
        ( Y = 1 -> nl ; true ),
        Amount is IslandBoard[X, Y],
        write(Amount),
        write(' ')
    ),
    nl, nl.

print_board(IslandBoard, Bridges) :-
    ( foreachindex([X, Y], IslandBoard), param(IslandBoard, Bridges) do
        ( Y = 1 -> nl ; true ),
        Amount is IslandBoard[X,Y],
        ( Amount > 0 ->
            write(Amount)
        ;
            NS is Bridges[X, Y, 1],
            EW is Bridges[X, Y, 2],
            symbol(NS, EW, Char),
            write(Char)
        ),
        write(' ')
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
    [](7,1,3), [](7,4,3), [](7,5,1), [](7,7,2))).

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
    [](13,1,3), [](13,6,3), [](13,8,1),  [](13,10,2), [](13,13,2))).

% puzzle 3
% http://www.conceptispuzzles.com/index.aspx?uri=puzzle/hashi/techniques
puzzle(3, 6, [](
    [](1,1,1), [](1,3,4), [](1,5,2),
    [](2,4,2), [](2,6,3),
    [](3,1,4), [](3,3,7), [](3,5,1),
    [](4,4,2), [](4,6,5),
    [](5,3,3), [](5,5,1),
    [](6,1,3), [](6,4,3), [](6,6,3))).

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
    [](8,2,2), [](8,4,3), [](8,7,2))).
