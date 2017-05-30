:- use_module(library(chr)).

:- chr_constraint solve/1, puzzle_board/1, bridge_constraints/0.
:- chr_constraint enum/1, enum_board/0.
:- chr_constraint make_domains/0, domain_list/1.
:- chr_constraint create_islands/1, create_empty_board/2.
:- chr_constraint board/7, island/3,  xmax/1, ymax/1, print_board/0, print_board/2.
:- chr_constraint board_facts_from_row/3, board_facts_from_matrix/2.
:- chr_constraint diff/2, clear_store/0, connected/0, connected/2.
:- chr_constraint create_connection/3, create_connection/5, pick_first_island/0, reachable/2.

:- op(700, xfx, in).
:- op(700, xfx, le).
:- op(700, xfx, eq).
:- op(700, xfx, or_eq).
:- op(600, xfx, '..').
:- chr_constraint le/2, eq/2, in/2, add/3, or_eq/3, or/2.
:- chr_option(debug, off).
:- chr_option(optimize, full).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HASHIWOKAKERO SOLUTION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% solve a given game board
solve(Number) <=>
    % find the game board and load the board facts into the constraint store
    puzzle_board(Number),
    pick_first_island,
    writeln("Given board:"),
    print_board,

    upto(DomainList, 2),
    reverse(DomainList, List),
    domain_list(List),

    % create the bridge constraint rules
    bridge_constraints,
    print_board,

    % after doing all bridge constraints, make domains for remaining variables (N E S W)
    make_domains,
    % chr_show_store(user),

    % after generating all necessary domains, start the search
    enum_board,

    % all islands need to be in the reachable set
    connected,

    print_board,
    clear_store,
    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BRIDGE CONSTRAINT RULES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% amount of bridges equals island's amount
bridge_constraints, board(_, _, Amount, N, E, S, W) ==> Amount > 0 |
    add(N, E, Sum),
    add(S, W, Sum2),
    add(Sum, Sum2, Amount).

% bridges going one way == bridges going the opposite way
bridge_constraints, board(_, _, 0, N, E, S, W) ==>
    N = S, E = W.

% bridges dont cross
board(_, _, 0, N, E, _, _) ==> number(N), N > 0 | E = 0.
board(_, _, 0, N, E, _, _) ==> number(E), E > 0 | N = 0.

% bridges going north == bridges going south in position above
bridge_constraints, board(X, Y, _, N, _, _, _), board(X2, Y, _, _, _, S, _) ==> X > 1, X2 is X-1  | eq(N, S).

% bridge can not go north at top of board
bridge_constraints, board(X, _, _, N, _, _, _)                              ==> X == 1            | N = 0.

% bridges going east == bridges going west in position to the right
bridge_constraints, board(X, Y, _, _, E, _, _), board(X, Y2, _, _, _, _, W) ==> Y2 is Y+1         | eq(E, W).

% bridge cannot go east at right of board
bridge_constraints, board(_, Y, _, _, E, _, _), ymax(Size)                  ==> Y == Size         | E = 0.

% bridges going south == bridges going north in position under
bridge_constraints, board(X, Y, _, _, _, S, _), board(X2, Y, _, N, _, _, _) ==> X2 is X+1         | eq(S, N).

% bridge cannot go south at bottom of board
bridge_constraints, board(X, _, _, _, _, S, _), xmax(Size)                  ==> X == Size         | S = 0.

% bridges going west == bridge going east at position left
bridge_constraints, board(X, Y, _, _, _, _, W), board(X, Y, _, _, E, _, _)  ==> Y > 1 , Y is Y-1  | eq(W, E).

% bridge connot go west at left of board
bridge_constraints, board(_, Y, _, _, _, _, W)                              ==> Y == 1            | W = 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DOMAIN GENERATION RULES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_domains, domain_list(Domain), board(_, _, _, N, _, _, _) ==> var(N) |
    N in Domain.

make_domains, domain_list(Domain), board(_, _, _, _, E, _, _) ==> var(E) |
    E in Domain.

make_domains, domain_list(Domain), board(_, _, _, _, _, S, _) ==> var(S) |
    S in Domain.

make_domains, domain_list(Domain), board(_, _, _, _, _, _, W) ==> var(W) |
    W in Domain.

% remove duplicate indomain constraints
X in Domain \ X in Domain <=>
    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSTRAINT IMPROVEMENT RULES
% inspired by http://www.conceptispuzzles.com/index.aspx?uri=puzzle/hashi/techniques
% improvements used:
%     "1. Islands with 4 in the corner, 6 on the side and 8 in the middle:"
%     "2. Islands with 3 in the corner, 5 on the side and 7 in the middle:"
%     "3. Special cases of 3 in the corner, 5 on the side and 7 in the middle:"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% % islands with 4 in a corner of the board have 2 connections in all remaining directions
% board(1, 1, 4, N, E, S, W)                               \ add(N, E, Sum), add(S, W, Sum2), add(Sum, Sum2, 4) <=>
%     N = 0, E = 2, S = 2, W = 0.
% board(1, YMax, 4, N, E, S, W), ymax(YMax)                \ add(N, E, Sum), add(S, W, Sum2), add(Sum, Sum2, 4) <=>
%     N = 0, E = 0, S = 2, W = 2.
% board(XMax, 1, 4, N, E, S, W), xmax(XMax)                \ add(N, E, Sum), add(S, W, Sum2), add(Sum, Sum2, 4) <=>
%     N = 2, E = 2, S = 0, W = 0.
% board(XMax, YMax, 4, N, E, S, W), xmax(XMax), ymax(YMax) \ add(N, E, Sum), add(S, W, Sum2), add(Sum, Sum2, 4) <=>
%     N = 2, E = 0, S = 0, W = 2.
%
% % islands with 6 on a side of the board have 2 connections in all remaining directions
% board(1, _, 6, N, E, S, W)                \ add(N, E, Sum), add(S, W, Sum2), add(Sum, Sum2, 6) <=>
%     N = 0, E = 2, S = 2, W = 2.
% board(_, YMax, 6, N, E, S, W), ymax(YMax) \ add(N, E, Sum), add(S, W, Sum2), add(Sum, Sum2, 6) <=>
%     N = 2, E = 0, S = 2, W = 2.
% board(XMax, _, 6, N, E, S, W), xmax(XMax) \ add(N, E, Sum), add(S, W, Sum2), add(Sum, Sum2, 6) <=>
%     N = 2, E = 2, S = 0, W = 2.
% board(_, 1, 6, N, E, S, W)                \ add(N, E, Sum), add(S, W, Sum2), add(Sum, Sum2, 6) <=>
%     N = 2, E = 2, S = 2, W = 0.
%
% % islands with 8 have 2 connections in all directions
% board(_, _, 8, N, E, S, W) \ add(N, E, Sum), add(S, W, Sum2), add(Sum, Sum2, 8) <=>
%     N = 2, E = 2, S = 2, W = 2.
%
% % % Neighbors = [ [Direction, Amount] ]
% % board(X, Y, 7, N, E, S, W), neighbors(X, Y, Neighbors) ==> member(['N', 1], Neighbors) |
% %     N = 1, E = 2, S = 2, W = 2.
% % board(X, Y, 7, N, E, S, W), neighbors(X, Y, Neighbors) ==> member(['E', 1], Neighbors) |
% %     N = 2, E = 1, S = 2, W = 2.
% % board(X, Y, 7, N, E, S, W), neighbors(X, Y, Neighbors) ==> member(['S', 1], Neighbors) |
% %     N = 2, E = 2, S = 1, W = 2.
% % board(X, Y, 7, N, E, S, W), neighbors(X, Y, Neighbors) ==> member(['W', 1], Neighbors) |
% %     N = 2, E = 2, S = 2, W = 1.
% %
% % board(1, 1, 3, N, E, S, W)                               ==> member(['N', 1], Neighbors) |
% %     N = 0, E = 2, S = 2, W = 0.
% % board(1, YMax, 3, N, E, S, W), ymax(YMax)                ==> member(['N', 1], Neighbors) |
% %     N = 0, E = 0, S = 2, W = 2.
% % board(XMax, 1, 3, N, E, S, W), xmax(XMax)                ==> member(['N', 1], Neighbors) |
% %     N = 2, E = 2, S = 0, W = 0.
% % board(XMax, YMax, 3, N, E, S, W), xmax(XMax), ymax(YMax) ==> member(['N', 1], Neighbors) |
% %     N = 2, E = 0, S = 0, W = 2.
%
% % islands with 3 in a corner of the board have at least 1 connection in all remaining directions, so remove 0 from the domains
% board(1, 1, 3, _, E, S, _)                               \ E in D, S in D <=> D = [0, 1, 2] | ND = [1, 2], E in ND, S in ND.
% board(1, YMax, 3, _, _, S, W), ymax(YMax)                \ S in D, W in D <=> D = [0, 1, 2] | ND = [1, 2], S in ND, W in ND.
% board(XMax, 1, 3, N, E, _, _), xmax(XMax)                \ N in D, E in D <=> D = [0, 1, 2] | ND = [1, 2], N in ND, E in ND.
% board(XMax, YMax, 3, N, _, _, W), xmax(XMax), ymax(YMax) \ N in D, W in D <=> D = [0, 1, 2] | ND = [1, 2], N in ND, W in ND.
%
% % islands with 5 on a side of the board have at least 1 connection in all remaining directions, so remove 0 from the domains
% board(1, _, 5, _, E, S, W)                 \ E in D, S in D, W in D <=> D = [0, 1, 2] | ND = [1, 2], E in ND, S in ND, W in ND.
% board(_, YMax, 5, N, _, S, W), ymax(YMax)  \ N in D, S in D, W in D <=> D = [0, 1, 2] | ND = [1, 2], N in ND, S in ND, W in ND.
% board(XMax, _, 5, N, E, _, W), xmax(XMax)  \ N in D, E in D, W in D <=> D = [0, 1, 2] | ND = [1, 2], N in ND, E in ND, W in ND.
% board(_, 1, 5, N, E, S, _)                 \ N in D, E in D, S in D <=> D = [0, 1, 2] | ND = [1, 2], N in ND, E in ND, S in ND.
%
% % islands with 7 have at least 1 connection in all directions, so remove 0 from the domains
% board(_, _, 7, N, E, S, W) \ N in D, E in D, S in D, W in D  <=> D = [0, 1, 2] | ND = [1, 2], N in ND, E in ND, S in ND, W in ND.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RULES USED FOR DOMAIN SOLVING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% X and Y are instantiated and are different
add(X, 0, Z) <=> Z = X.
add(0, Y, Z) <=> Z = Y.

add(X, Y, Z) <=> number(Y), number(Z), var(X) | X is Z - Y.
add(X, Y, Z) <=> number(X), number(Z), var(Y) | Y is Z - X.
add(X, Y, Z) <=> number(X), number(Y) | Z is X + Y.

or_eq(X, Y, Z) <=> number(X), number(Y), number(Z), Z == 1 | X == Y.
or_eq(X, Y, Z) <=> number(X), number(Y), number(Z), Z == 0 | true.

eq(X, Y) <=> var(X), number(Y) | X = Y.
eq(Y, X) <=> var(X), number(Y) | X = Y.
eq(X, Y) <=> number(X), number(Y) | X == Y.

% X and Y are instantiated and are different
diff(X, Y) <=> number(X), number(Y) | X \== Y.
diff(Y, X) \ X in L <=> number(Y), select(Y, L, NL) | X in NL.
diff(X, Y) \ X in L <=> number(Y), select(Y, L, NL) | X in NL.

% enum(L): assigns values to variables X in L
enum(X)              <=> number(X) | true.
enum(X), X in Domain <=> member(X, Domain).

% eg when "0 in [0, 1, 2]", 0 should just be member of Domain
X in Domain <=> number(X) | member(X, Domain).

% search for constraint variables
enum_board, board(_, _, _, N, E, S, W) ==>
    enum(N), enum(E), enum(S), enum(W).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONNECTIVITY CONSTRAINT RULES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% when an island's bridge is found, add a new connected fact
enum_board, board(A, B, Am, N, _, _, _) ==> Am > 0, number(N), N > 0 | create_connection(A, B, 'N').
enum_board, board(A, B, Am, _, E, _, _) ==> Am > 0, number(E), E > 0 | create_connection(A, B, 'E').
enum_board, board(A, B, Am, _, _, S, _) ==> Am > 0, number(S), S > 0 | create_connection(A, B, 'S').
enum_board, board(A, B, Am, _, _, _, W) ==> Am > 0, number(W), W > 0 | create_connection(A, B, 'W').

% adds a new connected fact for an island going to direction Direction from position (A, B)
create_connection(A, B, Direction) <=>
    next_pos(A, B, Direction, C, D),
    create_connection(A, B, Direction, C, D).
board(X, Y, Amount, _, _, _, _) \ create_connection(OriginalX, OriginalY, _, X, Y) <=> Amount > 0 |
    connected([OriginalX, OriginalY], [X, Y]).
board(X, Y, Amount, _, _, _, _) \ create_connection(OriginalX, OriginalY, Direction, X, Y) <=> Amount == 0 |
    next_pos(X, Y, Direction, XN, YN),
    create_connection(OriginalX, OriginalY, Direction, XN, YN).

% remove duplicate connected facts
enum_board, connected(A, B) \ connected(A, B) <=> true.
enum_board, connected(A, B) \ connected(B, A) <=> true.

% put first island in reachable set
island(X, Y, _) \ pick_first_island <=> reachable(X, Y).

% build up reachable set
enum_board, reachable(X, Y) \ connected([X, Y], [A, B]) <=> reachable(A, B).
enum_board, reachable(X, Y) \ connected([A, B], [X, Y]) <=> reachable(A, B).

% remove duplicate reachable facts
enum_board, reachable(A, B) \ reachable(A, B) <=> true.

% connectivity constraint: each island fact needs to have an accompanying reachable fact
connected \ island(X, Y, _), reachable(X, Y)  <=> true.
connected, island(_, _, _)                    <=> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELPER RULES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% clear the chr store after solving the puzzle
clear_store \ board(_, _, _, _, _, _, _) <=> true.
clear_store \ xmax(_), ymax(_), domain_list(_) <=> true.
clear_store, bridge_constraints, enum_board, make_domains, connected <=> true.

% create board facts from a puzzle fact
%   Each puzzle(Id, S, Islands) fact defines the input of one problem:
%   its identifier Id, the size S (width and height), and the list of islands Islands.
puzzle_board(Number) <=> puzzle(Number, Size, Islands) |
    ymax(Size),
    xmax(Size),
    create_empty_board(1, 1),
    create_islands(Islands).

% create board facts for an empty board
xmax(Size) \ create_empty_board(_, Y) <=> Y > Size | true.

xmax(Size) \ create_empty_board(X, Y) <=> X > Size |
    Y2 is Y + 1,
    create_empty_board(1, Y2).

xmax(Size) \ create_empty_board(X, Y) <=> X =< Size |
    board(X, Y, 0, _, _, _, _),
    X2 is X + 1,
    create_empty_board(X2, Y).

% create board facts from an array of Islands
%   each island takes the form (X, Y, N) where X is the row number, Y is the column
%   number and N the number of bridges that should arrive in this island.
create_islands([ [X, Y, Amount] | Islands ]), board(X, Y, _, N, E, S, W) <=>
    board(X, Y, Amount, N, E, S, W),
    island(X, Y, Amount),
    create_islands(Islands).
create_islands([]) <=> true.

% load the board from a matrix fact
puzzle_board(Number) <=> board(Number, Matrix), length(Matrix, XMax), nth1(1, Matrix, Row), length(Row, YMax) |
    xmax(XMax),
    ymax(YMax),
    board_facts_from_matrix(Matrix, 1).

% create board facts from a matrix that contains the board
board_facts_from_matrix([], _).
board_facts_from_matrix([ Row | Rows ], X) <=>
    board_facts_from_row(Row, X, 1),
    XN is X + 1,
    board_facts_from_matrix(Rows, XN),
    !.

% create board facts from a row that contains a row of the board
board_facts_from_row([], _, _).
board_facts_from_row([ Number | Row ], X, Y) <=>
    board(X, Y, Number, _, _, _, _),
    ( Number > 0 ->
        island(X, Y, Number)
    ;
        true
    ),
    YN is Y + 1,
    board_facts_from_row(Row, X, YN).

% prints the board
print_board <=> print_board(1, 1).
board(X,Y, Val, NS, EW, _, _) \ print_board(X, Y) <=>
    (Val > 0 ->
        write(Val)
    ;
        ( (var(NS) ; var(EW)) ->
            write('_')
        ;
            ( number(NS), number(EW) ->
                symbol(NS, EW, Char),
                write(Char)
            ;
                write(' ')
            )
        )
    ),
    Y2 is Y + 1,
    print_board(X, Y2),
    !.
board(X, _, _, _, _, _, _) \ print_board(X, _) <=>
    X2 is X + 1,
    nl,
    print_board(X2, 1).
print_board(_, _) <=> nl.

symbol(0, 0, ' ').
symbol(0, 1, '-').
symbol(0, 2, '=').
symbol(1, 0, '|').
symbol(2, 0, '"').
symbol(_, _, "*").

next_pos(X, Y, 'N', X2, Y) :- X2 is X-1.
next_pos(X, Y, 'S', X2, Y) :- X2 is X+1.
next_pos(X, Y, 'E', X, Y2) :- Y2 is Y+1.
next_pos(X, Y, 'W', X, Y2) :- Y2 is Y-1.

% upto(N, L): L = [0..N]
upto([], -1).
upto([ N | L ], N) :-
    N >= 0,
    N1 is N-1,
    upto(L, N1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SAMPLE PROBLEMS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% slow boards: 1, 2, 6, 10, 11
% fast boards: 3, 4, 5, 8, 9

% puzzle 1, easy
% http://en.wikipedia.org/wiki/File:Val42-Bridge1n.png
% solution: http://en.wikipedia.org/wiki/File:Val42-Bridge1.png
puzzle(1, 7, [
    [1,1,2], [1,2,3], [1,4,4], [1,6,2],
    [2,7,2],
    [3,1,1], [3,2,1], [3,5,1], [3,6,3], [3,7,3],
    [4,1,2], [4,4,8], [4,6,5], [4,7,2],
    [5,1,3], [5,3,3], [5,7,1],
    [6,3,2], [6,6,3], [6,7,4],
    [7,1,3], [7,4,3], [7,5,1], [7,7,2]
]).

% puzzle 2, moderate
% http://en.wikipedia.org/wiki/File:Bridges-example.png
% solution: http://upload.wikimedia.org/wikipedia/en/1/10/Bridges-answer.PNG

puzzle(2, 13, [
    [1,1,2],  [1,3,4],  [1,5,3],   [1,7,1],   [1,9,2],   [1,12,1],
    [2,10,3], [2,13,1],
    [3,5,2],  [3,7,3],  [3,9,2],
    [4,1,2],  [4,3,3],  [4,6,2],   [4,10,3],  [4,12,1],
    [5,5,2],  [5,7,5],  [5,9,3],   [5,11,4],
    [6,1,1],  [6,3,5],  [6,6,2],   [6,8,1],   [6,12,2],
    [7,7,2],  [7,9,2],  [7,11,4],  [7,13,2],
    [8,3,4],  [8,5,4],  [8,8,3],   [8,12,3],
    [10,1,2], [10,3,2], [10,5,3],  [10,9,3],  [10,11,2], [10,13,3],
    [11,6,2], [11,8,4], [11,10,4], [11,12,3],
    [12,3,1], [12,5,2],
    [13,1,3], [13,6,3], [13,8,1],  [13,10,2], [13,13,2]
]).

% puzzle 3
% http://www.conceptispuzzles.com/index.aspx?uri=puzzle/hashi/techniques
puzzle(3, 6, [
    [1,1,1], [1,3,4], [1,5,2],
    [2,4,2], [2,6,3],
    [3,1,4], [3,3,7], [3,5,1],
    [4,4,2], [4,6,5],
    [5,3,3], [5,5,1],
    [6,1,3], [6,4,3], [6,6,3]
]).

% puzzle 4
% http://www.conceptispuzzles.com/index.aspx?uri=puzzle/euid/010000008973f050f28ceb4b11c74e73d34e1c47d885e0d8449ab61297e5da2ec85ea0804f0c5a024fbf51b5a0bd8f573565bc1b/play
puzzle(4, 8, [
    [1,1,2], [1,3,2], [1,5,5], [1,7,2],
    [2,6,1], [2,8,3],
    [3,1,6], [3,3,3],
    [4,2,2], [4,5,6], [4,7,1],
    [5,1,3], [5,3,1], [5,6,2], [5,8,6],
    [6,2,2],
    [7,1,1], [7,3,3], [7,5,5], [7,8,3],
    [8,2,2], [8,4,3], [8,7,2]
]).

% http://stackoverflow.com/questions/20337029/hashi-puzzle-representation-to-solve-all-solutions-with-prolog-restrictions/20364306#20364306
board(5, [
    [3, 0, 6, 0, 0, 0, 6, 0, 3],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 1, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [2, 0, 0, 0, 0, 1, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [1, 0, 3, 0, 0, 2, 0, 0, 0],
    [0, 3, 0, 0, 0, 0, 4, 0, 1]
]).

% same as puzzle 2
% https://en.wikipedia.org/wiki/Hashiwokakero#/media/File:Bridges-example.png
board(6, [
    [2, 0, 4, 0, 3, 0, 1, 0, 2, 0, 0, 1, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 1],
    [0, 0, 0, 0, 2, 0, 3, 0, 2, 0, 0, 0, 0],
    [2, 0, 3, 0, 0, 2, 0, 0, 0, 3, 0, 1, 0],
    [0, 0, 0, 0, 2, 0, 5, 0, 3, 0, 4, 0, 0],
    [1, 0, 5, 0, 0, 2, 0, 1, 0, 0, 0, 2, 0],
    [0, 0, 0, 0, 0, 0, 2, 0, 2, 0, 4, 0, 2],
    [0, 0, 4, 0, 4, 0, 0, 3, 0, 0, 0, 3, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [2, 0, 2, 0, 3, 0, 0, 0, 3, 0, 2, 0, 3],
    [0, 0, 0, 0, 0, 2, 0, 4, 0, 4, 0, 3, 0],
    [0, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0],
    [3, 0, 0, 0, 0, 3, 0, 1, 0, 2, 0, 0, 2]
]).

% board that cannot be solved
board(7, [
    [1, 0, 1, 0, 2],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 2]
]).


board(8, [
    [1, 0, 2, 0, 3],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 2]
]).

board(9, [
    [2, 0, 0, 0, 2],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [2, 0, 0, 0, 2]
]).

board(10, [
    [2, 0, 3, 0, 0, 0, 4, 0, 0, 0, 2, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 3, 0, 3],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [2, 0, 0, 0, 0, 0, 8, 0, 0, 0, 5, 0, 2],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [3, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 1],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 3, 0, 4],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [3, 0, 0, 0, 0, 0, 3, 0, 1, 0, 0, 0, 2]
]).

board(11, [
    [0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 3, 0, 2, 0],
    [4, 0, 0, 0, 4, 0, 0, 3, 0, 0, 0, 4, 0, 4, 0, 0, 2, 0, 0, 0, 1],
    [0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 4, 0],
    [4, 0, 0, 0, 0, 0, 0, 0, 2, 0, 3, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0],
    [0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0],
    [0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 3, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0],
    [0, 4, 0, 0, 0, 0, 0, 0, 2, 0, 0, 3, 0, 0, 0, 0, 0, 4, 0, 1, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 3, 0, 1, 0, 0, 0, 0, 0],
    [0, 0, 3, 0, 6, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 2, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 4, 0, 6, 0, 0, 0, 0, 0, 0, 5],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 2, 0, 4, 0, 1, 0, 2, 0, 0, 3, 0, 4, 0, 0, 0, 0, 2, 0, 0],
    [0, 5, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 4, 0, 0, 0, 0, 0, 0, 1, 0],
    [3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 3, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 5],
    [0, 0, 0, 1, 0, 0, 0, 4, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 3, 0, 0, 0, 4, 0, 0, 0, 2, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 2],
    [1, 0, 2, 0, 3, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 4, 0, 2, 0]
]).

board(12, [
    [0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0]
]).

board(13, [
    [4, 0, 3, 0, 4, 0, 3, 0, 4],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [2, 0, 0, 0, 0, 0, 0, 0, 2],
    [0, 0, 2, 0, 8, 0, 0, 2, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 4, 0, 5, 0, 0, 1, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [2, 0, 6, 0, 2, 0, 0, 0, 0]
]).
