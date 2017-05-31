:- use_module(library(chr)).
:- consult("boards").

:- chr_constraint solve/1, puzzle_board/1, bridge_constraints/0.
:- chr_constraint enum/1, search/0.
:- chr_constraint make_domains/0.
:- chr_constraint create_islands/1, create_empty_board/2.
:- chr_constraint board/7, island/3,  xmax/1, ymax/1, print_board/0, print_board/2.
:- chr_constraint board_facts_from_row/3, board_facts_from_matrix/2.
:- chr_constraint clear_store/0, connected/0, connected/2.
:- chr_constraint create_connection/3, create_connection/5, pick_first_island/0, reachable/2.
:- chr_constraint le/2, eq/2, in/2, add/3, or/2, check/0.

:- op(700, xfx, in).
:- op(700, xfx, le).
:- op(700, xfx, eq).
:- op(600, xfx, '..').

:- chr_option(debug, off).
:- chr_option(optimize, full).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HASHIWOKAKERO SOLUTION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% solve a given game board
solve(Number) <=>
    % find the game board and load the board facts into the constraint store
    puzzle_board(Number),
    writeln("Given board:"),
    print_board,

    % create the bridge constraint rules
    bridge_constraints,

    % after doing all bridge constraints, make domains for remaining variables (N E S W)
    make_domains,

    writeln("Board before search:"),
    print_board,
    % chr_show_store(user),

    % after generating all necessary domains, start the search
    check,
    search,

    % all islands need to be in the reachable set
    pick_first_island,
    connected,

    writeln("Board after search:"),
    print_board,
    clear_store,
    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BRIDGE CONSTRAINT RULES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% amount of bridges equals island's amount
bridge_constraints, board(_, _, Amount, N, E, S, W) ==> Amount > 0 |
    add(N, E, Sum),
    Sum in 0..4,
    add(S, W, Sum2),
    Sum2 in 0..4,
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

% extra indomain constraints are generated for literals, this is done so that domain
% solving with add(A, B, C) becomes easier. e.g. add(A, 1, 2) => domain of A can be constrained to 1..1
make_domains ==>
    0 in 0..0, 1 in 1..1, 2 in 2..2, 3 in 3..3, 4 in 4..4,
    5 in 5..5, 6 in 6..6, 7 in 7..7, 8 in 8..8,
    0 in 0..0, 1 in 1..1, 2 in 2..2, 3 in 3..3, 4 in 4..4,
    5 in 5..5, 6 in 6..6, 7 in 7..7, 8 in 8..8.

make_domains, board(_, _, _, N, _, _, _) ==> var(N) | N in 0..2.
make_domains, board(_, _, _, _, E, _, _) ==> var(E) | E in 0..2.
make_domains, board(_, _, _, _, _, S, _) ==> var(S) | S in 0..2.
make_domains, board(_, _, _, _, _, _, W) ==> var(W) | W in 0..2.

% the domain of an island with 1 is [0, 1] and not [0, 1, 2]
make_domains, board(_, _, 1, N, _, _, _) \ N in 0..2 <=>  N in 0..1.
make_domains, board(_, _, 1, _, E, _, _) \ E in 0..2 <=>  E in 0..1.
make_domains, board(_, _, 1, _, _, S, _) \ S in 0..2 <=>  S in 0..1.
make_domains, board(_, _, 1, _, _, _, W) \ W in 0..2 <=>  W in 0..1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSTRAINT IMPROVEMENT RULES
% inspired by http://www.conceptispuzzles.com/index.aspx?uri=puzzle/hashi/techniques
% improvements used:
%     "1. Islands with 4 in the corner, 6 on the side and 8 in the middle:"
%     "2. Islands with 3 in the corner, 5 on the side and 7 in the middle:"
%     "3. Special cases of 3 in the corner, 5 on the side and 7 in the middle:"
%
% "1. Islands with a single neighbor:" is already handled by the bridge_constraints
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

% % Neighbors = [ [Direction, Amount] ]
% board(X, Y, 7, N, E, S, W), neighbors(X, Y, Neighbors) ==> member(['N', 1], Neighbors) |
%     N = 1, E = 2, S = 2, W = 2.
% board(X, Y, 7, N, E, S, W), neighbors(X, Y, Neighbors) ==> member(['E', 1], Neighbors) |
%     N = 2, E = 1, S = 2, W = 2.
% board(X, Y, 7, N, E, S, W), neighbors(X, Y, Neighbors) ==> member(['S', 1], Neighbors) |
%     N = 2, E = 2, S = 1, W = 2.
% board(X, Y, 7, N, E, S, W), neighbors(X, Y, Neighbors) ==> member(['W', 1], Neighbors) |
%     N = 2, E = 2, S = 2, W = 1.
%
% board(1, 1, 3, N, E, S, W)                               ==> member(['N', 1], Neighbors) |
%     N = 0, E = 2, S = 2, W = 0.
% board(1, YMax, 3, N, E, S, W), ymax(YMax)                ==> member(['N', 1], Neighbors) |
%     N = 0, E = 0, S = 2, W = 2.
% board(XMax, 1, 3, N, E, S, W), xmax(XMax)                ==> member(['N', 1], Neighbors) |
%     N = 2, E = 2, S = 0, W = 0.
% board(XMax, YMax, 3, N, E, S, W), xmax(XMax), ymax(YMax) ==> member(['N', 1], Neighbors) |
%     N = 2, E = 0, S = 0, W = 2.

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

% remove duplicate indomain constraints
X in A..B \ X in A..B <=> var(X) | true.

% when there are two indomain constraints for a variable, fail
check, X in A..B, X in C..D ==> var(X) | chr_show_store(user), writeln(["X in 2 domains", X, [A, B], [C, D]]), false.

% when a variable's domain gets reduced to 1 number, set the value of the variable
X in A..A <=> var(X) | X = A.

% remove duplicate equality constraints
eq(X, Y) \ eq(Y, X) <=> true.
% equality constraint
eq(X, Y) <=> number(X), number(Y) | X == Y.
% equality domain constraint solving
eq(X, Y) \ X in A..B, Y in C..D <=> A \== C | L is max(A, C), X in L..B, Y in L..D.
eq(X, Y) \ X in A..B, Y in C..D <=> B \== D | U is min(B, D), X in A..U, Y in C..U.

% addition constraint
add(X, Y, Z) <=> number(X), number(Y), number(Z) | Z is X + Y.
% addition domain constraint solving
add(X, Y, Z) \ X in A..B, Y in C..D, Z in E..F <=>
    not( ( A >= E-D, B =< F-C, C >= E-B, D =< F-A, E >= A+C, F =< B+D ) ) |
        NewA is max(A, E-D), NewB is min(B, F-C), X in NewA..NewB,
        NewC is max(C, E-B), NewD is min(D, F-A), Y in NewC..NewD,
        NewE is max(E, A+C), NewF is min(F, B+D), Z in NewE..NewF.

% assign values to variables X. They get values in the domain A..B
enum(X)            <=> number(X) | true.
enum(X), X in A..B <=> numlist(A, B, Domain), member(X, Domain).

% eg when "0 in 0..2", 0 should just be member of Domain, TODO remove this? is it necessary?
% search \ X in A..B <=> number(X) | numlist(A, B, Domain), member(X, Domain).

% search for constraint variables
search, X in _.._ ==> var(X) |
    enum(X).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONNECTIVITY CONSTRAINT RULES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% when an island's bridge is found, add a new connected fact
board(A, B, Am, N, _, _, _) ==> Am > 0, number(N), N > 0 | create_connection(A, B, 'N').
board(A, B, Am, _, E, _, _) ==> Am > 0, number(E), E > 0 | create_connection(A, B, 'E').
board(A, B, Am, _, _, S, _) ==> Am > 0, number(S), S > 0 | create_connection(A, B, 'S').
board(A, B, Am, _, _, _, W) ==> Am > 0, number(W), W > 0 | create_connection(A, B, 'W').

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
connected(A, B) \ connected(A, B) <=> true.
connected(A, B) \ connected(B, A) <=> true.

% put first island in reachable set
island(X, Y, _) \ pick_first_island <=> reachable(X, Y).

% build up reachable set
reachable(X, Y) \ connected([X, Y], [A, B]) <=> reachable(A, B).
reachable(X, Y) \ connected([A, B], [X, Y]) <=> reachable(A, B).

% remove duplicate reachable facts
reachable(A, B) \ reachable(A, B) <=> true.

% connectivity constraint: each island fact needs to have an accompanying reachable fact
connected \ island(X, Y, _), reachable(X, Y)  <=> true.
connected, island(_, _, _)                    <=> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELPER RULES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% clear the chr store after solving the puzzle
clear_store \ board(_, _, _, _, _, _, _) <=> true.
clear_store \ xmax(_), ymax(_) <=> true.
clear_store \ bridge_constraints <=> true.
clear_store \ search <=> true.
clear_store \ make_domains <=> true.
clear_store \ connected <=> true.
clear_store \ 0 in 0..0, 1 in 1..1, 2 in 2..2 <=> true.
clear_store \ 3 in 3..3, 4 in 4..4 <=> true.
clear_store \ 5 in 5..5, 6 in 6..6 <=> true.
clear_store \ 7 in 7..7, 8 in 8..8 <=> true.
clear_store <=> true.

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
