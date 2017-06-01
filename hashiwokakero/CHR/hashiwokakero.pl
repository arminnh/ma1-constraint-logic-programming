:- use_module(library(chr)).
:- consult("boards").

:- chr_constraint time/1, timeall/0, solve/1, load_board/1, bridge_constraints/0.
:- chr_constraint enum/1, search/0, write_variable/2.
:- chr_constraint make_domains/0.
:- chr_constraint create_islands/1, create_empty_board/2.
:- chr_constraint board/7, neighbors/5, island/3,  xmax/1, ymax/1, print_board/0, print_board/2.
:- chr_constraint board_facts_from_row/3, board_facts_from_matrix/2.
:- chr_constraint clear_store/0, connected/0, connected/2.
:- chr_constraint create_connection/3, create_connection/5, pick_first_island/0, reachable/2.
:- chr_constraint le/2, eq/2, in/2, add/3, or/2, check/0, create_neighbors/0.

:- op(700, xfx, in).
:- op(700, xfx, le).
:- op(700, xfx, eq).
:- op(600, xfx, '..').

:- chr_option(debug, off).
:- chr_option(optimize, full).

% solve a given game board
solve(Number) <=>
    % find the game board and load the board facts into the constraint store
    load_board(Number),
    create_neighbors,
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
    search,

    % all islands need to be in the reachable set
    pick_first_island,
    connected,

    writeln("Board after search:"),
    print_board,
    clear_store,
    true.

timeall <=>
    time(1), time(3), time(4), time(5), time(6), time(8), time(9), time(10),
    time(11), time(12), time(13), time(14), time(15), time(16), time(17), time(18).

time(Number) <=>
    % statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
    statistics(walltime, [_ | [_]]),

    load_board(Number), create_neighbors, bridge_constraints, make_domains, search, pick_first_island, connected, clear_store,

    % statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),
    statistics(walltime, [_ | [ExecutionTimeMS]]),
    ExTimeS is ExecutionTimeMS / 1000,
    write(Number), write(': '), write(ExTimeS), write('s'), nl,
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

bridge_constraints <=> true.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSTRAINT IMPROVEMENT RULES
% inspired by http://www.conceptispuzzles.com/index.aspx?uri=puzzle/hashi/techniques
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% improvement A: island with 1 can not be connected to other island with 1, so make domain 0..0 (just set variable to 0)
make_domains, board(X1, Y1, 1, N, _, _, _), neighbors(X1, Y1, 'N', X2, Y2), island(X2, Y2, 1) \ N in _.._ <=> var(N) | N = 0.
make_domains, board(X1, Y1, 1, _, E, _, _), neighbors(X1, Y1, 'E', X2, Y2), island(X2, Y2, 1) \ E in _.._ <=> var(E) | E = 0.
make_domains, board(X1, Y1, 1, _, _, S, _), neighbors(X1, Y1, 'S', X2, Y2), island(X2, Y2, 1) \ S in _.._ <=> var(S) | S = 0.
make_domains, board(X1, Y1, 1, _, _, _, W), neighbors(X1, Y1, 'W', X2, Y2), island(X2, Y2, 1) \ W in _.._ <=> var(W) | W = 0.

% improvement B: 2 can not be connected to 2 by 2 bridges, so make domain A..1
make_domains, board(X1, Y1, 2, N, _, _, _), neighbors(X1, Y1, 'N', X2, Y2), island(X2, Y2, 2) \ N in A..2 <=> var(N) | N in A..1.
make_domains, board(X1, Y1, 2, _, E, _, _), neighbors(X1, Y1, 'E', X2, Y2), island(X2, Y2, 2) \ E in A..2 <=> var(E) | E in A..1.
make_domains, board(X1, Y1, 2, _, _, S, _), neighbors(X1, Y1, 'S', X2, Y2), island(X2, Y2, 2) \ S in A..2 <=> var(S) | S in A..1.
make_domains, board(X1, Y1, 2, _, _, _, W), neighbors(X1, Y1, 'W', X2, Y2), island(X2, Y2, 2) \ W in A..2 <=> var(W) | W in A..1.

% improvement C: Isolation of a three-island segment

make_domains <=> true.

% IMPACT OF THESE IMPROVEMENTS ON PREVIOUSLY SLOW BOARDS:
%   board number | time without improvements | time with improvement A | time with improvement B | time with improvements A and B
%   1   | 0.078s  | 0.071s | 0.077s | 0.078s
%   3   | 0.041s  | 0.045s | 0.044s | 0.042s
%   4   | 0.093s  | 0.111s | 0.102s | 0.099s
%   5   | 0.111s  | 0.112s | 0.111s | 0.119s
%   6   | 0.272s  | 0.27s  | 0.288s | 0.258s
%   8   | 0.015s  | 0.016s | 0.009s | 0.009s
%   9   | 0.015s  | 0.024s | 0.014s | 0.012s
%   10  | 0.403s  | 0.4s   | 0.446s | 0.395s
%   11  | 35.927s | 2.967s | 93.877s | 3.017s
%   12  | 0.148s  | 0.128s | 0.151s | 0.131s
%   13  | 0.12s   | 0.137s | 0.143s | 0.127s
%   14  | 0.243s  | 0.198s | 0.223s | 0.18s
%   15  | 0.19s   | 0.223s | 0.21s | 0.216s
%   16  | 3.938s  | 3.594s | 4.046s | 3.706s
%   17  | 4.546s  | 4.054s | 4.611s | 4.002s
%   18  | 54.622s | 1.054s | 53.611s | 1.023s
%   2   | aborted after 6405s | still takes way too long, wtf you doin matey

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RULES USED FOR DOMAIN SOLVING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% remove duplicate indomain constraints
X in A..B \ X in A..B <=> var(X) | true.

% when a variable's domain gets reduced to 1 number, set the value of the variable
X in A..A <=> var(X) | X = A.
% X in A..A <=> var(X) | print_board, X = A.

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
% % choose values for numbers in smaller intervals before those in larger intervals
% enum(X), X in 0..2 <=> member(X, [0, 2]).
% enum(X), X in 1..2 <=> member(X, [1, 2]).
% enum(X), X in 0..1 <=> member(X, [0, 1]).

% eg when "0 in 0..2", 0 should just be member of Domain, TODO remove this? is it necessary?
% search \ X in A..B <=> number(X) | numlist(A, B, Domain), member(X, Domain).

% search for constraint variables
search, X in _.._ ==> var(X) |
    enum(X).
search <=> true.

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
connected <=> true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELPER RULES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% clear the chr store after solving the puzzle
clear_store \ board(_, _, _, _, _, _, _) <=> true.
clear_store \ neighbors(_, _, _, _, _) <=> true.
clear_store \ xmax(_), ymax(_) <=> true.
clear_store \ 0 in 0..0, 1 in 1..1, 2 in 2..2 <=> true.
clear_store \ 3 in 3..3, 4 in 4..4 <=> true.
clear_store \ 5 in 5..5, 6 in 6..6 <=> true.
clear_store \ 7 in 7..7, 8 in 8..8 <=> true.
clear_store <=> true.

% create board facts from a puzzle fact
%   Each puzzle(Id, S, Islands) fact defines the input of one problem:
%   its identifier Id, the size S (width and height), and the list of islands Islands.
load_board(Number) <=> puzzle(Number, Size, Islands) |
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
load_board(Number) <=> board(Number, Matrix), length(Matrix, XMax), nth1(1, Matrix, Row), length(Row, YMax) |
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

% create neighbor facts. two islands are neighbors when they can be connected by a bridge
create_neighbors, island(X1, Y, _), island(X2, Y, _) ==> X1 > X2 | neighbors(X1, Y, 'N', X2, Y).
create_neighbors, island(X, Y1, _), island(X, Y2, _) ==> Y1 < Y2 | neighbors(X, Y1, 'E', X, Y2).
create_neighbors, island(X1, Y, _), island(X2, Y, _) ==> X1 < X2 | neighbors(X1, Y, 'S', X2, Y).
create_neighbors, island(X, Y1, _), island(X, Y2, _) ==> Y1 > Y2 | neighbors(X, Y1, 'W', X, Y2).

% remove neighbor facts when there is another island in between to islands
create_neighbors, neighbors(X1, Y, 'N', X, Y) \ neighbors(X2, Y, 'N', X, Y) <=> X1 < X2 | true.
create_neighbors, neighbors(X, Y1, 'E', X, Y) \ neighbors(X, Y2, 'E', X, Y) <=> Y1 > Y2 | true.
create_neighbors, neighbors(X1, Y, 'S', X, Y) \ neighbors(X2, Y, 'S', X, Y) <=> X1 > X2 | true.
create_neighbors, neighbors(X, Y1, 'W', X, Y) \ neighbors(X, Y2, 'W', X, Y) <=> Y1 < Y2 | true.

create_neighbors <=> true.

% prints the board
print_board <=> print_board(1, 1).
board(X,Y, Val, N, E, _, _) \ print_board(X, Y) <=>
    (Val > 0 ->
        write(Val)
    ;
        ( (var(N) ; var(E)) ->
            write_variable(N, E)
        ;
            ( number(N), number(E) ->
                symbol(N, E, Char),
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

N in 0..1 \ write_variable(N, _) <=> write('.').
N in 0..2 \ write_variable(N, _) <=> write(',').
N in 1..2 \ write_variable(N, _) <=> write(';').
E in 0..1 \ write_variable(_, E) <=> write('~').
E in 0..2 \ write_variable(_, E) <=> write('/').
E in 1..2 \ write_variable(_, E) <=> write('\\').
write_variable(_, _)             <=> write('_').

% symbol(N, E, char): char is the character to print depending on the values of N and E
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
