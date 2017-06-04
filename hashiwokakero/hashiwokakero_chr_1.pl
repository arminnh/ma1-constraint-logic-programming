%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% A CHR SOLVER FOR HASHIWOKAKERO PUZZLES
%   the difference between 'hashiwokakero_chr_1' and 'hashiwokakero_chr_2' is
%   the way the connectivity constraint is handled.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(chr)).
:- consult("boards").

:- chr_constraint solve/1, time/1, timeall/0.
:- chr_constraint load_board/1, create_empty_board/2, create_islands/1, board_facts_from_row/3, board_facts_from_matrix/2.
:- chr_constraint xmax/1, ymax/1, board/7, island/3, neighbors/5, print_board/0, print_board/2, write_variable/2.
:- chr_constraint bridge_constraints/0, make_domains/0, clear_store/0.
:- chr_constraint search/0, enum/1, eq/2, in/2, add/3.
:- chr_constraint connected/0, connected/2, pick_first_island/0, reachable/2.


:- op(700, xfx, in).
:- op(700, xfx, eq).
:- op(600, xfx, '..').

:- chr_option(debug, off).
:- chr_option(optimize, full).

% solve a given game board
solve(Number) <=>
    % find the game board and load the board facts into the constraint store
    load_board(Number),
    writeln("Given board:"), print_board,
    % create the bridge constraint rules
    bridge_constraints,
    % after doing all bridge constraints, make domains for remaining variables (N E S W)
    make_domains,
    writeln("Board before search:"), print_board,
    % after generating all necessary domains, start the search
    search,
    % all islands need to be in the reachable set
    connected,
    writeln("Board after search:"), print_board,
    clear_store,
    true.

% solve a given game boad and print the time it took to solve the board.
time(Number) <=>
    statistics(walltime, [_ | [_]]),
    load_board(Number), bridge_constraints, make_domains, search, connected, clear_store,
    statistics(walltime, [_ | [ExecutionTimeMS]]),
    ExTimeS is ExecutionTimeMS / 1000,
    write(Number), write(': '), write(ExTimeS), write('s'), nl.

timeall <=>
    time(1), time(3), time(4), time(5), time(6), time(8), time(9), time(10),
    time(11), time(12), time(13), time(14), time(15), time(16), time(17).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BRIDGE CONSTRAINT RULES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% amount of bridges equals island's amount
bridge_constraints, board(_, _, Amount, N, E, S, W) ==> Amount > 0 |
    Sum in 0..4,
    Sum2 in 0..4,
    add(N, E, Sum),
    add(S, W, Sum2),
    add(Sum, Sum2, Amount).

% bridges going one way == bridges going the opposite way
bridge_constraints, board(_, _, 0, N, E, S, W) ==> N = S, E = W.

% bridges dont cross
board(_, _, 0, N, E, _, _) ==> number(N), N > 0 | E = 0.
board(_, _, 0, N, E, _, _) ==> number(E), E > 0 | N = 0.

% bridges going one way == bridges going the other way
bridge_constraints, board(X, Y, _, N, _, _, _), board(X2, Y, _, _, _, S, _) ==> X > 1, X2 is X-1  | N eq S.
bridge_constraints, board(X, Y, _, _, E, _, _), board(X, Y2, _, _, _, _, W) ==> Y2 is Y+1         | E eq W.

% bridges cannot go outside of the board
bridge_constraints, board(1, _, _, N, _, _, _)                ==> N = 0.
bridge_constraints, board(_, YMax, _, _, E, _, _), ymax(YMax) ==> E = 0.
bridge_constraints, board(XMax, _, _, _, _, S, _), xmax(XMax) ==> S = 0.
bridge_constraints, board(_, 1, _, _, _, _, W)                ==> W = 0.

bridge_constraints <=> true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DOMAIN GENERATION RULES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% extra indomain constraints are generated for literals, this is done so that domain
% solving with add(A, B, C) becomes easier. e.g. add(A, 1, 2) => domain of A can be constrained to 1..1
make_domains ==>
    0 in 0..0, 1 in 1..1, 2 in 2..2, 3 in 3..3, 4 in 4..4, 5 in 5..5, 6 in 6..6, 7 in 7..7, 8 in 8..8,
    0 in 0..0, 1 in 1..1, 2 in 2..2, 3 in 3..3, 4 in 4..4, 5 in 5..5, 6 in 6..6, 7 in 7..7, 8 in 8..8.

make_domains, board(_, _, _, N, _, _, _) ==> var(N) | N in 0..2.
make_domains, board(_, _, _, _, E, _, _) ==> var(E) | E in 0..2.
make_domains, board(_, _, _, _, _, S, _) ==> var(S) | S in 0..2.
make_domains, board(_, _, _, _, _, _, W) ==> var(W) | W in 0..2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSTRAINT IMPROVEMENT RULES
% inspired by http://www.conceptispuzzles.com/index.aspx?uri=puzzle/hashi/techniques
%   "Isolation of a two-island segment"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% improvement A: island with 1 cannot be connected to other island with 1, so set variable to 0
make_domains, board(X1, Y1, 1, N, _, _, _), neighbors(X1, Y1, 'N', X2, Y2), island(X2, Y2, 1) \ N in _.._ <=> var(N) | N = 0.
make_domains, board(X1, Y1, 1, _, E, _, _), neighbors(X1, Y1, 'E', X2, Y2), island(X2, Y2, 1) \ E in _.._ <=> var(E) | E = 0.

% improvement B: 2 cannot be connected to 2 by 2 bridges, so make domain A..1
make_domains, board(X1, Y1, 2, N, _, _, _), neighbors(X1, Y1, 'N', X2, Y2), island(X2, Y2, 2) \ N in A..2 <=> var(N) | N in A..1.
make_domains, board(X1, Y1, 2, _, E, _, _), neighbors(X1, Y1, 'E', X2, Y2), island(X2, Y2, 2) \ E in A..2 <=> var(E) | E in A..1.

make_domains <=> true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RULES USED FOR DOMAIN SOLVING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% remove duplicate indomain constraints
X in A..B \ X in A..B <=> var(X) | true.

% when a variable's domain gets reduced to 1 number, set the value of the variable
X in A..A <=> var(X) | X = A.

% equality constraint
X eq Y <=> number(X), number(Y) | X == Y.
% equality domain constraint solving
X eq Y \ X in A..B, Y in C..D <=> A \== C | L is max(A, C), X in L..B, Y in L..D.
X eq Y \ X in A..B, Y in C..D <=> B \== D | U is min(B, D), X in A..U, Y in C..U.

% addition constraint
add(X, Y, Z) <=> number(X), number(Y), number(Z) | Z is X + Y.
% addition domain constraint solving
add(X, Y, Z) \ X in A..B, Y in C..D, Z in E..F <=>
    not( ( A >= E-D, B =< F-C, C >= E-B, D =< F-A, E >= A+C, F =< B+D ) ) |
        NewA is max(A, E-D), NewB is min(B, F-C), X in NewA..NewB,
        NewC is max(C, E-B), NewD is min(D, F-A), Y in NewC..NewD,
        NewE is max(E, A+C), NewF is min(F, B+D), Z in NewE..NewF.

% assign values to variables X. X must lie between A and B
enum(X)            <=> number(X) | true.
enum(X), X in A..B <=> between(A, B, X).

% search for constraint variables
search, X in _.._ ==> var(X) | enum(X).
search <=> true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RULES USED TO DETECT CONNECTIONS BETWEEN ISLANDS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% when an island's bridge is found, add a new connected fact
board(A, B, Am, N, _, _, _), neighbors(A, B, 'N', C, D) ==> Am > 0, number(N), N > 0 | connected([A, B], [C, D]).
board(A, B, Am, _, E, _, _), neighbors(A, B, 'E', C, D) ==> Am > 0, number(E), E > 0 | connected([A, B], [C, D]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONNECTIVITY CONSTRAINT PROPAGATION METHOD 1
%   build up a 'reachable' set starting from a certain island
%   when a connection is made between an island and the set, add the island to the set
%   if at the end of the search, not all islands are in the reachable set,
%   then the solution is not valid
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% put first island in reachable set
connected ==> pick_first_island.
island(X, Y, _) \ pick_first_island <=> reachable(X, Y).

% build up reachable set
reachable(X, Y) \ connected([X, Y], [A, B]) <=> reachable(A, B).
reachable(X, Y) \ connected([A, B], [X, Y]) <=> reachable(A, B).
reachable(X, Y) \ reachable(X, Y) <=> true.

% connectivity constraint: each island fact needs to have an accompanying reachable fact
connected \ island(X, Y, _), reachable(X, Y)  <=> true.
connected, island(_, _, _)                    <=> false.
connected <=> true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELPER RULES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% create board facts from a puzzle fact
%   Each puzzle(Id, S, Islands) fact defines the input of one problem:
%   its identifier Id, the size S (width and height), and the list of islands Islands.
load_board(Number) <=> puzzle(Number, Size, Islands) |
    ymax(Size),
    xmax(Size),
    create_empty_board(1, 1),
    create_islands(Islands).

% create board facts for an empty board
xmax(XMax) \ create_empty_board(_, Y) <=> Y > XMax               | true.
xmax(XMax) \ create_empty_board(X, Y) <=> X > XMax,  Y2 is Y + 1 | create_empty_board(1, Y2).
xmax(XMax) \ create_empty_board(X, Y) <=> X =< XMax, X2 is X + 1 |
    board(X, Y, 0, _, _, _, _),
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
board_facts_from_matrix([ Row | Rows ], X) <=> XN is X + 1 |
    board_facts_from_row(Row, X, 1),
    board_facts_from_matrix(Rows, XN).
board_facts_from_matrix([], _) <=> true.

% create board facts from a row that contains a row of the board
board_facts_from_row([ 0 | Row ], X, Y) <=> YN is Y + 1 |
    board(X, Y, 0, _, _, _, _),
    board_facts_from_row(Row, X, YN).
board_facts_from_row([ Number | Row ], X, Y) <=> YN is Y + 1 |
    board(X, Y, Number, _, _, _, _),
    island(X, Y, Number),
    board_facts_from_row(Row, X, YN).
board_facts_from_row([], _, _) <=> true.

% create neighbor facts. two islands are neighbors when they can be connected by a bridge
island(X1, Y, _), island(X2, Y, _) ==> X1 > X2 | neighbors(X1, Y, 'N', X2, Y).
island(X, Y1, _), island(X, Y2, _) ==> Y1 < Y2 | neighbors(X, Y1, 'E', X, Y2).

% remove neighbor facts when there is another island in between to islands
neighbors(X1, Y, 'N', X, Y) \ neighbors(X2, Y, 'N', X, Y) <=> X1 < X2 | true.
neighbors(X, Y1, 'E', X, Y) \ neighbors(X, Y2, 'E', X, Y) <=> Y1 > Y2 | true.

% clear the chr store after solving the puzzle
clear_store \ xmax(_), ymax(_) <=> true.
clear_store \ board(_, _, _, _, _, _, _) <=> true.
clear_store \ island(_, _, _) <=> true.
clear_store \ neighbors(_, _, _, _, _) <=> true.
clear_store \ X in _.._ <=> number(X) | true.
clear_store <=> true.

% prints the board
print_board <=> print_board(1, 1).
board(X,Y, Val, N, E, _, _) \ print_board(X, Y) <=> Y2 is Y + 1 |
    (Val > 0 ->
        write(Val)
    ;
        ( (var(N) ; var(E)) ->
            write_variable(N, E)
        ;
            symbol(N, E, Char),
            write(Char)
        )
    ),
    print_board(X, Y2),
    !.
board(X, _, _, _, _, _, _) \ print_board(X, _) <=> X2 is X + 1, nl | print_board(X2, 1).
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
