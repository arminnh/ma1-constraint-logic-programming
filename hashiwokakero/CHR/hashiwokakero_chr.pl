:- use_module(library(chr)).

:- chr_constraint solve/1, puzzle_board/1, print_board/0, bridge_constraints/0.
:- chr_constraint print_row/1, print_pos/1, enum/1, enum_board/0.
:- chr_constraint make_domain/2, make_domains/0, domain_list/1.
:- chr_constraint islands_board/1, matrix_board/2, create_islands/1, create_empty_board/3.
:- chr_constraint board/7, create_board/3, output/1, xmax/1, ymax/1, print_board/2,
                  board_facts_from_row/3, board_facts_from_matrix/2,
                  diff/2, clear_store/0, connected/0, connected/2, find_neighbor/3, find_neighbor/5.

:- op(700, xfx, in).
:- op(700, xfx, le).
:- op(700, xfx, eq).
:- op(700, xfx, or_eq).
:- op(600, xfx, '..').
:- chr_constraint le/2, eq/2, in/2, add/3, or_eq/3, or/2.
:- chr_option(debug,off).
:- chr_option(optimize,full).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HASHIWOKAKERO SOLUTION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% solve a given game board
solve(Number) <=>
    % find the game board and load the board facts into the constraint store
    puzzle_board(Number),
    writeln("Given board:"),
    print_board(1,1),
    nl,

    upto(DomainList, 2),
    reverse(DomainList, List),
    domain_list(List),

    % create the bridge constraint rules
    bridge_constraints,

    print_board(1,1),
    clear_store,
    nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BRIDGE CONSTRAINT RULES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% amount of bridges equals island's amount
bridge_constraints, board(_,_, Amount, N, E, S, W) ==> Amount > 0 |
    add(N, E, Sum),
    add(S, W, Sum2),
    add(Sum, Sum2, Amount),
    true.

% bridges going one way == bridges going the opposite way
bridge_constraints, board(_, _, 0, N, E, S, W) ==>
    N = S,
    E = W.

% bridges dont cross
board(_, _, 0, N, E, _, _) ==> number(N), N > 0 | E = 0.
board(_, _, 0, N, E, _, _) ==> number(E), E > 0 | N = 0.

% bridges going north == bridges going south in position above
bridge_constraints, board(X, Y, _, N, _, _, _), board(X2, Y, _, _, _, S2, _)  ==> X > 1, X2 is X-1 |
    eq(N,S2).

% bridge can not go north at top of board
bridge_constraints, board(X,_, _, N, _, _, _)                          ==> X == 1 |
    N = 0.

% bridges going east == bridges going west in position to the right
bridge_constraints, board(X,Y, _, _, E, _, _), board(X,Y2,_,_,_,_,W2)  ==> Y2 is Y+1 |
    eq(E, W2).

% bridge cannot go east at right of board
bridge_constraints, board(_,Y, _, _, E, _, _), ymax(Size)              ==> Y == Size |
    E = 0.

% bridges going south == bridges going north in position under
bridge_constraints, board(X,Y, _, _, _, S, _), board(X2,Y,_,N2,_,_,_)  ==> X2 is X+1 |
    eq(S, N2).

% bridge cannot go south at bottom of board
bridge_constraints, board(X,_, _, _, _, S, _), xmax(Size)              ==> X == Size |
    S = 0.

% bridges going west == bridge going east at position left
bridge_constraints, board(X,Y, _, _, _, _, W), board(X,Y2,_, _,E2,_,_) ==> Y > 1 , Y2 is Y-1 |
    eq(W, E2).

% bridge connot go west at left of board
bridge_constraints, board(_,Y, _, _, _, _, W)                          ==> Y == 1 |
    W = 0.

% after doing all bridge constraints, make domains for remaining variables N E S W
bridge_constraints <=> make_domains.

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
X in Domain \ X in Domain <=> true.

% after generating all necessary domains, start the search
make_domains <=> enum_board.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELPER RULES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

clear_store \ board(_, _, _, _, _, _, _) <=> true.
clear_store \ xmax(_), ymax(_), domain_list(_) <=> true.
clear_store <=> true.

% load the Board from a puzzle fact
puzzle_board(Number) <=> puzzle(Number, Size, Islands) |
    % Each puzzle(Id, S, Islands) fact defines the input of one problem:
    % its identifier Id, the size S (width and height), and the list of islands Islands.
    ymax(Size),
    xmax(Size),

    % create the board facts from the island list
    islands_board(Islands).

xmax(Size) \ islands_board(Islands) <=>
    create_empty_board(1, 1, Size),
    create_islands(Islands).

% create a usable Board from an array of Islands
% each island takes the form (X, Y, N) where X is the row number, Y is the column
% number and N the number of bridges that should arrive in this island.
create_empty_board(_, Y, Size) <=> Y > Size |
    true.

create_empty_board(X, Y, Size) <=> X > Size |
    Y2 is Y + 1,
    create_empty_board(1, Y2, Size).

create_empty_board(X, Y, Size) <=> X =< Size |
    board(X, Y, 0, _, _, _, _),
    X2 is X + 1,
    create_empty_board(X2,Y,Size).

create_islands([]) <=>
    true.

create_islands([ [X, Y, Amount] | Islands ]), board(X, Y, _, N, E, S, W) <=>
    board(X, Y, Amount, N, E, S, W),
    create_islands(Islands).

% load the board from a matrix fact
puzzle_board(Number) <=> board(Number, Matrix), length(Matrix, XMax), nth1(1, Matrix, Row), length(Row, YMax) |
    % create a board from a matrix that contains the islands
    xmax(XMax),
    ymax(YMax),

    % create the board facts from the matrix
    board_facts_from_matrix(Matrix, 1).

% create a usable Board from a matrix that contains the islands
board_facts_from_matrix([], _).
board_facts_from_matrix([ Row | Rows ], X) <=>
    board_facts_from_row(Row, X, 1),
    XN is X + 1,
    board_facts_from_matrix(Rows, XN),
    !.

board_facts_from_row([], _, _).
board_facts_from_row([ Number | Row ], X, Y) <=>
    board(X, Y, Number, _, _, _, _),
    YN is Y + 1,
    board_facts_from_row(Row, X, YN).


board(X,Y, Val, NS, EW, _, _) \ print_board(X,Y) <=>
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
    print_board(X,Y2),
    !.


board(X, _, _, _, _, _, _) \ print_board(X, _) <=>
    X2 is X + 1,
    nl,
    print_board(X2,1).

print_board(_,_) <=> true.

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
    enum(N),
    enum(E),
    enum(S),
    enum(W).


% when an island is found, add to connected set????????????????????????
enum_board, board(A, B, Am, N, _, _, _) ==> Am > 0, number(N), N > 0 | find_neighbor(A, B, 'N').
enum_board, board(A, B, Am, _, E, _, _) ==> Am > 0, number(E), E > 0 | find_neighbor(A, B, 'E').
enum_board, board(A, B, Am, _, _, S, _) ==> Am > 0, number(S), S > 0 | find_neighbor(A, B, 'S').
enum_board, board(A, B, Am, _, _, _, W) ==> Am > 0, number(W), W > 0 | find_neighbor(A, B, 'W').

find_neighbor(A, B, Direction) <=>
    next_pos(A, B, Direction, C, D),
    find_neighbor(A, B, Direction, C, D).

board(X, Y, Amount, _, _, _, _) \ find_neighbor(OriginalX, OriginalY, _, X, Y) <=> Amount > 0 |
    connected([OriginalX, OriginalY], [X, Y]).

board(X, Y, Amount, _, _, _, _) \ find_neighbor(OriginalX, OriginalY, Direction, X, Y) <=> Amount == 0 |
    next_pos(X, Y, Direction, XN, YN),
    find_neighbor(OriginalX, OriginalY, Direction, XN, YN).

enum_board \ connected([A, B], [C, D]) <=> C < A         | connected([C, D], [A, B]).
enum_board \ connected([A, B], [C, D]) <=> C == A, D < B | connected([C, D], [A, B]).

enum_board, connected(A, B) \ connected(A, B) <=> true.
enum_board \ connected(A, A) <=> true.
enum_board, connected(A, B), connected(B, C) ==> connected(A, C).
enum_board, connected(A, B), connected(A, C) ==> B \== C | connected(B, C).

% % find first island to put in reachable set
% board(X, Y, Am1, _, _, _, _), board(X2, Y2, Am2, _, _, _, _) ==> Am1 > 0, Am2 > 0, X < X2 | reachable(X, Y).
% board(X, Y, Am1, _, _, _, _), board(X2, Y2, Am2, _, _, _, _) ==> Am1 > 0, Am2 > 0, X == X2, Y < Y2 | reachable(X, Y).
%
% % build reachable set
% connected, reachable(X, Y), connected([X, Y], [X2, Y2]) ==>
%     reachable(X2, Y2).
%

enum_board <=> connected.
% % all islands need to be connected
connected, board(A, B, Am1, _, _, _, _), board(C, D, Am2, _, _, _, _) \ connected([A, B], [C, D]) <=> Am1 > 0, Am2 > 0, A < C |
    true.
connected, board(A, B, Am1, _, _, _, _), board(C, D, Am2, _, _, _, _) \ connected([A, B], [C, D]) <=> Am1 > 0, Am2 > 0, A == C, B < D |
    true.
connected, board(_, _, Am1, _, _, _, _), board(_, _, Am2, _, _, _, _) ==> Am1 > 0, Am2 > 0 |
    false.

% upto(N, L): L = [1..N]
upto([], -1).
upto([ N | L ], N) :-
    N >= 0,
    N1 is N-1,
    upto(L, N1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SAMPLE PROBLEMS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
