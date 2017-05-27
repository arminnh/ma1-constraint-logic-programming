:- use_module(library(chr)).

:- chr_constraint solve/1, puzzle_board/1, print_board/0, hashiwokakero/1.
:- chr_constraint print_row/1, print_pos/1, enum/1, enum_board/0.
:- chr_constraint make_domain/2, make_domains/1, domain_list/1.
:- chr_constraint islands_board/1, matrix_board/2, create_islands/1, create_empty_board/3.
:- chr_constraint board/7, create_board/3, output/1, size/1, print_board/2, board_facts_from_row/3.

:- op(700, xfx, in).
:- op(700, xfx, le).
:- op(700, xfx, eq).
:- op(600, xfx, '..').
:- chr_constraint le/2, eq/2, in/2, add/3.
:- chr_option(debug,off).
:- chr_option(optimize,full).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HASHIWOKAKERO SOLUTION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% solve a given game board
solve(Number) <=>
    % find the game board
    puzzle_board(Number),
    upto(DomainList, 2),
    domain_list(DomainList),
    writeln("Given board:"),
    print_board,

    enum_board.

    % create bridges and set constraints
    %hashiwokakero_constraints,
    %writeln("kk"),

    % do search on variables
    %search(naive, Board),

    % Check that everything is connected
    %writeln("connected"),
    %board_connected_set(Board),

    % print results
    %writeln("Search done:"),
    %print_board.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RULES USED FOR READING BOARD
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% load the Board from a puzzle fact
puzzle_board(Number) <=>
    % Each puzzle(Id, S, Islands) fact defines the input of one problem:
    % its identifier Id, the size S (width and height), and the list of islands Islands.
    puzzle(Number, Size, Islands) |
    size(Size),
    % create a board with the islands on it
    islands_board(Islands).

%load the board from a matrix fact
puzzle_board(Number) <=>
    % create a board from a matrix that contains the islands
    board(Number, Matrix) |
    board_facts_from_matrix(Matrix, 1).

% create a usable Board from a matrix that contains the islands
board_facts_from_matrix([], _).
board_facts_from_matrix([ Row | Rows ], X) :-
    board_facts_from_row(Row),
    XN is X + 1,
    board_facts_from_matrix(Rows, XN).

board_facts_from_row([]).
board_facts_from_row([ Number | Row ], X, Y) <=>
    board(X, Y, Number, N, E, S, W),
    N in Domain,
    E in Domain,
    S in Domain,
    W in Domain,
    YN is Y + 1,
    board_facts_from_row(Row, X, YN).

% create a usable Board from an array of Islands
% each island takes the form (X, Y, N) where X is the row number, Y is the column
% number and N the number of bridges that should arrive in this island.
create_empty_board(_,Y, Size) <=> Y > Size|
    true.

create_empty_board(X,Y, Size) <=> X > Size|
    % X Y Amount, N E, S, W
    Y2 is Y + 1,
    create_empty_board(1,Y2,Size)
    .

domain_list(Domain) \ create_empty_board(X,Y, Size) <=> X =< Size|
    % X Y Amount, N E, S, W
    board(X,Y, 0, N, E, S, W),
    N in Domain,
    E in Domain,
    S in Domain,
    W in Domain,
    X2 is X + 1,
    create_empty_board(X2,Y,Size)
    .

create_islands([]) <=>
    true.

domain_list(Domain) \ create_islands([ [X,Y,Amount] | Islands]), board(X,Y, _ , N , E , S,W) <=>
    board(X,Y, Amount , N , E , S,W),
    create_islands(Islands).

size(Size) \ islands_board(Islands) <=>
    create_empty_board(1,1, Size),
    %dim(Board, [Size, Size, 5]),
    create_islands(Islands),
    true.


size(Size) \ print_board(_,Y) <=> Y > Size|
    nl, nl,
    true.

size(Size) \ print_board(X,Y) <=> X > Size|
    % X Y Amount, N E, S, W
    Y2 is Y + 1,
    nl,
    print_board(1,Y2)
    .

size(Size), board(X,Y, Val, NS, EW, _, _) \ print_board(X,Y) <=> X =< Size |
    % X Y Amount, N E, S, W
    (Val > 0 ->
        write(Val)
    ;
        ( nonvar(NS), nonvar(EW) ->
            symbol(NS, EW, Char),
            write(Char)
        ;
            write(' ')
        )
    ),
    X2 is X + 1,
    print_board(X2,Y)
    .

print_board <=>
    print_board(1,1),
    true.


symbol(0, 0, ' ').
symbol(0, 1, '-').
symbol(0, 2, '=').
symbol(1, 0, '|').
symbol(2, 0, '"').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RULES USED FOR CONSTRAINTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RULES USED FOR DOMAIN SOLVING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% enum(L): assigns values to variables X in L
enum(X)              <=> number(X) | true.
enum(X), X in Domain <=> member(X, Domain).

% enum_board(Board): fills Board with values
%enum_board <=> true.

board(X, Y, Amount, N, E, S, W), enum_board ==>
    enum(N),
    enum(E),
    enum(S),
    enum(W).

% upto(N, L): L = [1..N]
upto([], -1).
upto([ N | L ], N) :-
    N >= 0,
    N1 is N-1,
    upto(L, N1).


% make_domain(L, D): create 'X in D' constraints for all variables X in L
make_domain([], _) <=> true.
make_domain([ Val | Tail ], DomainList) <=> var(Val) |
    Val in DomainList,
    make_domain(Tail, DomainList).
make_domain([ _ | Tail ], DomainList) <=>
    make_domain(Tail, DomainList).

% make_domains(L): L is an list of N elements, make_domains creates 'X in [1..N]' constraints
make_domains([]) <=> true.
domain_list(DomainList) \ make_domains([ Row | Tail ]) <=>
    list_remove_vars(Row, NewRow),
    % Domain list is 1..N, NewRow are the values on a specific Row
    subtract(DomainList, NewRow, SmallerDomainList),
    writeln([DomainList, Row, NewRow, SmallerDomainList]),

    % For a specific row
    make_domain(Row, SmallerDomainList),

    % For the rest of the board
    make_domains(Tail).

list_remove_vars([], []).
list_remove_vars([ Head | Tail1 ], FilteredList) :-
    var(Head),
    list_remove_vars(Tail1, FilteredList).
list_remove_vars([ Head | Tail1 ], [ Head | Tail2 ]) :-
    list_remove_vars(Tail1, Tail2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELPER RULES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



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
