:- use_module(library(chr)).

:- chr_constraint solve/1, sudoku/1, print_board/2.
:- chr_constraint diff/2, smart_diff/6, enum/1, enum_board/0, upto/2, domain_list/1.
:- chr_constraint board/4, generate_known_board_facts/3.
:- chr_constraint sn/1, n/1.
:- chr_constraint generate_remaining_board_facts/1, generate_board_value_facts/2.
:- chr_constraint do_diffs/0.

:- op(700, xfx, in).
:- op(700, xfx, le).
:- op(700, xfx, eq).
:- op(600, xfx, '..').
:- chr_constraint le/2, eq/2, in/2, add/3.
:- chr_option(debug,off).
:- chr_option(optimize,full).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SUDOKU SOLUTION USING TRIVIAL VIEWPOINT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve(ProblemName) <=>
    % statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
    statistics(walltime, [_ | [_]]),

    % get the sudoku board
    problem(ProblemName, Board),

    % fill the sudoku board
    sudoku(Board),

    writeln("\nResult:"),
    print_board(1,1),

    % statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),
    statistics(walltime, [_ | [ExecutionTimeMS]]),
    write('Execution took '), write(ExecutionTimeMS), write(' ms.'), nl,

    ExTimeS is ExecutionTimeMS / 1000,
    write('Execution took '), write(ExTimeS), write(' s.'), nl,

    ExTimeM is ExTimeS / 60,
    write('Execution took '), write(ExTimeM), write(' min.'), nl,
    true.

sudoku(Board) <=>
    % store N for later reuse = size of N*N board
    length(Board, N),
    n(N),

    % store SN for later reuse = sqrt(N) = amount of sudoku blocks
    sqrt(N, NN),
    SN is round(NN),
    sn(SN),

    % create and store a list that contains the domain of the possible values on the board
    upto(DomainList, N),
    domain_list(DomainList),

    % generate (X, Y, BlockIndex, Value) facts
    % those facts will later be used for insertion of diff(A, B) rules
    generate_known_board_facts(Board, 1, 1),

    % set the domains of the possible values on the board
    generate_remaining_board_facts(N),

    % start generation of diffs
    do_diffs,

    print_board(1,1),

    % start search for values
    enum_board,
    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RULES USED FOR CONSTRAINTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_board_value_facts(_, 0) <=>
    true.

% if fact already exists on this X value for the Value, don't generate another one
board(Value, Index, _, _) \ generate_board_value_facts(Value, Index)<=>
    Index2 is Index - 1,
    generate_board_value_facts(Value, Index2).

domain_list(Domain) \ generate_board_value_facts(Value, Index) <=> Index > 0 |
    board(Value, Index, Y, BlockIndex),
    Y in Domain,
    BlockIndex in Domain,
    Index2 is Index - 1,
    generate_board_value_facts(Value, Index2).

generate_remaining_board_facts(0) <=>
    true.

n(N) \ generate_remaining_board_facts(Value) <=>

    generate_board_value_facts(Value, N),
    Value2 is Value - 1,
    generate_remaining_board_facts(Value2).


% generate_known_board_facts(Board, X, Y) will generate board(X,Y, BlockIndex, Value)
% facts which will later be used to instert diff rules into the constraint store

% got all values on the board
n(N) \ generate_known_board_facts(_, X, _) <=> N2 is N+1, X == N2 |
    true.

% after going over all columns, go to next row and start from column 1 again
n(N) \ generate_known_board_facts(Board, X, Y) <=> N2 is N+1, Y == N2 |
    X2 is X + 1,
    generate_known_board_facts(Board, X2, 1).

generate_known_board_facts(Board, X, Y) <=>  nth1(X, Board, Row), nth1(Y, Row, Value), var(Value) |
    Y2 is Y + 1,
    generate_known_board_facts(Board, X, Y2).

sn(SN) \ generate_known_board_facts(Board, X, Y) <=> nth1(X, Board, Row), nth1(Y, Row, Value), nonvar(Value) |
    % get the value on position (X, Y) on the board

    % calculate block index
    XX is X-1,
    XXX is XX // SN,
    BlockRow is XXX + 1,

    YY is Y-1,
    YYY is YY // SN,
    BlockCol is YYY + 1,
    BlockIndex is (BlockRow-1) * SN + BlockCol,

    % save this data for later use
    board(Value, X,Y, BlockIndex),

    % go to the next case
    Y2 is Y + 1,
    generate_known_board_facts(Board, X, Y2).


% amount of diffs: sum([1..N-1]) * N * 3
%     3 because: positions for values on different columns,
%                posistions for values in different blocks
%                no 2 values on same block

% all values in same blocks must be different, guards used to break symmetry
do_diffs, board(Value, X1, Y1, BlockIndex1), board(Value, X2, Y2, BlockIndex2) ==> X1 < X2 |
    diff(Y1,Y2), diff(BlockIndex1, BlockIndex2).

% do_diffs, board(Value, X1, Y1, BlockIndex1), board(Value, X2, Y2, BlockIndex2) ==> X1 < X2 |
%     smart_diff(X1, Y1, X2, Y2, BlockIndex1, BlockIndex2), diff(BlockIndex1, BlockIndex2).

do_diffs, board(Value1, X, Y1, _), board(Value2, X, Y2, _) ==> Value1 < Value2 |
    diff(Y1,Y2).

% no need for symmetry breaking here as it's been done during construction
% diff(X, Y), diff(X, Y) <=> diff(X, Y).
% diff(Y, X), diff(X, Y) <=> diff(X, Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RULES USED FOR DOMAIN SOLVING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

same_block_row(Block1, Block2, SN):-
    B1 is Block1 - 1,
    B2 is Block2 - 1,
    R1 is div(B1, SN),
    R2 is div(B2, SN),
    R1 == R2.

interval([Start|_], Start, Start).
interval([N|T], Start, N):-
    N > Start,
    N1 is N - 1,
    interval(T, Start, N1).

block_y_vals(Block, SN, L):-
    B is Block -1,
    R is mod(B, SN),
    Start is R * SN + 1,
    End is (R+1) * SN,
    interval(L,Start,End),
    length(L, SN).

% X and Y are instantiated and are different
smart_diff(_,Y1, _,  Y2, _, _) <=> nonvar(Y1), nonvar(Y2) | Y1 \== Y2.
sn(SN), smart_diff(X1, Y1, X2, Y2, _, Block2) \ Y1 in L <=>
    nonvar(Y2), nonvar(Block2), same_block_row(X1, X2, SN),
    block_y_vals(Block2, SN, Columns), subtract(L, Columns, NL), L \== NL, length(NL,C1), C1 > 0 | Y1 in NL.

sn(SN), smart_diff(X1, Y1, X2, Y2, Block1, _) \ Y2 in L <=>
    nonvar(Y1), nonvar(Block1), same_block_row(X1, X2, SN),
    block_y_vals(Block1, SN, Columns), subtract(L, Columns, NL), L \== NL,length(NL,C1), C1 > 0  | Y2 in NL.

smart_diff(_, Y, _,  X, _, _) \ X in L <=> nonvar(Y), select(Y, L, NL) | X in NL.
smart_diff(_,X,_, Y,_,_) \ X in L <=> nonvar(Y), select(Y, L, NL) | X in NL.


diff(X, Y) <=> nonvar(X), nonvar(Y) | X \== Y.
diff(Y, X) \ X in L <=> nonvar(Y), select(Y, L, NL) | X in NL.
diff(X, Y) \ X in L <=> nonvar(Y), select(Y, L, NL) | X in NL.

% enum(L): assigns values to variables X in L
enum(X)              <=> number(X) | true .
enum(X), X in Domain <=> member(X, Domain).


board(_,_,Y, _) \ Y in [D] <=> var(Y) |
    Y is D.

board(_, _, Y, _), enum_board ==>
    enum(Y).
    % enum(BlockIndex),

sn(SN), board(_, X, Y, BlockIndex), enum_board ==> number(Y), var(BlockIndex) |
    XX is X-1,

    XXX is XX // SN,

    BlockRow is XXX + 1,
    YY is Y-1,

    YYY is YY // SN,

    BlockCol is YYY + 1,

    BlockIndex is (BlockRow-1) * SN + BlockCol.

% upto(N, L): L = [1..N]
upto([], 0).
upto([ N | L ], N) :-
    N > 0,
    N1 is N-1,
    upto(L, N1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELPER RULES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

n(N) \ print_board(X,_) <=> X > N |
        true.

n(N) \ print_board(X,Y) <=> Y > N |
    X2 is X + 1,
    writeln(""),
    print_board(X2, 1).


board(Value, X, Y, _) \ print_board(X,Y) <=>  nonvar(Value) |
    write(" "),
    write(Value),
    Y2 is Y + 1,
    print_board(X,Y2).

 board(Value, X, Y, _) \ print_board(X,Y) <=> var(Value) |
    write(" _"),
    Y2 is Y + 1,
    print_board(X,Y2).

% If board on this position doesn't exist.
print_board(X,Y2) <=>
    write(" _"),
    Y3 is Y2 + 1,
    print_board(X,Y3).

solve1() :- solve(1).
solve2() :- solve(2).
solve3() :- solve(3).
solve4() :- solve(4).
solve5() :- solve(5).
solve6() :- solve(6).
solve7() :- solve(7).
solve8() :- solve(8).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SAMPLE PROBLEMS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% alternative version to store numbers: problem(ProblemName, X, Y, Number)
problem(1, [ [_, _, 2, _, _, 5, _, 7, 9],
             [1, _, 5, _, _, 3, _, _, _],
             [_, _, _, _, _, _, 6, _, _],
             [_, 1, _, 4, _, _, 9, _, _],
             [_, 9, _, _, _, _, _, 8, _],
             [_, _, 4, _, _, 9, _, 1, _],
             [_, _, 9, _, _, _, _, _, _],
             [_, _, _, 1, _, _, 3, _, 6],
             [6, 8, _, 3, _, _, 4, _, _] ]).

problem(2, [ [_, _, 3, _, _, 8, _, _, 6],
             [_, _, _, 4, 6, _, _, _, _],
             [_, _, _, 1, _, _, 5, 9, _],
             [_, 9, 8, _, _, _, 6, 4, _],
             [_, _, _, _, 7, _, _, _, _],
             [_, 1, 7, _, _, _, 9, 5, _],
             [_, 2, 4, _, _, 1, _, _, _],
             [_, _, _, _, 4, 6, _, _, _],
             [6, _, _, 5, _, _, 8, _, _] ]).

problem(3, [ [_] ]).

problem(4, [ [2, 1],
	         [_, _] ]).

problem(5, [ [1, _, 3, _],
	         [3, _, _, 4],
	         [2, _, _, _],
	         [4, _, 2, _] ]).

problem(6, [ [_, _, _, _],
		     [_, _, _, _],
	      	 [_, _, _, _],
		     [_, _, _, _] ]).

problem(7, [ [1, _, 3, _, _, 5, 4, 8],
	         [3, 5, 2, 4, _, 6, 7, _],
	         [2, _, 6, 3, _, 4, _, _],
	         [4, 2, 5, _, 7, _, 1, _],
	         [5, 3, 7, _, 6, 2, _, 4],
	         [6, _, _, 5, _, 7, _, _],
             [7, 4, _, 6, _, _, 3, _],
             [8, _, _, _, _, _, 6, _] ]).

% very easy sudoku from http://www.sudokukingdom.com/very-easy-sudoku.php
problem(8, [ [8, _, 4, _, 5, 7, _, _, 9],
             [5, 7, _, _, _, _, 5, _, 1],
             [1, _, _, 4, 8, 2, 7, 3, _],
             [6, 1, _, _, _, 3, 8, _, 4],
             [_, 4, _, 2, _, 8, 1, 5, _],
             [2, _, 8, _, 1, 4, _, _, 3],
             [_, 2, 9, 3, _, _, _, 1, _],
             [_, 8, 1, 9, 2, _, _, 7, 6],
             [_, _, 5, 8, 7, _, 9, 4, _] ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SOLUTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Solution for problem 1 in the trivial viewpoint:
solution(1, [ [3,  6,  2,  8,  4,  5,  1,  7,  9],
              [1,  7,  5,  9,  6,  3,  2,  4,  8],
              [9,  4,  8,  2,  1,  7,  6,  3,  5],
              [7,  1,  3,  4,  5,  8,  9,  6,  2],
              [2,  9,  6,  7,  3,  1,  5,  8,  4],
              [8,  5,  4,  6,  2,  9,  7,  1,  3],
              [4,  3,  9,  5,  7,  6,  8,  2,  1],
              [5,  2,  7,  1,  8,  4,  3,  9,  6],
              [6,  8,  1,  3,  9,  2,  4,  5,  7] ]).

solution(2, [ [1] ]).

solution(3, [ [2, 1],
              [1, 2] ]).

solution(4, [ [1, 2, 3],
              [3, 1, 2],
              [2, 3, 1] ]).

solution(5, [ [1, 4, 3, 2],
              [3, 2, 1, 4],
              [2, 1, 4, 3],
              [4, 3, 2, 1] ]).

solution(8, [ [8, 3, 4, 1, 5, 7, 2, 6, 9],
              [5, 7, 2, 6, 3, 9, 5, 8, 1],
              [1, 9, 6, 4, 8, 2, 7, 3, 5],
              [6, 1, 7, 5, 9, 3, 8, 2, 4],
              [9, 4, 3, 2, 6, 8, 1, 5, 7],
              [2, 5, 8, 7, 1, 4, 6, 9, 3],
              [7, 2, 9, 3, 4, 6, 5, 1, 8],
              [4, 8, 1, 9, 2, 5, 3, 7, 6],
              [3, 6, 5, 8, 7, 1, 9, 4, 2] ]).
