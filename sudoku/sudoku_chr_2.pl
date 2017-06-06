:- use_module(library(chr)).

:- chr_constraint solve_viewpoint2/1, sudoku_viewpoint2/1, print_board_viewpoint2/2.
:- chr_constraint diff_viewpoint2/2, smart_diff_viewpoint2/6, enum_viewpoint2/1,
                enum_board_viewpoint2/0, upto_viewpoint2/2, domain_list_viewpoint2/1.
:- chr_constraint board_viewpoint2/4, generate_known_board_facts_viewpoint2/3.
:- chr_constraint sn_viewpoint2/1, n_viewpoint2/1.
:- chr_constraint generate_remaining_board_facts_viewpoint2/1, generate_board_value_facts_viewpoint2/2.
:- chr_constraint do_diffs_viewpoint2/0.


:- op(700, xfx, in).
:- op(700, xfx, le).
:- op(700, xfx, eq).
:- op(600, xfx, '..').
:- chr_constraint le/2, eq/2, in/2, add/3.
:- chr_option(debug,off).
:- chr_option(optimize,full).
:- consult(sudex_toledo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SUDOKU SOLUTION USING NUMBERS HAVE POSITION VIEWPOINT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve_viewpoint2(ProblemName) <=>
    % statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
    statistics(walltime, [_ | [_]]),

    % get the sudoku_viewpoint2 board_viewpoint2
    (problem(ProblemName, Board) ; puzzles(Board, ProblemName)),

    % fill the sudoku_viewpoint2 board_viewpoint2
    sudoku_viewpoint2(Board),

    writeln("\nResult:"),
    print_board_viewpoint2(1,1),

    % statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),
    statistics(walltime, [_ | [ExecutionTimeMS]]),
    write('Execution took '), write(ExecutionTimeMS), write(' ms.'), nl,

    ExTimeS is ExecutionTimeMS / 1000,
    write('Execution took '), write(ExTimeS), write(' s.'), nl,

    ExTimeM is ExTimeS / 60,
    write('Execution took '), write(ExTimeM), write(' min.'), nl,
    true.

sudoku_viewpoint2(Board) <=>
    % store N for later reuse = size of N*N board_viewpoint2
    length(Board, N),
    n_viewpoint2(N),

    % store SN for later reuse = sqrt(N) = amount of sudoku_viewpoint2 blocks
    sqrt(N, NN),
    SN is round(NN),
    sn_viewpoint2(SN),

    % create and store a list that contains the domain of the possible values on the board_viewpoint2
    upto_viewpoint2(DomainList, N),
    domain_list_viewpoint2(DomainList),

    % generate (X, Y, BlockIndex, Value) facts
    % those facts will later be used for insertion of diff_viewpoint2(A, B) rules
    generate_known_board_facts_viewpoint2(Board, 1, 1),

    % set the domains of the possible values on the board_viewpoint2
    generate_remaining_board_facts_viewpoint2(N),

    % start generation of diffs
    do_diffs_viewpoint2,

    print_board_viewpoint2(1,1),

    % start search for values
    enum_board_viewpoint2,
    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RULES USED FOR CONSTRAINTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Generates the board facts thus board(Number, X, Y, Blockindex)
generate_board_value_facts_viewpoint2(_, 0) <=>
    true.

% if fact already exists on this X value for the Value, don't generate another one
board_viewpoint2(Value, Index, _, _) \ generate_board_value_facts_viewpoint2(Value, Index)<=>
    Index2 is Index - 1,
    generate_board_value_facts_viewpoint2(Value, Index2).

% Fact doesn't exist yet, create it
domain_list_viewpoint2(Domain) \ generate_board_value_facts_viewpoint2(Value, Index) <=> Index > 0 |
    board_viewpoint2(Value, Index, Y, BlockIndex),
    Y in Domain,
    BlockIndex in Domain,
    Index2 is Index - 1,
    generate_board_value_facts_viewpoint2(Value, Index2).

generate_remaining_board_facts_viewpoint2(0) <=>
    true.

n_viewpoint2(N) \ generate_remaining_board_facts_viewpoint2(Value) <=>
    generate_board_value_facts_viewpoint2(Value, N),
    Value2 is Value - 1,
    generate_remaining_board_facts_viewpoint2(Value2).


% generate_known_board_facts_viewpoint2(Board, X, Y) will generate board_viewpoint2(X,Y, BlockIndex, Value)
% facts which will later be used to instert diff_viewpoint2 rules into the constraint store

% got all values on the board_viewpoint2
n_viewpoint2(N) \ generate_known_board_facts_viewpoint2(_, X, _) <=> N2 is N+1, X == N2 |
    true.

% after going over all columns, go to next row and start from column 1 again
n_viewpoint2(N) \ generate_known_board_facts_viewpoint2(Board, X, Y) <=> N2 is N+1, Y == N2 |
    X2 is X + 1,
    generate_known_board_facts_viewpoint2(Board, X2, 1).

generate_known_board_facts_viewpoint2(Board, X, Y) <=>  nth1(X, Board, Row), nth1(Y, Row, Value), var(Value) |
    Y2 is Y + 1,
    generate_known_board_facts_viewpoint2(Board, X, Y2).

sn_viewpoint2(SN) \ generate_known_board_facts_viewpoint2(Board, X, Y) <=> nth1(X, Board, Row), nth1(Y, Row, Value), nonvar(Value) |
    % get the value on position (X, Y) on the board_viewpoint2

    % calculate block index
    XX is X-1,
    XXX is XX // SN,
    BlockRow is XXX + 1,

    YY is Y-1,
    YYY is YY // SN,
    BlockCol is YYY + 1,
    BlockIndex is (BlockRow-1) * SN + BlockCol,

    % save this data for later use
    board_viewpoint2(Value, X,Y, BlockIndex),

    % go to the next case
    Y2 is Y + 1,
    generate_known_board_facts_viewpoint2(Board, X, Y2).


% amount of diffs: sum([1..N-1]) * N * 3
%     3 because: positions for values on different columns,
%                posistions for values in different blocks
%                no 2 values on same block

% all values in same blocks must be different, guards used to break symmetry

%% UNCOMMENT THIS FOR VERSION WITHOUT SMARTDIFFS
% do_diffs_viewpoint2, board_viewpoint2(Value, X1, Y1, BlockIndex1), board_viewpoint2(Value, X2, Y2, BlockIndex2) ==> X1 < X2 |
%     diff_viewpoint2(Y1,Y2), diff_viewpoint2(BlockIndex1, BlockIndex2).

% Do diffs between board facts in a smart way
do_diffs_viewpoint2, board_viewpoint2(Value, X1, Y1, BlockIndex1), board_viewpoint2(Value, X2, Y2, BlockIndex2) ==> X1 < X2 |
    smart_diff_viewpoint2(X1, Y1, X2, Y2, BlockIndex1, BlockIndex2), diff_viewpoint2(BlockIndex1, BlockIndex2).

do_diffs_viewpoint2, board_viewpoint2(Value1, X, Y1, _), board_viewpoint2(Value2, X, Y2, _) ==> Value1 < Value2 |
    diff_viewpoint2(Y1,Y2).

% no need for symmetry breaking here as it's been done during construction
% diff_viewpoint2(X, Y), diff_viewpoint2(X, Y) <=> diff_viewpoint2(X, Y).
% diff_viewpoint2(Y, X), diff_viewpoint2(X, Y) <=> diff_viewpoint2(X, Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RULES USED FOR DOMAIN SOLVING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Checks if two block indices are on the same block row
same_block_row(Block1, Block2, SN):-
    B1 is Block1 - 1,
    B2 is Block2 - 1,
    R1 is div(B1, SN),
    R2 is div(B2, SN),
    R1 == R2.

% Create all the values in a certain interval
% So Start 1 and N 3 then we get [1,2,3]
interval([Start|_], Start, Start).
interval([N|T], Start, N):-
    N > Start,
    N1 is N - 1,
    interval(T, Start, N1).

% Creates the y values of a block
block_y_vals(Block, SN, L):-
    B is Block -1,
    R is mod(B, SN),
    Start is R * SN + 1,
    End is (R+1) * SN,
    interval(L,Start,End),
    length(L, SN).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SMART DIFF RULES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% X and Y are instantiated and are different
smart_diff_viewpoint2(_,Y1, _,  Y2, _, _) <=> nonvar(Y1), nonvar(Y2) | Y1 \== Y2.

% Okay so we know one Y value, this means that we also know it's Block number
% So we can remove all these Y values from the other boards with the same number
% in this block row
sn_viewpoint2(SN), smart_diff_viewpoint2(X1, Y1, X2, Y2, _, Block2) \ Y1 in L <=>
    nonvar(Y2), nonvar(Block2), same_block_row(X1, X2, SN),
    block_y_vals(Block2, SN, Columns), subtract(L, Columns, NL), L \== NL, length(NL,C1), C1 > 0 | Y1 in NL.

% Okay so we know one Y value, this means that we also know it's Block number
% So we can remove all these Y values from the other boards with the same number
% in this block row
sn_viewpoint2(SN), smart_diff_viewpoint2(X1, Y1, X2, Y2, Block1, _) \ Y2 in L <=>
    nonvar(Y1), nonvar(Block1), same_block_row(X1, X2, SN),
    block_y_vals(Block1, SN, Columns), subtract(L, Columns, NL), L \== NL,length(NL,C1), C1 > 0  | Y2 in NL.

% If we're not on the same block row the only thing we can do is just remove this
% value from our domain
smart_diff_viewpoint2(_, Y, _,  X, _, _) \ X in L <=> nonvar(Y), select(Y, L, NL) | X in NL.
smart_diff_viewpoint2(_,X,_, Y,_,_) \ X in L <=> nonvar(Y), select(Y, L, NL) | X in NL.

% Normal diffs
diff_viewpoint2(X, Y) <=> nonvar(X), nonvar(Y) | X \== Y.
diff_viewpoint2(Y, X) \ X in L <=> nonvar(Y), select(Y, L, NL) | X in NL.
diff_viewpoint2(X, Y) \ X in L <=> nonvar(Y), select(Y, L, NL) | X in NL.

% enum_viewpoint2(L): assigns values to variables X in L
enum_viewpoint2(X)              <=> number(X) | true .
enum_viewpoint2(X), X in Domain <=> member(X, Domain).

board_viewpoint2(_,_,_, B) \ B in [D] <=> var(B) |
    B is D.

board_viewpoint2(_,_,Y, _) \ Y in [D] <=> var(Y) |
    Y is D.

board_viewpoint2(_, _, Y, _), enum_board_viewpoint2 ==>
    enum_viewpoint2(Y).
    % enum_viewpoint2(BlockIndex),

% Fixes the blockindex value
sn_viewpoint2(SN), board_viewpoint2(_, X, Y, BlockIndex), enum_board_viewpoint2 \ BlockIndex in D <=> number(Y), var(BlockIndex) |
    XX is X-1,

    XXX is XX // SN,

    BlockRow is XXX + 1,
    YY is Y-1,

    YYY is YY // SN,

    BlockCol is YYY + 1,

    BlockIndex is (BlockRow-1) * SN + BlockCol.

% upto_viewpoint2(N, L): L = [1..N]
upto_viewpoint2([], 0).
upto_viewpoint2([ N | L ], N) :-
    N > 0,
    N1 is N-1,
    upto_viewpoint2(L, N1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELPER RULES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

n_viewpoint2(N) \ print_board_viewpoint2(X,_) <=> X > N |
        true.

n_viewpoint2(N) \ print_board_viewpoint2(X,Y) <=> Y > N |
    X2 is X + 1,
    writeln(""),
    print_board_viewpoint2(X2, 1).


board_viewpoint2(Value, X, Y, _) \ print_board_viewpoint2(X,Y) <=>  nonvar(Value) |
    write(" "),
    write(Value),
    Y2 is Y + 1,
    print_board_viewpoint2(X,Y2).

 board_viewpoint2(Value, X, Y, _) \ print_board_viewpoint2(X,Y) <=> var(Value) |
    write(" _"),
    Y2 is Y + 1,
    print_board_viewpoint2(X,Y2).

% If board_viewpoint2 on this position doesn't exist.
print_board_viewpoint2(X,Y2) <=>
    write(" _"),
    Y3 is Y2 + 1,
    print_board_viewpoint2(X,Y3).

solve1() :- solve_viewpoint2(1).
solve2() :- solve_viewpoint2(2).
solve3() :- solve_viewpoint2(3).
solve4() :- solve_viewpoint2(4).
solve5() :- solve_viewpoint2(5).
solve6() :- solve_viewpoint2(6).
solve7() :- solve_viewpoint2(7).
solve8() :- solve_viewpoint2(8).

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

% very easy sudoku_viewpoint2 from http://www.sudokukingdom.com/very-easy-sudoku_viewpoint2.php
problem(8, [ [8, _, 4, _, 5, 7, _, _, 9],
             [5, 7, _, _, _, _, 5, _, 1],
             [1, _, _, 4, 8, 2, 7, 3, _],
             [6, 1, _, _, _, 3, 8, _, 4],
             [_, 4, _, 2, _, 8, 1, 5, _],
             [2, _, 8, _, 1, 4, _, _, 3],
             [_, 2, 9, 3, _, _, _, 1, _],
             [_, 8, 1, 9, 2, _, _, 7, 6],
             [_, _, 5, 8, 7, _, 9, 4, _] ]).

 problem(9, [ [1, _, 2, _, _, _, _, _, _],
              [3, 8, 7, _, _, _, _, _, _],
              [4, 6, 5, _, _, _, _, _, _],
              [_, _, _, _, _, _, _, _, _],
              [_, _, _, _, _, _, _, _, _],
              [_, _, _, _, _, _, _, _, _],
              [_, _, _, _, _, _, _, _, _],
              [_, _, _, _, _, _, _, _, _],
              [_, _, _, _, _, _, _, _, _] ]).
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
