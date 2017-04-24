:- use_module(library(chr)).
% TODO: ask if transpose is allowed
:- use_module(library(clpfd), [ transpose/2 ]).

:- chr_constraint solve/1, sudoku/1, print_board/1, print_numbers/1,
                  diff/2, list_diff/1, list_diff/2, rows_different/1, enum/1,
                  enum_board/1, upto/2, domain/2, make_domains/1, board_blocks/2,
                  solve1/0, solve2/0, solve3/0, solve4/0, solve5/0, solve6/0,
                  take_first_elements/3, take_elements/4, take_elements/3,
                  set_array_lengths/2, rows_blocks/4, board_blocks/4, putlist/1,
                  block/2, board_blocks2/1, generate_board/3, sn/1, n/1, board/4.

:- op(700, xfx, in).
:- op(700, xfx, le).
:- op(700, xfx, eq).
:- op(600, xfx, '..').
:- chr_constraint le/2, eq/2, in/2, add/3.

solve1 <=> solve(1).
solve2 <=> solve(2).
solve3 <=> solve(3).
solve4 <=> solve(4).
solve5 <=> solve(5).
solve6 <=> solve(6).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SUDOKU SOLUTION USING TRIVIAL VIEWPOINT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve(ProblemName) <=>
    % get the sudoku board
    problem(ProblemName, Board),
    print_board(Board),

    % sill the sudoku board
    sudoku(Board),
    writeln("\nResult:"),
    print_board(Board).

% (X, Y , Value, Block)


sudoku(Board) <=>
    % set the numbers's domains
    make_domains(Board),

    length(Board, N),
    sqrt(N, NN),
    SN is round(NN),
    sn(SN),
    n(N),

    generate_board(Board, 1, 1),
    % row constraints
    %rows_different(Board),

    % column constraints
    %transpose(Board, Board2),
    %rows_different(Board2),

    % block constraintss
    %board_blocks2(Board),

    %board_blocks(Board, Blocks),
    %rows_different(Blocks),

    % get the values
    enum_board(Board).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RULES USED FOR CONSTRAINTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

diff(X,Y) <=> nonvar(X), nonvar(Y) | X \== Y.

list_diff(_, []) <=> true.
list_diff(Val, [ Val2 | Vals ]) <=>
    diff(Val, Val2),
    list_diff(Val, Vals).

list_diff([]) <=> true.
list_diff([ Val | Vals ]) <=>
    list_diff(Val, Vals),
    list_diff(Vals).

rows_different([]) <=> true.
rows_different([ Row | Rows ]) <=>
    list_diff(Row),
    rows_different(Rows).


board(_,Y, _, Value1), board(_,Y, _, Value2) ==>
    diff(Value1, Value2).

board(X,_, _, Value1), board(X,_, _, Value2) ==>
        diff(Value1, Value2).

board(X1,Y1, BlockIndex, Value1), board(X2,Y2, BlockIndex, Value2) ==>
        diff(Value1, Value2).

%block(Index, Value1), block(Index,Value2) ==>
%    diff(Value1, Value2).

% generate_blocks(Board, X, Y) will generate the blocks with the block predicate
% It will do this by calculating the block index of each value in the board
% and it will store the value alongside it's block index in de block constraint.

% end case
n(N) \ generate_board(_, X, _) <=> N2 is N+1, X == N2 |
    true.

% When we have looped over all our columns
n(N) \ generate_board(Board, X, Y) <=> N2 is N+1, Y == N2 |
    X2 is X + 1,
    generate_board(Board, X2, 1).

sn(SN) \ generate_board(Board, X, Y) <=>
    %write("Generate_blocks: X: "), write(X), write(" Y: "), writeln(Y), writeln(SN),
    % First we calculate the block index
    XX is X-1,
    XXX is XX // SN,
    BlockRow is XXX + 1,

    YY is Y-1,
    YYY is YY // SN,
    BlockCol is YYY + 1,
    BlockIndex is (BlockRow-1) * SN + BlockCol,

    % Then we need to get the number outside of the board
    nth1(X, Board, Row),
    nth1(Y, Row, Number),

    % save this number for this block index
    board(X,Y, BlockIndex, Number),

    % Go to the next case
    Y2 is Y + 1,
    generate_board(Board, X, Y2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RULES USED FOR DOMAIN SOLVING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% enum(L): fills L with values
enum([])                        <=> true.
enum([ X | Tail ])              <=> number(X) | enum(Tail).
enum([ X | Tail ]), X in Domain <=> member(X, Domain), enum(Tail).

% enum_board(Board): fills Board with values
enum_board([]) <=> true.
enum_board([ Row | Rows ]) <=>
    enum(Row),
    enum_board(Rows).

% length(X, 4), length(Y, 4), domain(X, Y), upto(Y, 4).
% length(X, 4), length(Y, 4), domain(X, Y), upto(Y, 4), enum(X).
% upto(N, L): L = [1..N]
upto([], 0).
upto([ N | L ], N) :-
    N > 0,
    N1 is N-1,
    upto(L, N1).

% domain(L, D): create 'X in D' constraints for all X from L
domain([], _) <=> true.
domain([ Val | Tail ], Domain) <=>
    Val in Domain,
    domain(Tail, Domain).

% make_domains(L): L is an list of N elements, make_domains creates 'X in [1..N]' constraints
make_domains([]) <=> true.
make_domains([ Row | Tail ]) <=>
    length(Row, N),
    upto(DomainList, N),
    domain(Row, DomainList),
    make_domains(Tail).

% nth1_list(Index, List, ElemList): On position Index, List contains all elements of ElemList
nth1_list(_, _, []).
nth1_list(Index, List, [ Elem | Tail ]) :-
    write("        Element: "), write(Elem), write(" in Block: "), write(List), write(" on position "), writeln(Index),
    nth1(Index, List, Elem),
    Next is Index + 1,
    nth1_list(Next, List, Tail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELPER RULES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_numbers([]) <=> writeln("").
print_numbers([ Number | Tail ]) <=>
    nonvar(Number) |
    write(" "),
    write(Number),
    print_numbers(Tail).

print_numbers([ _ | Tail ]) <=>
    write(" _"),
    print_numbers(Tail).

print_board([]) <=> writeln("").
print_board([ Row | Tail ]) <=>
    print_numbers(Row),
    print_board(Tail).

% set_matrix_lengths(Rows, N): sets the lengths of rows in Rows to N
set_array_lengths([], _) <=> true.
set_array_lengths([ Row | Rows ], N) <=>
    length(Row, N),
    set_array_lengths(Rows, N).

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

sulution(5, [ [1, 4, 3, 2],
              [3, 2, 1, 4],
              [2, 1, 4, 3],
              [4, 3, 2, 1] ]).
