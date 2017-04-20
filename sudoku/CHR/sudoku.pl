:- use_module(library(chr)).
% TODO: ask if transpose is allowed
:- use_module(library(clpfd), [ transpose/2 ]).

:- chr_constraint solve/1, sudoku/1, print_board/1, print_numbers/1,
                  diff/2, list_diff/1, list_diff/2, rows_different/1, enum/1,
                  enum_board/1, upto/2, domain/2, make_domains/1, board_blocks/2,
                  solve1/0, solve2/0, solve3/0, solve4/0, solve5/0,
                  take_first_elements/3, take_elements/4, set_array_lengths/2,
                  rows_blocks/4, board_blocks/4.

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

sudoku(Board) <=>
    % set the numbers's domains
    make_domains(Board),

    % row constraints
    rows_different(Board),

    % column constraints
    transpose(Board, Board2),
    rows_different(Board2),

    writeln(Board), writeln(""),

    % block constraintss
    board_blocks(Board, Blocks),
    rows_different(Blocks),

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

% board_blocks(Board, Blocks): Blocks is a list of lists that represent the Board's sudoku blocks
board_blocks(Board, Blocks) <=>
    length(Blocks, N),
    length(Board, N),
    set_array_lengths(Blocks, N),
    sqrt(N, NN),
    SN is round(NN),
    board_blocks(Board, Blocks, SN, SN).
board_blocks(_, _, _, 0)<=> true.
board_blocks(Board, Blocks, SN, Count) <=>
    I is (SN-Count) * SN + 1,
    J is I + SN - 1,
    write("\nboard_blocks | Count: "), write(Count), write(", I: "), write(I), write(", J: "), writeln(J),
    write("   BLOCKS: "), writeln(Blocks),
    % take rows I to J of board
    L is J-I+1,
    length(Rows, L),
    take_elements(Board, I, J, Rows),
    % take blocks I to J of board
    writeln("Take elements for Blocks"),
    take_elements(Blocks, I, J, Blocks2),
    write("    partition rows:"), write(Rows), write(", into blocks: "), writeln(Blocks2),
    % put correct parts of the rows in the blocks
    rows_blocks(Rows, Blocks2, SN, SN),

    Count2 is Count-1,
    board_blocks(Board, Blocks, SN, Count2).

% rows_blocks(Rows, Blocks, SN, Count): takes certain parts of Rows in Rows and puts them into blcosk
rows_blocks([], _, _, _)<=> true.
rows_blocks([ _ | Rows ], Blocks, SN, 0) <=>
    rows_blocks(Rows, Blocks, SN, SN).

rows_blocks([ Row | Rows ], Blocks, SN, Count) <=>
    write("    rows_blocks | partition row:"), write(Row), write(", into blocks: "), writeln(Blocks),
    write("      SN: "), write(SN), write(", Count: "), writeln(Count),
    I is (SN - Count) * SN + 1,
    J is I + SN - 1,
    X is SN - Count + 1,
    write("      I: "), write(I), write(", J: "), write(J), write(", X: "), writeln(X),

    % take rows I to J of board
    take_elements(Row, I, J, Elements),
    % take blocks I to J of board
    nth1(X, Blocks, Block),
    write("      Elements: "), write(Elements), write(" in Block: "), writeln(Block),
    % Elements is in Block
    length(Blocks, N),
    length(Rows, L),
    Index is (N - L - 1) * SN + 1,
    nth1_list(Index, Block, Elements),

    Count2 is Count-1,
    rows_blocks([ Row | Rows ], Blocks, SN, Count2).

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

% take_first_elements(List1, I, List2): List2 contains [List1[1..I]]
take_first_elements(List1, N, List2) <=>
    take_elements(List1, 1, N, List2).

% take_elements(List1, I, J, List2): List2 contains [List1[I..J]]
take_elements(_, 1, 0, []) <=>
    true.

take_elements([X | Tail], 1, J, [X | Tail2]) <=> J > 0 |
    write("take_elements: "), writeln(X),
    JJ is J-1,
    %write("take_elements: "), writeln(X),
    take_elements(Tail, 1, JJ, Tail2).

take_elements([_ | Tail], I, J, List) <=> I > 1, J > 0 |
    write("take_elements2: "), writeln(Tail),
    write("I: "), write(I), write(" J: "), writeln(J),
    writeln(List),

    %I > 1,
    %J > 0,
    II is I-1,
    JJ is J-1,
    write("II: "), write(II), write(" JJ: "), writeln(JJ),
    take_elements(Tail, II, JJ, List).

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
