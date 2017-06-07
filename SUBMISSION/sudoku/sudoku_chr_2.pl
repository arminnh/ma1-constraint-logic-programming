:- use_module(library(chr)).
:- consult(boards).

:- chr_constraint solve_viewpoint_2/1, board_viewpoint_2/4, generate_known_board_facts_viewpoint_2/3.
:- chr_constraint generate_remaining_board_facts_viewpoint_2/1, generate_board_value_facts_viewpoint_2/2.
:- chr_constraint sn_viewpoint_2/1, n_viewpoint_2/1, domain_list_viewpoint_2/1.
:- chr_constraint print_board_viewpoint_2/2, print_board_viewpoint_2/0.
:- chr_constraint search_viewpoint_2/0, enum_viewpoint_2/1, clear_store_viewpoint_2/0.
:- chr_constraint in/2, do_diffs_viewpoint_2/0, diff_viewpoint_2/2, smart_diff_viewpoint_2/6.

:- op(700, xfx, in).

:- chr_option(debug,off).
:- chr_option(optimize,full).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SUDOKU SOLVER USING ALTERNATIVE VIEWPOINT
%   In the alternative viewpoint, we keep track of lists of postions for
%   sudoku values (e.g. Number 1 is on positions (1, 1), (2, 3), ...)
%   the Y values of the positions and the BlockIndex values are the search
%   variables in this viewpoint
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve_viewpoint_2(ProblemName) <=>
    statistics(walltime, [_ | [_]]),

    % get the sudoku board
    load_board(ProblemName, Board),

    % store N for later reuse = size of N*N
    length(Board, N),
    n_viewpoint_2(N),

    % store SN for later reuse = sqrt(N) = amount of sudoku blocks
    sqrt(N, NN),
    SN is round(NN),
    sn_viewpoint_2(SN),

    % create and store a list that contains the domain of the possible values on the board
    numlist(1, N, DL),
    reverse(DL, DomainList),
    domain_list_viewpoint_2(DomainList),

    % generate board_viewpoint_2(Value, Index, Y, BlockIndex) facts and domains for variables
    generate_known_board_facts_viewpoint_2(Board, 1, 1),
    generate_remaining_board_facts_viewpoint_2(N),
    writeln("Given board:"), print_board_viewpoint_2,

    % generate diff constraints
    do_diffs_viewpoint_2,
    writeln("Board before search:"), print_board_viewpoint_2,

    % start search for values
    search_viewpoint_2,
    writeln("Board after search:"), print_board_viewpoint_2,
    clear_store_viewpoint_2,

    statistics(walltime, [_ | [ExecutionTimeMS]]),
    write('Execution took '), write(ExecutionTimeMS), write(' ms.'), nl,
    ExTimeS is ExecutionTimeMS / 1000,
    write('Execution took '), write(ExTimeS), write(' s.'), nl,
    ExTimeM is ExTimeS / 60,
    write('Execution took '), write(ExTimeM), write(' min.'), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSTRAINT RULES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% amount of diffs: sum([1..N-1]) * N * 3
% the 3 is for: positions for values on different columns, posistions for values in different blocks no 2 values on same block

% all values in same blocks must be different, guards used to break symmetry

%% UNCOMMENT THIS FOR VERSION WITHOUT SMARTDIFFS
% do_diffs_viewpoint_2, board_viewpoint_2(Value, X1, Y1, BlockIndex1), board_viewpoint_2(Value, X2, Y2, BlockIndex2) ==> X1 < X2 |
%     diff_viewpoint_2(Y1,Y2), diff_viewpoint_2(BlockIndex1, BlockIndex2).

% DO DIFFS BETWEEN BOARD FACTS IN A SMART WAY
do_diffs_viewpoint_2, board_viewpoint_2(Value, X1, Y1, BlockIndex1), board_viewpoint_2(Value, X2, Y2, BlockIndex2) ==> X1 < X2 |
    smart_diff_viewpoint_2(X1, Y1, X2, Y2, BlockIndex1, BlockIndex2), diff_viewpoint_2(BlockIndex1, BlockIndex2).

do_diffs_viewpoint_2, board_viewpoint_2(Value1, X, Y1, _), board_viewpoint_2(Value2, X, Y2, _) ==> Value1 < Value2 |
    diff_viewpoint_2(Y1, Y2).

do_diffs_viewpoint_2 <=> true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RULES USED FOR DOMAIN SOLVING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% SMART DIFF RULES
% When the Y's are instantiated, they must be different
smart_diff_viewpoint_2(_, Y1, _, Y2, _, _) <=> nonvar(Y1), nonvar(Y2) | Y1 \== Y2.

% When a Y value is known, the accompanying BlockIndex is also known
% The Y value can then be removed from the other positions with the same block row
sn_viewpoint_2(SN), smart_diff_viewpoint_2(X1, Y1, X2, Y2, _, Block2) \ Y1 in L <=>
    nonvar(Y2), nonvar(Block2), same_block_row(X1, X2, SN), block_y_vals(Block2, SN, Columns),
    subtract(L, Columns, NL), L \== NL, length(NL, C1), C1 > 0 |
        Y1 in NL.
sn_viewpoint_2(SN), smart_diff_viewpoint_2(X1, Y1, X2, Y2, Block1, _) \ Y2 in L <=>
    nonvar(Y1), nonvar(Block1), same_block_row(X1, X2, SN), block_y_vals(Block1, SN, Columns),
    subtract(L, Columns, NL), L \== NL,length(NL,C1), C1 > 0  |
        Y2 in NL.

% if the block rows are different, then just remove this value from the domain
smart_diff_viewpoint_2(_, Y, _, X, _, _) \ X in L <=> nonvar(Y), select(Y, L, NL) | X in NL.
smart_diff_viewpoint_2(_, X, _, Y, _, _) \ X in L <=> nonvar(Y), select(Y, L, NL) | X in NL.

% NORMAL DIFF RULES
diff_viewpoint_2(X, Y) <=> nonvar(X), nonvar(Y) | X \== Y.
% domain solving for diff constraints
diff_viewpoint_2(Y, X) \ X in L <=> nonvar(Y), select(Y, L, NL) | X in NL.
diff_viewpoint_2(X, Y) \ X in L <=> nonvar(Y), select(Y, L, NL) | X in NL.

% enum_viewpoint_2(L): assigns values to variables X in L
enum_viewpoint_2(X)              <=> number(X) | true .
enum_viewpoint_2(X), X in Domain <=> member(X, Domain).

board_viewpoint_2(_, _, Y, _) \ Y in [D] <=> var(Y) | Y = D.

search_viewpoint_2, board_viewpoint_2(_, _, Y, _) ==> enum_viewpoint_2(Y).

% calculate the BlockIndex when a Y value is known
search_viewpoint_2, sn_viewpoint_2(SN), board_viewpoint_2(_, X, Y, BlockIndex) \ BlockIndex in _ <=> number(Y), var(BlockIndex) |
    BlockRow is ((X-1) // SN) + 1,
    BlockCol is ((Y-1) // SN) + 1,
    BlockIndex is (BlockRow-1) * SN + BlockCol.

search_viewpoint_2 <=> true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELPER RULES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% two different ways to load boards
load_board(ProblemName, Board) :-
    board(ProblemName, Board).
load_board(ProblemName, Board) :-
    puzzles(Board, ProblemName).

% generate_known_board_facts_viewpoint_2(Board, X, Y) generates board_viewpoint_2(Value, X, Y, BlockIndex)
% facts which will later be used to instert diff_viewpoint_2 rules into the constraint store
% got all values on the board
n_viewpoint_2(N) \ generate_known_board_facts_viewpoint_2(_, N2, _) <=> N2 is N+1 | true.
% after going over all columns, go to next row and start from column 1 again
n_viewpoint_2(N) \ generate_known_board_facts_viewpoint_2(Board, X, N2) <=> N2 is N+1, X2 is X + 1 |
    generate_known_board_facts_viewpoint_2(Board, X2, 1).
generate_known_board_facts_viewpoint_2(Board, X, Y) <=> nth1(X, Board, Row), nth1(Y, Row, Value), var(Value), Y2 is Y + 1 |
    generate_known_board_facts_viewpoint_2(Board, X, Y2).
sn_viewpoint_2(SN) \ generate_known_board_facts_viewpoint_2(Board, X, Y) <=> nth1(X, Board, Row), nth1(Y, Row, Value), nonvar(Value), Y2 is Y + 1 |
    % calculate block index
    BlockRow is ((X-1) // SN) + 1,
    BlockCol is ((Y-1) // SN) + 1,
    BlockIndex is (BlockRow-1) * SN + BlockCol,

    % generate the board fact
    board_viewpoint_2(Value, X, Y, BlockIndex),

    % go to the next case
    generate_known_board_facts_viewpoint_2(Board, X, Y2).

% generate_board_value_facts_viewpoint_2(Value, X) generates board facts for the sudoku number Value.
% e.g. board(1, 4, Y, BlockIndex) means that there is a 1 somewhere on row 4
generate_board_value_facts_viewpoint_2(_, 0) <=> true.
% if fact already exists on this X value for the Value, don't generate another one
board_viewpoint_2(Value, X, _, _) \ generate_board_value_facts_viewpoint_2(Value, X) <=> X2 is X - 1 |
    generate_board_value_facts_viewpoint_2(Value, X2).
% fact doesn't exist yet, create it
domain_list_viewpoint_2(Domain) \ generate_board_value_facts_viewpoint_2(Value, X) <=> X > 0, X2 is X - 1 |
    board_viewpoint_2(Value, X, Y, BlockIndex),
    Y in Domain,
    BlockIndex in Domain,
    generate_board_value_facts_viewpoint_2(Value, X2).

% generate_remaining_board_facts_viewpoint_2(Value) generates board facts for every possible
% value on the sudoku board. There are N different possible values on a N*N board
generate_remaining_board_facts_viewpoint_2(0) <=> true.
n_viewpoint_2(N) \ generate_remaining_board_facts_viewpoint_2(Value) <=> Value2 is Value - 1 |
    generate_board_value_facts_viewpoint_2(Value, N),
    generate_remaining_board_facts_viewpoint_2(Value2).

% rules for printing the board
print_board_viewpoint_2 <=> print_board_viewpoint_2(1, 1).
n_viewpoint_2(N) \ print_board_viewpoint_2(X, _) <=> X > N | nl.
n_viewpoint_2(N) \ print_board_viewpoint_2(X, Y) <=> Y > N, X2 is X + 1 |
    nl,
    print_board_viewpoint_2(X2, 1).
board_viewpoint_2(Value, X, Y, _) \ print_board_viewpoint_2(X, Y) <=> nonvar(Value), Y2 is Y + 1 |
    write(" "),
    write(Value),
    print_board_viewpoint_2(X, Y2).
 board_viewpoint_2(Value, X, Y, _) \ print_board_viewpoint_2(X, Y) <=> var(Value), Y2 is Y + 1 |
    write(" _"),
    print_board_viewpoint_2(X, Y2).
% If board_viewpoint_2 on this position doesn't exist.
print_board_viewpoint_2(X, Y2) <=>
    write(" _"),
    Y3 is Y2 + 1,
    print_board_viewpoint_2(X,Y3).

% clear the chr store after solving the puzzle
clear_store_viewpoint_2 \ board_viewpoint_2(_, _, _, _) <=> true.
clear_store_viewpoint_2 \ sn_viewpoint_2(_), n_viewpoint_2(_), domain_list_viewpoint_2(_) <=> true.
clear_store_viewpoint_2 <=> true.

% Checks if two block indices are on the same block row
same_block_row(Block1, Block2, SN):-
    B1 is Block1 - 1,
    B2 is Block2 - 1,
    R1 is div(B1, SN),
    R2 is div(B2, SN),
    R1 == R2.

% L contains the Y values of a block on a sudoku board
block_y_vals(Block, SN, L):-
    B is Block -1,
    R is mod(B, SN),
    Start is R * SN + 1,
    End is (R+1) * SN,
    numlist(Start, End, L),
    length(L, SN).
