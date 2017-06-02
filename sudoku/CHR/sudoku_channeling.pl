:- use_module(library(chr)).

:- chr_constraint solve/1, sudoku_channeling/1, print_board/1, print_numbers/1.
:- chr_constraint diff/2, enum/1, enum_board/0, upto/2, domain_list/1, make_domain/2, make_domains/1.
:- chr_constraint board/4.
:- chr_constraint generate_board_facts/3.
:- chr_constraint sn/1, n/1.
:- chr_constraint solve_other_viewpoint/2, sudoku_other_viewpoint/1, print_board_other_viewpoint/2.
:- chr_constraint enum_board_other_viewpoint/0.
:- chr_constraint board_other_viewpoint/4, generate_known_board_facts/3.
:- chr_constraint generate_remaining_board_facts/1, generate_board_value_facts/2.
:- chr_constraint do_diffs/0.
:- chr_constraint channel/0.
:- chr_constraint sudoku/1, sudoku_other/1, solve_classic/2, solve_channel/2.
:- chr_constraint incr_counter/0, count/1, change_count/1.

:- op(700, xfx, in).
:- op(700, xfx, le).
:- op(700, xfx, eq).
:- op(600, xfx, '..').
:- chr_constraint le/2, eq/2, in/2, add/3.
:- chr_option(debug,off).
:- chr_option(optimize,full).
:- consult(sudex_toledo).

:- chr_constraint experiments/0.

experiments <=>
	open('experiments.txt', write, Stream),
	write(Stream, "\\begin{table}[h!]
  \\begin{tabular}{|c|c|c|c|c|c|c|}
    \\hline
    \\multirow{1}{*}{Puzzle} &
      \\multicolumn{1}{L|}{Classical Viewpoint} &
      \\multicolumn{1}{L|}{Our Viewpoint} &
      \\multicolumn{1}{L|}{Channeling} \\\\
    & ms & ms & ms \\\\
    \\hline\n"),
	(   puzzles(P, lambda),
		writeln(lambda),

		% Classic viewpoint
		solve_classic(lambda, Time1),
		%solve_other_viewpoint(lambda, Time2),
		%solve_channel(lambda,Time3),
	    %writeln('Execution took '), write(ExTimeS), write(' s.'), nl,

	 	write(Stream, lambda),
		write(Stream, " & "),
		write(Stream, Time1),
		write(Stream, "s & "),
		write(Stream, Time2),
		write(Stream, "s & "),
		write(Stream, Time3),
		write(Stream, '\\\\'),
		write(Stream, "\n"),
		writeln(["finished", lambda])
    ;   true
    ),
	write(Stream," \\hline
  \\end{tabular}
\\end{table}"),
	writeln("Finished all"),
	close(Stream).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SUDOKU SOLUTION USING TRIVIAL VIEWPOINT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
solve_classic(ProblemName, ExTimeS) <=>
    % statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
    statistics(walltime, [_ | [_]]),

    % get the sudoku board
    (problem(ProblemName, Board) ; puzzles(Board, ProblemName)),
    %print_board(Board),
    count(0),
    % fill the sudoku board
    sudoku(Board),

    %writeln("\nResult:"),
    print_board(Board),
    %writeln(Board),

    % statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),
    statistics(walltime, [_ | [ExecutionTimeMS]]),
    %write('Execution took '), write(ExecutionTimeMS), write(' ms.'), nl,

    ExTimeS is ExecutionTimeMS / 1000,
    %write('Execution took '), write(ExTimeS), write(' s.'), nl,

    %ExTimeM is ExTimeS / 60,
    %write('Execution took '), write(ExTimeM), write(' min.'), nl,
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

    % set the domains of the possible values on the board
    make_domains(Board),

    % generate (X, Y, BlockIndex, Value) facts
    % those facts will later be used for insertion of diff(A, B) rules
    generate_board_facts(Board, 1, 1),

    % search for values
    enum_board,
    true.

solve_other_viewpoint(ProblemName, ExTimeS) <=>
    % statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
    statistics(walltime, [_ | [_]]),
    count(0),
    % get the sudoku board
    (problem(ProblemName, Board) ;  puzzles(Board, ProblemName)),

    % fill the sudoku board
    sudoku_other_viewpoint(Board),

    % writeln("\nResult:"),
    % print_board_other_viewpoint(1,1),

    % statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),
    statistics(walltime, [_ | [ExecutionTimeMS]]),
    %write('Execution took '), write(ExecutionTimeMS), write(' ms.'), nl,

    ExTimeS is ExecutionTimeMS / 1000,
    %write('Execution took '), write(ExTimeS), write(' s.'), nl,

    %ExTimeM is ExTimeS / 60,
    %write('Execution took '), write(ExTimeM), write(' min.'), nl,
    true.

sudoku_other_viewpoint(Board) <=>
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

    %print_board_other_viewpoint(1,1),

    % start search for values
    enum_board_other_viewpoint,
    true.

solve_channel(ProblemName, ExTimeS) <=>
    % statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
    statistics(walltime, [_ | [_]]),
    count(0),
    % get the sudoku board
    (problem(ProblemName, Board) ;  puzzles(Board, ProblemName)),
    %print_board(Board),

    % fill the sudoku board
    sudoku_channeling(Board),

    % writeln("\nResult:"),
    % print_board(Board),
    % print_board_other_viewpoint(1,1),
    % writeln(Board),

    % statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),
    statistics(walltime, [_ | [ExecutionTimeMS]]),
    %write('Execution took '), write(ExecutionTimeMS), write(' ms.'), nl,

    ExTimeS is ExecutionTimeMS / 1000,
    %write('Execution took '), write(ExTimeS), write(' s.'), nl,

    %ExTimeM is ExTimeS / 60,
    %write('Execution took '), write(ExTimeM), write(' min.'), nl,
    true.

sudoku_channeling(Board) <=>
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

    % set the domains of the possible values on the board
    make_domains(Board),

    % generate (X, Y, BlockIndex, Value) facts
    % those facts will later be used for insertion of diff(A, B) rules
    generate_board_facts(Board, 1, 1),

    generate_known_board_facts(Board, 1, 1),

    % set the domains of the possible values on the board
    generate_remaining_board_facts(N),

    % start generation of diffs
    do_diffs,
    %writeln("channel"),
    channel,
    % search for values
    %enum_board,
    enum_board_other_viewpoint,

    true.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Channeling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Viewpoint 1 (thus board) is  board(X,Y, BlockIndex, Value),
% Viewpoint 2 (thus board_other_viewpoint) is board(Value, X,Y, BlockIndex),

% The search variable for board is Value1
channel, board(X,Y, BlockIndex, Value)
    , board_other_viewpoint(Value, X, Y2, B2) ==> number(Value), var(Y2), var(B2) |
        Y2 is Y,
        B2 is BlockIndex.

% The search variable for board_other_viewpoint is the Y index
channel,board_other_viewpoint(Value, X, Y, BlockIndex),
    board(X,Y, BlockIndex, V2) ==> var(V2), number(Y), number(BlockIndex) |
        V2 is Value.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RULES USED FOR CONSTRAINTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

change_count(Val), count(N) <=> var(Val) |
    N2 is N -1,
    count(N2).

change_count(Val) <=> true.


% generate_board_facts(Board, X, Y) will generate board(X,Y, BlockIndex, Value)
% facts which will later be used to instert diff rules into the constraint store

% got all values on the board
n(N) \ generate_board_facts(_, X, _) <=> N2 is N+1, X == N2 |
    true.

% after going over all columns, go to next row and start from column 1 again
n(N) \ generate_board_facts(Board, X, Y) <=> N2 is N+1, Y == N2 |
    X2 is X + 1,
    generate_board_facts(Board, X2, 1).

sn(SN) \ generate_board_facts(Board, X, Y) <=>
    % get the value on position (X, Y) on the board
    nth1(X, Board, Row),
    nth1(Y, Row, Value),
    change_count(Value),
    % calculate block index
    XX is X-1,
    XXX is XX // SN,
    BlockRow is XXX + 1,

    YY is Y-1,
    YYY is YY // SN,
    BlockCol is YYY + 1,
    BlockIndex is (BlockRow-1) * SN + BlockCol,

    % save this data for later use
    board(X,Y, BlockIndex, Value),

    % go to the next case
    Y2 is Y + 1,
    generate_board_facts(Board, X, Y2).

% 9x9 board: 1458 diff rules -> 972 rules = sum([1..8]) * 9 * 2 + 5 * 9
%                                         = sum([1..N-1]) * N * SN

%% All these symmetry breaking things should go into the report
% all values in same columns must be different, guards used to break symmetry
board(X1, Y, _, Value1), board(X2, Y, _, Value2) ==> X1 < X2 |
    diff(Value1, Value2).

% all values in same rows must be different, guards used to break symmetry
board(X, Y1, _, Value1), board(X, Y2, _, Value2) ==> Y1 < Y2 |
    diff(Value1, Value2).

% all values in same blocks must be different, guards used to break symmetry
board(X1, Y1, BlockIndex, Value1), board(X2, Y2, BlockIndex, Value2) ==> X1 \== X2, Y1 \== Y2 |
     diff(Value1, Value2).

%board(_, Y1, BlockIndex, Value1), board(_, Y2, BlockIndex, Value2) ==> (Y1 < Y2) |
%    diff(Value1, Value2).
enum(X)              <=> number(X) | true .
enum(X), X in Domain <=> member(X, Domain), incr_counter.
incr_counter, count(N) <=>
    N2 is N +1,
    count(N2).

% X and Y are instantiated and are different
diff(X, Y) <=> nonvar(X), nonvar(Y) | X \== Y.
% Put improvement into report!
diff(Y, X) \ X in L <=> nonvar(Y), select(Y, L, NL) | X in NL.
diff(X, Y) \ X in L <=> nonvar(Y), select(Y, L, NL) | X in NL.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RULES USED FOR DOMAIN SOLVING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% enum(L): assigns values to variables X in L
% enum_board(Board): fills Board with values
board(_,_, _, Value), enum_board ==>
    enum(Value).

% upto(N, L): L = [1..N]
upto([], 0).
upto([ N | L ], N) :-
    N > 0,
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
%      OTHER VIEWPOINT
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RULES USED FOR CONSTRAINTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_board_value_facts(_, 0) <=>
    true.

% if fact already exists on this X value for the Value, don't generate another one
board_other_viewpoint(Value, Index, _, _) \ generate_board_value_facts(Value, Index) <=>
    Index2 is Index - 1,
    generate_board_value_facts(Value, Index2).

domain_list(Domain) \ generate_board_value_facts(Value, Index) <=> Index > 0 |
    board_other_viewpoint(Value, Index, Y, BlockIndex),
    Y in Domain,
    BlockIndex in Domain,
    Index2 is Index - 1,
    generate_board_value_facts(Value, Index2).

generate_remaining_board_facts(0) <=>
    true.

n(N) \ generate_remaining_board_facts(Value), count(Count) <=>
    N2 is Count - 1,
    count(N2),
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
    board_other_viewpoint(Value, X,Y, BlockIndex),

    % go to the next case
    Y2 is Y + 1,
    generate_known_board_facts(Board, X, Y2).


% amount of diffs: sum([1..N-1]) * N * 3
%     3 because: positions for values on different columns,
%                posistions for values in different blocks
%                no 2 values on same block

% all values in same blocks must be different, guards used to break symmetry
do_diffs, board_other_viewpoint(Value, X1, Y1, BlockIndex1), board_other_viewpoint(Value, X2, Y2, BlockIndex2) ==> X1 < X2 |
    diff(Y1,Y2), diff(BlockIndex1, BlockIndex2).

do_diffs, board_other_viewpoint(Value1, X, Y1, _), board_other_viewpoint(Value2, X, Y2, _) ==> Value1 < Value2 |
    diff(Y1,Y2).

% no need for symmetry breaking here as it's been done during construction
% diff(X, Y), diff(X, Y) <=> diff(X, Y).
% diff(Y, X), diff(X, Y) <=> diff(X, Y).


board_other_viewpoint(Val, X, Y, BlockIndex), enum_board_other_viewpoint ==>
    %writeln(["Enum", Val, X, Y, BlockIndex]),
    enum(Y).
    % enum(BlockIndex),

sn(SN), board_other_viewpoint(_, X, Y, BlockIndex), enum_board_other_viewpoint ==> number(Y), var(BlockIndex) |
    XX is X-1,
    XXX is XX // SN,
    BlockRow is XXX + 1,

    YY is Y-1,
    YYY is YY // SN,
    BlockCol is YYY + 1,
    BlockIndex is (BlockRow-1) * SN + BlockCol.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELPER RULES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

n(N) \ print_board_other_viewpoint(X,_) <=> X > N |
        true.

n(N) \ print_board_other_viewpoint(X,Y) <=> Y > N |
    X2 is X + 1,
    writeln(""),
    print_board_other_viewpoint(X2, 1).


board_other_viewpoint(Value, X, Y, _) \ print_board_other_viewpoint(X,Y) <=>  nonvar(Value) |
    write(" "),
    write(Value),
    Y2 is Y + 1,
    print_board_other_viewpoint(X,Y2).

 board_other_viewpoint(Value, X, Y, _) \ print_board_other_viewpoint(X,Y) <=> var(Value) |
    write(" _"),
    Y2 is Y + 1,
    print_board_other_viewpoint(X,Y2).

% If board on this position doesn't exist.
print_board_other_viewpoint(X,Y2) <=>
    write(" _"),
    Y3 is Y2 + 1,
    print_board_other_viewpoint(X,Y3).

% filter_list(DomainList, Row) :- filter_list(DomainList, Row, []).
%
%
% :- chr_constraint filter_list/3.
%
%
% filter_list(_, [], _) <=> true.
% filter_list(List, [ _ | Tail ], FilteredList) <=> nonvar(Head), select(Head, List, FilteredList) ,
%     filter_list(List, Tail, FilteredList).
% filter_list(List, [ _ | Tail ], FilteredList) <=>
%     filter_list(List, Tail, FilteredList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELPER RULES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_numbers([]) <=> writeln("").
print_numbers([ Number | Tail ]) <=> nonvar(Number) |
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
