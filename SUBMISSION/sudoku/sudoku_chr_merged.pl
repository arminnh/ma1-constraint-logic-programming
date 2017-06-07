:- use_module(library(chr)).
:- consult(boards).

:- chr_constraint solve1/1, solve1/2, board/4, generate_board_facts/3, clear_store/0.
:- chr_constraint sn/1, n/1, domain_list/1, print_board/1, print_numbers/1.
:- chr_constraint search/0, enum/1, make_domain/2, make_domains/1.
:- chr_constraint likely_number/4, create_likely_numbers/0, fix_domains/0.
:- chr_constraint in/2, diff/2.

:- chr_constraint solve2/1, solve2/2, board_viewpoint_2/4, generate_known_board_facts_viewpoint_2/3.
:- chr_constraint generate_remaining_board_facts_viewpoint_2/1, generate_board_value_facts_viewpoint_2/2.
:- chr_constraint sn_viewpoint_2/1, n_viewpoint_2/1, domain_list_viewpoint_2/1.
:- chr_constraint print_board_viewpoint_2/2, print_board_viewpoint_2/0.
:- chr_constraint search_viewpoint_2/0, enum_viewpoint_2/1, clear_store_viewpoint_2/0.
:- chr_constraint do_diffs_viewpoint_2/0, diff_viewpoint_2/2, smart_diff_viewpoint_2/6.

:- chr_constraint channel/0, experiments/0, solve3/1, solve3/2, sudoku_channeling/1.
:- chr_constraint do_board/2, clear_constraints/0.

:- op(700, xfx, in).

:- chr_option(debug,off).
:- chr_option(optimize,full).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MERGED CHR SOLVERS FOR CHANNELING CONSTRAINTS AND EXPERIMENTS
%   more comments can be found in the other chr files
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 3 DIFFERENT SUDOKU SOLVERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% solver using the classic viewpoint
solve1(ProblemName) <=> solve1(ProblemName, _).
solve1(ProblemName, ExTimeS) <=>
    statistics(walltime, [_ | [_]]),

    % get the sudoku board
    load_board(ProblemName, Board),
    writeln("Given board:"), print_board(Board),
    % store N for later reuse = size of N*N board
    length(Board, N), n(N),
    % store SN for later reuse = sqrt(N) = amount of sudoku blocks
    sqrt(N, NN), SN is round(NN), sn(SN),

    % create and store a list that contains the domain of the possible values on the board
    numlist(1, N, DL),
    reverse(DL, DomainList),
    domain_list(DomainList),
    % set the domains of the possible values on the board
    make_domains(Board),
    % generate board(X, Y, BlockIndex, Value) facts to put constraints on
    generate_board_facts(Board, 1, 1),
    % Heuristic: create likely numbers
    create_likely_numbers,
    % Fix the domains after this heuristic is finished
    fix_domains,

    % search for values
    writeln("Board before search:"), print_board(Board),
    search,
    writeln("Board after search:"), print_board(Board),
    clear_store,

    statistics(walltime, [_ | [ExecutionTimeMS]]),
    write('Execution took '), write(ExecutionTimeMS), write(' ms.'), nl,
    ExTimeS is ExecutionTimeMS / 1000,
    write('Execution took '), write(ExTimeS), write(' s.'), nl,
    ExTimeM is ExTimeS / 60,
    write('Execution took '), write(ExTimeM), write(' min.'), nl.

% solver using the alternative viewpoint
solve2(ProblemName) <=> solve2(ProblemName, _).
solve2(ProblemName, ExTimeS) <=>
    statistics(walltime, [_ | [_]]),

    % get the sudoku board
    load_board(ProblemName, Board),
    % store N for later reuse = size of N*N
    length(Board, N), n_viewpoint_2(N),
    % store SN for later reuse = sqrt(N) = amount of sudoku blocks
    sqrt(N, NN), SN is round(NN), sn_viewpoint_2(SN),

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

% solver using both viewpoints with channeling constraints
solve3(ProblemName) <=> solve3(ProblemName, _).
solve3(ProblemName, ExTimeS) <=>
    statistics(walltime, [_ | [_]]),

    % get the sudoku board
    load_board(ProblemName, Board),
    writeln("Given board:"), print_board(Board),
    % store N for later reuse = size of N*N board
    length(Board, N), n(N), n_viewpoint_2(N),
    % store SN for later reuse = sqrt(N) = amount of sudoku blocks
    sqrt(N, NN), SN is round(NN), sn(SN), sn_viewpoint_2(SN),

    % create and store a list that contains the domain of the possible values on the board
    numlist(1, N, DL),
    reverse(DL, DomainList),
    domain_list(DomainList), domain_list_viewpoint_2(DomainList),
    % set the domains of the possible values on the board
    make_domains(Board),
    % generate board(X, Y, BlockIndex, Value) facts to put constraints on
    generate_board_facts(Board, 1, 1),
    % generate board_viewpoint_2(Value, Index, Y, BlockIndex) facts and domains for variables
    generate_known_board_facts_viewpoint_2(Board, 1, 1),
    generate_remaining_board_facts_viewpoint_2(N),

    % Heuristic: create likely numbers
    create_likely_numbers,
    % Fix the domains after this heuristic is finished
    fix_domains,
    % generate diff constraints
    do_diffs_viewpoint_2,

	% generate channeling constraints
    channel,

    % search for values
    writeln("Board before search:"), print_board(Board),
    search,
    search_viewpoint_2,
    writeln("Board of classic viewpoint after search:"), print_board(Board),
    writeln("Board of alternative viewpoint after search:"), print_board_viewpoint_2,
    clear_store,
    clear_store_viewpoint_2,
    clear_constraints,

    statistics(walltime, [_ | [ExecutionTimeMS]]),
    write('Execution took '), write(ExecutionTimeMS), write(' ms.'), nl,
    ExTimeS is ExecutionTimeMS / 1000,
    write('Execution took '), write(ExTimeS), write(' s.'), nl,
    ExTimeM is ExTimeS / 60,
    write('Execution took '), write(ExTimeM), write(' min.'), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CHANNELING CONSTRAINTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% if the value is known in the classic viewpoint, we can insert this into the alternative viewpoint
channel, board(X, Y, BlockIndex, Value), board_viewpoint_2(Value, X, Y2, B2)
    ==> number(Value), var(Y2), var(B2) |
        Y2 = Y,
        B2 = BlockIndex.

% For board_viewpoint_2, the Y index is the search variable
% if the value is known in the alternative viewpoint, we can insert this into the classic viewpoint
channel, board_viewpoint_2(Value, X, Y, BlockIndex), board(X, Y, BlockIndex, V2)
    ==> var(V2), number(Y), number(BlockIndex) |
        V2 = Value.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSTRAINT RULES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% CLASSIC VIEWPOINT
% all values in same columns must be different, guards used to break symmetry
board(X1, Y, _, Value1), board(X2, Y, _, Value2) ==> X1 < X2 |
    diff(Value1, Value2).
% all values in same rows must be different, guards used to break symmetry
board(X, Y1, _, Value1), board(X, Y2, _, Value2) ==> Y1 < Y2 |
    diff(Value1, Value2).
% all values in same blocks must be different, guards used to break symmetry
board(X1, Y1, BlockIndex, Value1), board(X2, Y2, BlockIndex, Value2) ==> X1 \== X2, Y1 \== Y2 |
    diff(Value1, Value2).

% Put domain in likely number. The likely number needs to be something from the original domain
create_likely_numbers, board(X, Y, _, V1), V1 in D1 ==> var(V1) | likely_number(V1, X, Y, D1).

% If there are two likely_numbers for a certain position, merge both lists
create_likely_numbers, V1 in _ \ likely_number(V1, X, Y, R1), likely_number(V1, X, Y, R2)
    <=> flatten([ R1 | R2 ], R) |
    likely_number(V1, X, Y, R).

% For all the elems in a block, take the difference in their domains. Create likely numbers with this
% The idea is that if one number is not in the domain of the other one,
% it is very likely that the current position needs to take that number
create_likely_numbers, board(X1, Y1, B, V1), V1 in D1, board(_, _, B, V2)
    ==> number(V2), var(V1), subtract(D1, [V2], R) |
    likely_number(V1, X1, Y1, R).

create_likely_numbers, board(X1, Y1, B, V1), V1 in D1, board(_, _, B, V2), V2 in D2
    ==> var(V1), subtract(D1, D2, R), intersection(R, D1, Result), length(R, C), C > 0 |
    likely_number(V1, X1, Y1, Result).

% Remove create_likely_numbers
fix_domains \ create_likely_numbers <=> true.
% Fixes the domains of the positions, sort the list according to likelihood
fix_domains \ likely_number(V,_, _, D), V in _
    <=> count_occurrences(D, Occ), sort(2, @>=, Occ, S), take_first(S,Result) |
    V in Result.

fix_domains <=> true.

% ALTERNATIVE VIEWPOINT
% DO DIFFS BETWEEN BOARD FACTS IN A SMART WAY
do_diffs_viewpoint_2, board_viewpoint_2(Value, X1, Y1, BlockIndex1), board_viewpoint_2(Value, X2, Y2, BlockIndex2)
    ==> X1 < X2 |
    smart_diff_viewpoint_2(X1, Y1, X2, Y2, BlockIndex1, BlockIndex2), diff_viewpoint_2(BlockIndex1, BlockIndex2).

do_diffs_viewpoint_2, board_viewpoint_2(Value1, X, Y1, _), board_viewpoint_2(Value2, X, Y2, _)
    ==> Value1 < Value2 |
    diff_viewpoint_2(Y1, Y2).

do_diffs_viewpoint_2 <=> true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RULES USED FOR DOMAIN SOLVING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% CLASSIC VIEWPOINT
% X and Y are instantiated and are different
diff(X, Y) <=> nonvar(X), nonvar(Y) | X \== Y.
% domain solving for diff constraints
diff(Y, X) \ X in L <=> nonvar(Y), select(Y, L, NL) | X in NL.
diff(X, Y) \ X in L <=> nonvar(Y), select(Y, L, NL) | X in NL.

% enum(L): assigns values to variables X in L
enum(X)              <=> number(X) | true.
enum(X), X in Domain <=> member(X, Domain).

board(_, _, _, V) \ V in [D] <=> var(V) | V = D.

search, board(_, _, _, Value) ==> enum(Value).
search <=> true.

% ALTERNATIVE VIEWPOINT
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

% generate_board_facts(Board, X, Y) generates board(X,Y, BlockIndex, Value)
% facts which will be used to insert diff rules into the constraint store
% got all values on the board
n(N) \ generate_board_facts(_, N2, _) <=> N2 is N+1 | true.
% after going over all columns, go to next row and start from column 1 again
n(N) \ generate_board_facts(Board, X, N2) <=> N2 is N+1, X2 is X + 1 |
    generate_board_facts(Board, X2, 1).
sn(SN) \ generate_board_facts(Board, X, Y) <=> Y2 is Y + 1 |
    % get the value on position (X, Y) on the board
    nth1(X, Board, Row),
    nth1(Y, Row, Value),

    % calculate block index
    BlockRow is ((X-1) // SN) + 1,
    BlockCol is ((Y-1) // SN) + 1,
    BlockIndex is (BlockRow-1) * SN + BlockCol,

    % generate the board fact
    board(X,Y, BlockIndex, Value),

    % go to the next case
    generate_board_facts(Board, X, Y2).

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

% make_domain(L, D): create 'X in D' constraints for all variables X in L
make_domain([ Val | Tail ], DomainList) <=> var(Val) |
    Val in DomainList,
    make_domain(Tail, DomainList).
make_domain([ _ | Tail ], DomainList) <=>
    make_domain(Tail, DomainList).
make_domain([], _) <=> true.

% make_domains(L): L is an list of N elements, make_domains creates 'X in [1..N]' constraints
domain_list(DomainList) \ make_domains([ Row | Tail ]) <=>
    list_remove_vars(Row, NewRow),
    !, % cut to remove backtracking back into list_remove_vars (it will only find a different premutation of NewRow?)
    % Domain list is 1..N, NewRow are the values on a specific Row
    subtract(DomainList, NewRow, SmallerDomainList),
    make_domain(Row, SmallerDomainList),
    make_domains(Tail).
make_domains([]) <=> true.

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
clear_store \ board(_, _, _, _) <=> true.
clear_store \ sn(_), n(_), domain_list(_) <=> true.
clear_store <=> true.
clear_store_viewpoint_2 \ board_viewpoint_2(_, _, _, _) <=> true.
clear_store_viewpoint_2 \ sn_viewpoint_2(_), n_viewpoint_2(_), domain_list_viewpoint_2(_) <=> true.
clear_store_viewpoint_2 <=> true.

list_remove_vars([], []).
list_remove_vars([ Head | Tail1 ], FilteredList) :-
    var(Head),
    list_remove_vars(Tail1, FilteredList).
list_remove_vars([ Head | Tail1 ], [ Head | Tail2 ]) :-
    list_remove_vars(Tail1, Tail2).

% Count all the occurrences in list. e.g. the list [3,3,1] returns [[3,2], [1,1]]
count_occurrences(List, Occ):-
    findall([X,L], (bagof(true,member(X,List),Xs), length(Xs,L)), Occ).

% Takes first elements of tuples
% e.g. for [[3,2], [1,1], [2,1]] this predicate returns [3,1,2]
% One exception though, if there is a count of 9, then the probability that this
% number should be here is 100%, so we return this number
take_first([], []).
take_first([ [V, 9] | _ ], Result):-
    Result = [V].
take_first([ [V, _] | T ], Result):-
    take_first(T, Result2),
    flatten([ V | Result2 ], Result).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RULES USED TO RUN EXPERIMENTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    do_board(Name, Stream) <=>
        puzzles(_, Name),
        writeln(Name),
        % classic viewpoint
        solve1(Name, Time1),
    	writeln(["Classic", Time1]), clear_constraints,

        solve2(Name, Time2),
    	writeln(["other viewpoint", Time2]), clear_constraints,
        solve3(Name,Time3),

    	writeln(["channel", Time3]), clear_constraints,

        write(Stream, Name),
        write(Stream, " & "),
        write(Stream, Time1),
        write(Stream, "s & "),
        write(Stream, Time2),
        write(Stream, "s & "),
        write(Stream, Time3),
        write(Stream, 's \\\\'),
        write(Stream, "\n"),
        writeln(["finished", Name]).

    experiments <=>
    	open('experiments.txt', write, Stream),
    	write(Stream, "\\begin{table}[h!]
      \\begin{tabular}{|c|c|c|c|c|c|c|}
        \\hline
        \\multirow{1}{*}{Puzzle} &
          \\multicolumn{1}{L|}{Classical Viewpoint} &
          \\multicolumn{1}{L|}{Alternative viewpoint} &
          \\multicolumn{1}{L|}{Channeling} \\\\
        & ms & ms & ms \\\\
        \\hline\n"),
        do_board(lambda, Stream), clear_constraints,
        do_board(hard17, Stream), clear_constraints,
        do_board(eastermonster, Stream), clear_constraints,
        do_board(tarek_052, Stream), clear_constraints,
        do_board(goldennugget, Stream), clear_constraints,
        do_board(coloin, Stream), clear_constraints,
        do_board(extra2, Stream), clear_constraints,
        do_board(extra3, Stream), clear_constraints,
        do_board(extra4, Stream), clear_constraints,
        do_board(inkara2012, Stream), clear_constraints,
        do_board(clue18, Stream), clear_constraints,
        do_board(clue17, Stream), clear_constraints,
        do_board(sudowiki_nb28, Stream), clear_constraints,
        do_board(sudowiki_nb49, Stream), clear_constraints,

    	write(Stream," \\hline
      \\end{tabular}
    \\end{table}"),
    	writeln("Finished all"),
    	close(Stream).

    % Clear all the constraints from the constraint store
    clear_constraints \ _ in _ <=> true.
    clear_constraints \ channel <=> true.
    clear_constraints <=> true.
