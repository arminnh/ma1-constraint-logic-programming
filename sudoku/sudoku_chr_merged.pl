:- use_module(library(chr)).

:- chr_constraint solve/2, sudoku/1, print_board/1, print_numbers/1.
:- chr_constraint diff/2, enum/1, enum_board/0, upto/2, domain_list/1, make_domain/2, make_domains/1.
:- chr_constraint board/4.
:- chr_constraint generate_board_facts/3.
:- chr_constraint sn/1, n/1, likely_number/4, create_likely_numbers/0, fix_domains/0.
:- chr_constraint domain_counter/5, add_counters/4.

:- chr_constraint solve_viewpoint2/2, sudoku_viewpoint2/1, print_board_viewpoint2/2.
:- chr_constraint smart_diff_viewpoint2/6, enum_board_viewpoint2/0.
:- chr_constraint board_viewpoint2/4, generate_known_board_facts_viewpoint2/3.
:- chr_constraint generate_remaining_board_facts_viewpoint2/1, generate_board_value_facts_viewpoint2/2.
:- chr_constraint do_diffs_viewpoint2/0.

:- chr_constraint channel/0.
:- chr_constraint experiments/0.
:- chr_constraint solve_channel/2, sudoku_channeling/1.

:- op(700, xfx, in).
:- op(700, xfx, le).
:- op(700, xfx, eq).
:- op(600, xfx, '..').
:- chr_constraint le/2, eq/2, in/2, add/3.
:- chr_option(debug,off).
:- chr_option(optimize,full).
:- chr_constraint do_board/2.
:- consult(boards).
:- chr_constraint clear_constraints/0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% MERGED FILE FOR CHANNELING AND EXPERIMENTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
    do_board(lambda, Stream),
    clear_constraints,
    do_board(hard17, Stream),
    clear_constraints,
    do_board(eastermonster, Stream),
    clear_constraints,
    do_board(tarek_052, Stream),
    clear_constraints,
    do_board(goldennugget, Stream),
    clear_constraints,
    do_board(coloin, Stream),
    clear_constraints,
    do_board(extra2, Stream),
    clear_constraints,
    do_board(extra3, Stream),
    clear_constraints,

    do_board(extra4, Stream),    % extra4 = extra3 + 1 extra hint
    clear_constraints,

    %puzzles(P) :- fin(P).   % is the same as inkara2012
    do_board(inkara2012, Stream),
    clear_constraints,

    do_board(clue18, Stream),
    clear_constraints,

    do_board(clue17, Stream),
    clear_constraints,

    do_board(sudowiki_nb28, Stream),
    clear_constraints,

    do_board(sudowiki_nb49, Stream),
    clear_constraints,

	write(Stream," \\hline
  \\end{tabular}
\\end{table}"),
	writeln("Finished all"),
	close(Stream).

% Clear all the constraints from the constraint store
clear_constraints\ board(_,_,_,_) <=> true.
clear_constraints\ board_viewpoint2(_,_,_,_) <=> true.
clear_constraints\ diff(_,_) <=> true.
clear_constraints\ smart_diff_viewpoint2(_,_,_,_,_,_) <=> true.
clear_constraints\ V in D <=> true.
clear_constraints\ n(N) <=> true.
clear_constraints\ sn(N) <=> true.
clear_constraints\ enum_board_viewpoint2 <=> true.
clear_constraints\ enum_board <=> true.
clear_constraints\ create_likely_numbers <=> true.
clear_constraints\ fix_domains <=> true.
clear_constraints\ do_diffs_viewpoint2 <=> true.
clear_constraints\ channel <=> true.
clear_constraints \ domain_list(N) <=> true.

clear_constraints <=> true.

do_board(Name, Stream) <=>
    puzzles(P, Name),
    writeln(Name),

    % Classic viewpoint
    solve(Name, Time1),
	writeln(["Classic", Time1]),
	clear_constraints,
    solve_viewpoint2(Name, Time2),
	writeln(["other viewpoint", Time2]),
	clear_constraints,
    solve_channel(Name,Time3),
	writeln(["channel", Time3]),
	clear_constraints,
    %writeln('Execution took '), write(ExTimeS), write(' s.'), nl,

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SOLVES for experiments
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve(ProblemName, ExTimeS) <=>
    % statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
    statistics(walltime, [_ | [_]]),

    % get the sudoku board
    puzzles(Board, ProblemName),
    %print_board(Board),

    % fill the sudoku board
    sudoku(Board),

    % writeln("\nResult:"),
    % print_board(Board),
    % writeln(Board),

    % statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),
    statistics(walltime, [_ | [ExecutionTimeMS]]),
    %write('Execution took '), write(ExecutionTimeMS), write(' ms.'), nl,

    ExTimeS is ExecutionTimeMS / 1000,
    %write('Execution took '), write(ExTimeS), write(' s.'), nl,

    ExTimeM is ExTimeS / 60,
    %write('Execution took '), write(ExTimeM), write(' min.'), nl,
    true.


solve_viewpoint2(ProblemName, ExTimeS) <=>
    % statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
    statistics(walltime, [_ | [_]]),

    % get the sudoku_viewpoint2 board_viewpoint2
    puzzles(Board, ProblemName),
    %writeln("GOT BOARD"),
    % fill the sudoku_viewpoint2 board_viewpoint2
    sudoku_viewpoint2(Board),

    %writeln("\nResult:"),
    %print_board_viewpoint2(1,1),

    % statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),
    statistics(walltime, [_ | [ExecutionTimeMS]]),
    %write('Execution took '), write(ExecutionTimeMS), write(' ms.'), nl,

    ExTimeS is ExecutionTimeMS / 1000,
    %write('Execution took '), write(ExTimeS), write(' s.'), nl,

    ExTimeM is ExTimeS / 60,
    %write('Execution took '), write(ExTimeM), write(' min.'), nl,
    true.

solve_channel(ProblemName, ExTimeS) <=>
    % statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
    statistics(walltime, [_ | [_]]),
    % get the sudoku board
    puzzles(Board, ProblemName),
    %print_board(Board),

    % fill the sudoku board
    sudoku_channeling(Board),

    %writeln("\nResult:"),
    %print_board(Board),
    %print_board_viewpoint2(1,1),
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

    generate_known_board_facts_viewpoint2(Board, 1, 1),

    % set the domains of the possible values on the board
    generate_remaining_board_facts_viewpoint2(N),
	% Create heuristics
    create_likely_numbers,
    fix_domains,
    % start generation of diffs
    do_diffs_viewpoint2,
	% Insert channel fact for channeling constraints
    channel,
    % search for values
    enum_board,
    %enum_board_viewpoint2,
	%writeln("finished channel"),
    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CHANNELING CONSTRAINTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% If the value is known from first viewpoint
% We can insert this into our other viewpoint
channel, board(X,Y, BlockIndex, Value)
    , board_viewpoint2(Value, X, Y2, B2) ==> number(Value), var(Y2), var(B2) |
        Y2 is Y,
        B2 is BlockIndex.

% The search variable for board_viewpoint2 is the Y index
% If the value is known from the other viewpoint
% We can insert this into our classical viewpoint
channel,board_viewpoint2(Value, X, Y, BlockIndex),
    board(X,Y, BlockIndex, V2) ==> var(V2), number(Y), number(BlockIndex) |
        V2 is Value.

% IF MORE COMMENTS IS NEEDED CHECK INDIVDUAL FILES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% VIEWPOINT 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    create_likely_numbers,
    fix_domains,
    % search for values
    %writeln("pre search"),
    %print_board(Board),

    enum_board,%(Board),
    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RULES USED FOR CONSTRAINTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

% All these symmetry breaking things should go into the report
%all values in same columns must be different, guards used to break symmetry
board(X1, Y, _, Value1), board(X2, Y, _, Value2) ==> X1 < X2 |
    diff(Value1, Value2).

% all values in same rows must be different, guards used to break symmetry
board(X, Y1, _, Value1), board(X, Y2, _, Value2) ==> Y1 < Y2 |
    diff(Value1, Value2).

% % all values in same blocks must be different, guards used to break symmetry
board(X1, Y1, BlockIndex, Value1), board(X2, Y2, BlockIndex, Value2) ==> X1 \== X2, Y1 \== Y2 |
     diff(Value1, Value2).

%board(_, Y1, BlockIndex, Value1), board(_, Y2, BlockIndex, Value2) ==> (Y1 < Y2) |
%    diff(Value1, Value2).


% X and Y are instantiated and are different
diff(X, Y) <=> nonvar(X), nonvar(Y) | X \== Y.
% Put improvement into report!
diff(Y, X) \ X in L <=> nonvar(Y), select(Y, L, NL) | X in NL.
diff(X, Y) \ X in L <=> nonvar(Y), select(Y, L, NL) | X in NL.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RULES USED FOR DOMAIN SOLVING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Special case, add counter with 0, this will be used to sort the domain list
% Special case, add counter with 1, this will keep the sorted domain list
% add_counters(V,X,Y, []) <=>
%     domain_counter(V,X,Y, 0, 9),
%     domain_counter(V,X,Y, -1, []).
%
% add_counters(V,X,Y, [H|T]) <=>
%     domain_counter(V,X,Y, H, 1),
%     add_counters(V,X,Y,T).


% Put domain in likely number
% the likely number needs to be something from the domain
create_likely_numbers, board(X,Y, _, V1), V1 in D1 ==> var(V1)|
     %add_counters(V1,X,Y, D1).
     likely_number(V1,X,Y,D1).

count_occurrences(List, Occ):-
     findall([X,L], (bagof(true,member(X,List),Xs), length(Xs,L)), Occ).

% If there are two likely_numbers, take union of the both and check interesection
create_likely_numbers, V1 in D \ likely_number(V1,X,Y, R1), likely_number(V1,X,Y, R2)
    <=> flatten([R1|R2], R) |
    likely_number(V1,X,Y,R).
%
% %likely_number(V, []) <=> true.
% % For all the elems in a block, take the difference in their domains
% % Create likely numbers with this
% % The idea is that if one number is not in the domain of the other one,
% % it is very likely that the current pos needs to take that number
create_likely_numbers, board(X1,Y1, B, V1), V1 in D1, board(_,_, B, V2) ==> number(V2), var(V1), subtract(D1, [V2], R) |
    likely_number(V1,X1,Y1,R).

create_likely_numbers, board(X1,Y1, B, V1), V1 in D1, board(_,_, B, V2), V2 in D2 ==>
    var(V1), subtract(D1,D2,R), intersection(R,D1,Result), length(R,C), C > 0 |
    %writeln([V1, X2,Y2, D1, D2, R ]),
    likely_number(V1,X1,Y1, Result).

% enum(L): assigns values to variables X in L
enum(X)              <=> number(X) | true .
enum(X), X in Domain <=> member(X, Domain).


take_first([] , []).
take_first( [ [V,C] | T ] , Result):-
    (C == 9 ->
        %writeln("99999999999"),
        Result = [V]
    ;
        take_first(T, Result2),
        flatten([V|Result2], Result)
    )
    .


fix_domains \ create_likely_numbers <=> true.

fix_domains \ likely_number(V,X,Y,D), V in Dom <=>
    count_occurrences(D, Occ),
    sort(2, @>=, Occ, S),
    take_first(S,Result),
    %writeln([V,X,Y, Dom, Result, Occ]),
    V in Result.

enum_board \ fix_domains <=> true.

board(_,_,_, Value) \ Value in [X] <=> var(Value) |
    Value is X.

board(_,_, _, Value), enum_board ==>
    enum(Value).
    % enum(BlockIndex),


% % enum(L): assigns values to variables X in L
% enum([])                        <=> true.
% enum([ X | Tail ])              <=> number(X) | enum(Tail).
% enum([ X | Tail ]), X in Domain <=> member(X, Domain), enum(Tail).
%
% % enum_board(Board): fills Board with values
% enum_board([]) <=> true.
% enum_board([ Row | Rows ]) <=>
%     enum(Row),
%     enum_board(Rows).

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
    %writeln([DomainList, Row, NewRow, SmallerDomainList]),

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% VIEWPOINT 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SUDOKU SOLVER USING NUMBERS HAVE POSITION VIEWPOINT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sudoku_viewpoint2(Board) <=>
    % store N for later reuse = size of N*N board_viewpoint2
    length(Board, N),
    n(N),

    % store SN for later reuse = sqrt(N) = amount of sudoku_viewpoint2 blocks
    sqrt(N, NN),
    SN is round(NN),
    sn(SN),

    % create and store a list that contains the domain of the possible values on the board_viewpoint2
    upto(DomainList, N),
    domain_list(DomainList),

    % generate (X, Y, BlockIndex, Value) facts
    % those facts will later be used for insertion of diff_viewpoint2(A, B) rules
    generate_known_board_facts_viewpoint2(Board, 1, 1),

    % set the domains of the possible values on the board_viewpoint2
    generate_remaining_board_facts_viewpoint2(N),

    % start generation of diffs
    do_diffs_viewpoint2,

    %print_board_viewpoint2(1,1),

    % start search for values
    enum_board_viewpoint2,
    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RULES USED FOR CONSTRAINTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate_board_value_facts_viewpoint2(_, 0) <=>
    true.

% if fact already exists on this X value for the Value, don't generate another one
board_viewpoint2(Value, Index, _, _) \ generate_board_value_facts_viewpoint2(Value, Index)<=>
    Index2 is Index - 1,
    generate_board_value_facts_viewpoint2(Value, Index2).

domain_list(Domain) \ generate_board_value_facts_viewpoint2(Value, Index) <=> Index > 0 |
    board_viewpoint2(Value, Index, Y, BlockIndex),
    Y in Domain,
    BlockIndex in Domain,
    Index2 is Index - 1,
    generate_board_value_facts_viewpoint2(Value, Index2).

generate_remaining_board_facts_viewpoint2(0) <=>
    true.

n(N) \ generate_remaining_board_facts_viewpoint2(Value) <=>

    generate_board_value_facts_viewpoint2(Value, N),
    Value2 is Value - 1,
    generate_remaining_board_facts_viewpoint2(Value2).


% generate_known_board_facts_viewpoint2(Board, X, Y) will generate board_viewpoint2(X,Y, BlockIndex, Value)
% facts which will later be used to instert diff_viewpoint2 rules into the constraint store

% got all values on the board_viewpoint2
n(N) \ generate_known_board_facts_viewpoint2(_, X, _) <=> N2 is N+1, X == N2 |
    true.

% after going over all columns, go to next row and start from column 1 again
n(N) \ generate_known_board_facts_viewpoint2(Board, X, Y) <=> N2 is N+1, Y == N2 |
    X2 is X + 1,
    generate_known_board_facts_viewpoint2(Board, X2, 1).

generate_known_board_facts_viewpoint2(Board, X, Y) <=>  nth1(X, Board, Row), nth1(Y, Row, Value), var(Value) |
    Y2 is Y + 1,
    generate_known_board_facts_viewpoint2(Board, X, Y2).

sn(SN) \ generate_known_board_facts_viewpoint2(Board, X, Y) <=> nth1(X, Board, Row), nth1(Y, Row, Value), nonvar(Value) |
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
% do_diffs_viewpoint2, board_viewpoint2(Value, X1, Y1, BlockIndex1), board_viewpoint2(Value, X2, Y2, BlockIndex2) ==> X1 < X2 |
%     diff_viewpoint2(Y1,Y2), diff_viewpoint2(BlockIndex1, BlockIndex2).

do_diffs_viewpoint2, board_viewpoint2(Value, X1, Y1, BlockIndex1), board_viewpoint2(Value, X2, Y2, BlockIndex2) ==> X1 < X2 |
    smart_diff_viewpoint2(X1, Y1, X2, Y2, BlockIndex1, BlockIndex2), diff(BlockIndex1, BlockIndex2).

do_diffs_viewpoint2, board_viewpoint2(Value1, X, Y1, _), board_viewpoint2(Value2, X, Y2, _) ==> Value1 < Value2 |
    diff(Y1,Y2).

% no need for symmetry breaking here as it's been done during construction
% diff_viewpoint2(X, Y), diff_viewpoint2(X, Y) <=> diff_viewpoint2(X, Y).
% diff_viewpoint2(Y, X), diff_viewpoint2(X, Y) <=> diff_viewpoint2(X, Y).

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
smart_diff_viewpoint2(_,Y1, _,  Y2, _, _) <=> nonvar(Y1), nonvar(Y2) | Y1 \== Y2.
sn(SN), smart_diff_viewpoint2(X1, Y1, X2, Y2, _, Block2) \ Y1 in L <=>
    nonvar(Y2), nonvar(Block2), same_block_row(X1, X2, SN),
    block_y_vals(Block2, SN, Columns), subtract(L, Columns, NL), L \== NL, length(NL,C1), C1 > 0 | Y1 in NL.

sn(SN), smart_diff_viewpoint2(X1, Y1, X2, Y2, Block1, _) \ Y2 in L <=>
    nonvar(Y1), nonvar(Block1), same_block_row(X1, X2, SN),
    block_y_vals(Block1, SN, Columns), subtract(L, Columns, NL), L \== NL,length(NL,C1), C1 > 0  | Y2 in NL.

smart_diff_viewpoint2(_, Y, _,  X, _, _) \ X in L <=> nonvar(Y), select(Y, L, NL) | X in NL.
smart_diff_viewpoint2(_,X,_, Y,_,_) \ X in L <=> nonvar(Y), select(Y, L, NL) | X in NL.


% diff_viewpoint2(X, Y) <=> nonvar(X), nonvar(Y) | X \== Y.
% diff_viewpoint2(Y, X) \ X in L <=> nonvar(Y), select(Y, L, NL) | X in NL.
% diff_viewpoint2(X, Y) \ X in L <=> nonvar(Y), select(Y, L, NL) | X in NL.

% enum_viewpoint2(L): assigns values to variables X in L
% enum_viewpoint2(X)              <=> number(X) | true .
% enum_viewpoint2(X), X in Domain <=> member(X, Domain).

board_viewpoint2(_,_,_, B) \ B in [D] <=> var(B) |
    B is D.

board_viewpoint2(_,_,Y, _) \ Y in [D] <=> var(Y) |
    Y is D.

board_viewpoint2(_, _, Y, _), enum_board_viewpoint2 ==>
    enum(Y).
    % enum_viewpoint2(BlockIndex),

sn(SN), board_viewpoint2(_, X, Y, BlockIndex) \ BlockIndex in D <=> number(Y), var(BlockIndex) |
    XX is X-1,

    XXX is XX // SN,

    BlockRow is XXX + 1,
    YY is Y-1,

    YYY is YY // SN,

    BlockCol is YYY + 1,

    BlockIndex is (BlockRow-1) * SN + BlockCol.

% % upto(N, L): L = [1..N]
% upto([], 0).
% upto([ N | L ], N) :-
%     N > 0,
%     N1 is N-1,
%     upto(L, N1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELPER RULES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

n(N) \ print_board_viewpoint2(X,_) <=> X > N |
        true.

n(N) \ print_board_viewpoint2(X,Y) <=> Y > N |
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
