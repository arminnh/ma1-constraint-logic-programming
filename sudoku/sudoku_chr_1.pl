:- use_module(library(chr)).
:- consult(boards).

:- chr_constraint solve/1, board/4, generate_board_facts/3, clear_store/0.
:- chr_constraint sn/1, n/1, domain_list/1, print_board/1, print_numbers/1.
:- chr_constraint search/0, enum/1, make_domain/2, make_domains/1.
:- chr_constraint likely_number/4, create_likely_numbers/0, fix_domains/0.
:- chr_constraint in/2, diff/2.

:- op(700, xfx, in).

:- chr_option(debug,off).
:- chr_option(optimize,full).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SUDOKU SOLVER USING TRIVIAL VIEWPOINT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve(Problem) <=>
    statistics(walltime, [_ | [_]]),

    % get the sudoku board
    load_board(Problem, Board),
    writeln("Given board:"), print_board(Board),

    % store N for later reuse = size of N*N board
    length(Board, N),
    n(N),
    % store SN for later reuse = sqrt(N) = amount of sudoku blocks
    sqrt(N, NN),
    SN is round(NN),
    sn(SN),

    % create and store a list that contains the domain of the possible values on the board
    numlist(1, N, DL),
    reverse(DL, DomainList),
    domain_list(DomainList),

    % set the domains of the possible values on the board
    make_domains(Board),
    writeln("make domains"),

    % generate board(X, Y, BlockIndex, Value) facts to put constraints on
    generate_board_facts(Board, 1, 1),

    writeln("generate_board_facts:"),
    % Heuristic: create likely numbers
    create_likely_numbers,
    writeln("create_likely_numbers"),
    % Fix the domains after this heuristic is finished
    fix_domains,
    writeln("fix_domains:"),

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSTRAINT RULES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% amount of diffs for an N*N board: sum([1..N-1]) * N * SN
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
    %writeln([Occ,S, Result]),
    V in Result.

fix_domains <=> true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RULES USED FOR DOMAIN SOLVING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% X and Y are instantiated and are different
diff(X, Y) <=> nonvar(X), nonvar(Y) | X \== Y.
% domain solving for diff constraints
diff(Y, X) \ X in L <=> nonvar(Y), select(Y, L, NL) | X in NL.
diff(X, Y) \ X in L <=> nonvar(Y), select(Y, L, NL) | X in NL.

% enum(L): assigns values to variables X in L
enum(X)              <=> number(X) | true.
enum(X), X in Domain <=> member(X, Domain).

X in [D] <=> var(X) | X = D.

search, board(_,_, _, Value) ==> enum(Value).
search <=> true.

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

% clear the chr store after solving the puzzle
clear_store \ board(_, _, _, _) <=> true.
clear_store \ sn(_), n(_), domain_list(_) <=> true.
clear_store <=> true.

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
