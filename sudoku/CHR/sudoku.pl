:- use_module(library(chr)).

:- chr_constraint solve/1, sudoku/1, print_board/1, print_numbers/1.
:- chr_constraint diff/2, enum/1, enum_board/0, upto/2, domain_list/1, make_domain/2, make_domains/1.
:- chr_constraint board/4.
:- chr_constraint generate_board_facts/3.
:- chr_constraint sn/1, n/1, likely_number/4, create_likely_numbers/0, fix_domains/0.
:- chr_constraint domain_counter/5, add_counters/4.

:- op(700, xfx, in).
:- op(700, xfx, le).
:- op(700, xfx, eq).
:- op(600, xfx, '..').
:- chr_constraint le/2, eq/2, in/2, add/3.
:- chr_option(debug,off).
:- chr_option(optimize,full).
:- consult(sudex_toledo).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SUDOKU SOLUTION USING TRIVIAL VIEWPOINT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve(ProblemName) <=>
    % statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
    statistics(walltime, [_ | [_]]),

    % get the sudoku board
    (problem(ProblemName, Board) ; puzzles(Board, ProblemName)),
    print_board(Board),

    % fill the sudoku board
    sudoku(Board),

    writeln("\nResult:"),
    print_board(Board),
    writeln(Board),

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

    % set the domains of the possible values on the board
    make_domains(Board),

    % generate (X, Y, BlockIndex, Value) facts
    % those facts will later be used for insertion of diff(A, B) rules
    generate_board_facts(Board, 1, 1),

    % Heuristic: create likely numbers
    create_likely_numbers,

    % Fix the domains after this heuristic is finished
    fix_domains,
    % search for values
    writeln("pre search"),
    print_board(Board),

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
% the likely number needs to be something from the original domain
create_likely_numbers, board(X,Y, _, V1), V1 in D1 ==> var(V1)|
     %add_counters(V1,X,Y, D1).
     likely_number(V1,X,Y,D1).

% Count all the occurrences in list
% Thus the list [3,3,1] returns [[3,2], [1,1]]
count_occurrences(List, Occ):-
     findall([X,L], (bagof(true,member(X,List),Xs), length(Xs,L)), Occ).

% If there are two likely_numbers, take both lists and put into one bigger list
create_likely_numbers, V1 in D \ likely_number(V1,X,Y, R1), likely_number(V1,X,Y, R2)
    <=> flatten([R1|R2], R) |
    likely_number(V1,X,Y,R).


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


% Takes first elements of tupples
% For the list [[3,2], [1,1], [2,1]] this predicate returns [3,1,2]
% One exception though, if we have a count of 9 then the probability that this
% number should be here is 100%, so we return this number
take_first([] , []).
take_first( [ [V,C] | T ] , Result):-
    (C == 9 ->
        Result = [V]
    ;
        take_first(T, Result2),
        flatten([V|Result2], Result)
    )
    .

% Remove create_likely_numbers
fix_domains \ create_likely_numbers <=> true.

% Fixes the domains of the positions, sort the list according to likelihood
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
