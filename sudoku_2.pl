%
% ECLiPSe sample code - Sudoku problem
%
%	This is a puzzle, originating from Japan, where you have a
%	9x9 grid, consisting of 9 3x3 sub-grids. The challenge is
%	to fill the grid with numbers from 1 to 9 such that every row,
%	every column, and every 3x3 sub-grid contains the digits 1 to 9.
%	Some of these numbers are given, which is the way different
%	instances of the problem are made. The solution is usually unique.
%
%	Compile this file with ECLiPSe and call e.g.
%	:- solve(1).
%
% Author: Joachim Schimpf, IC-Parc
%



:- lib(ic).
:- import alldifferent/1 from ic_global.
:- lib(suspend).

solve(ProblemName) :-
	problem(ProblemName, Board),
	print_board(Board),
	sudoku(Board),
	labeling(Board),
	print_board(Board).

print_board(Board) :-
	dim(Board, [N,N]),
	( for(I,1,N), param(Board,N) do
	    ( for(J,1,N), param(Board,I) do
		X is Board[I,J],
		( var(X) -> write("  _") ; printf(" %2d", [X]) )
	    ), nl
	), nl.

sudoku(Board) :-
	dim(Board, [N2,N2]),
	N is integer(sqrt(N2)),
	Board[1..N2,1..N2] :: 1..N2,

	( for(I,1,N2), param(Board,N2) do
	    Row is Board[I,1..N2],
	    alldifferent(Row),
	    Col is Board[1..N2,I],
	    alldifferent(Col)
	),

	( multifor([I,J],1,N2,N), param(Board,N) do
	    ( multifor([K,L],0,N-1), param(Board,I,J), foreach(X,SubSquare) do
		X is Board[I+K,J+L]
	    ),
	    alldifferent(SubSquare)
	).

/*
Viewpoint(X, D)
Variables X: sets of rows, sets of columns, sets of blocks
Domain D: sets of values 1..N2

Another approach
Variables X: sets of number 1..N2
Domain D: set of positions

Values = array met N posities van arrays
         de N posities stellen de N mogelijke getallen voor die
         in cellen kunnen voorkomen
         de arrays zijn arrays van posities, elk getal komt even
         veel voor, dus elke array is even lang

werkwijze:
    declare Values variabele
    ken domain to aan values
    kopieer posities van Board naar Values
    pas de sudoku regels toe op Values
    zet de nieuwe berekende Values in Board
    done

    later wat verbeteren want dit lijkt me niet zo proper




dit probleem lijkt op het n-queens probleems:
    bepaalde nummers mogen andere nummers niet raken
    (bv 1 mag niet op zelfde kolom/rij als andere 1'en, maar
    mag wel met 2, 3, ...)

    misschien mogelijk om union van 9 viewpoints te gebruiken
    om sudoku voor te stellen?
*/

solve2(ProblemName) :-
    problem(ProblemName, Board),
    writeln("Given board:"),
	print_board(Board),
    writeln("Numbers of positions on a 9x9 board:"),
	dim(Board, [N,N]),
    ( for(I, 1,  N*N) do
        printf("%4d", [I]),
        ( mod(I, 9, 0) -> printf("\n", []) ; true)
    ),
    sudoku2(Board, Values),
    shallow_backtrack(Values),
	%shallow_backtrack2(Values),

    print_positions(Values).

shallow_backtrack2(List) :-
		  ( foreach(Var, List) do indomain(Var,Var)).

test2(Number) :-
    problem(Number, Board),
    solution2(Number, Positions),

    writeln('Problem:'),
    print_board(Board),
    writeln('Solution with other viewpoint:'),
    print_positions(Positions),

    writeln('Sudoku2:'),
    sudoku2(Board, Positions).

test12(Number) :-
	    problem(Number, Board),
	    solution12(Number, Positions),

	    writeln('Problem:'),
	    print_board(Board),
	    writeln('Solution with other viewpoint:'),
	    print_positions(Positions),

	    writeln('Sudoku2:'),
	    sudoku2(Board, Positions).

print_positions(Values) :-
    dim(Values, [N,N]),
    ( for(I, 1, N), param(Values, N) do
        write(I),
        write(" ->"),
        ( for(J, 1, N), param(Values, I) do
            X is Values[I, J],
            ( var(X) -> write(" _") ; printf("%4d", [X]) )
        ), nl
    ), nl.

sudoku2(Board, NumbersPositions) :-
    % dimensions of board = N by N and there are N possible numbers to be used on the Board
    % Board has sqrt(N) blocks of N numbers (with dim sqrt(N) by sqrt(N))
    dim(Board, [N, N]),

    % declare an array of arrays. each distinct number on the board gets an array.
    % each number is mapped to an array of positions where this number goes
    dim(NumbersPositions, [N, N]),
    NumbersPositions[1..N, 1..N] :: 1..N*N,

    % assign known positions to values in given board
    ( multifor([Row, Col], 1, N), param(N, Board, NumbersPositions) do
        Pos is (Row-1) * N + Col,

		% Number is a number or "_"
		Number is Board[Row, Col],

		( var(Number) ->
            % if Board has a "_" in this position, do nothing (= true)
            true
            ;
            % else if Board has a number in this position, the position needs
            % to be in the list NumbersPositions[Number]

            % get the array of positions for number as a list
            PositionsList is NumbersPositions[Number, 1..N],

            % let Pos be a member of the list of positions of number Number
            member(Pos, PositionsList)
        )
    ),

	% TODO: improve these constraints to add actual sudoku logic
     ( for(Number, 1, N), param(NumbersPositions, N) do

          % positions of a certain Number
  		Positions is NumbersPositions[Number],
  		/*(for(I, 1, N), param(Positions, N, Number) do
			PosI #= Positions[I],
			/*PosI #= N1*Q1 + R1,
			0 #=< R1,
			R1 #< N1,
			Q1 #>= 0,

			X #= PosI -1,
			R1 #= X mod N,
			Q1 #= X // N,
  			(for(J, I+1, N), param(Positions, I, N, Number) do
                  N1 is N+1,

                  % make each position be on a different row
                  % By definition of integer division, A mod B is the number Y
                  % such that Y + Q*B = A and such that Y is between 0 and B-1 (for some
                  % integer Q, usually called "quotient").
				 PosJ #= Positions[J],

                  /*PosJ #= N1*Q2 + R2,
                  0 #=< R2,
                  R2 #< N1,
                  Q2 #>= 0,

				 Y #= PosJ -1 ,
				 R2 #= Y mod N,
				 Q2 #= Y // N,
  				 %R1 #\= R2,
				 Q1 $\= Q2
				 %QI is PosI// N+1,
				 %QJ is PosJ// N+1,
				 %QI #\= QJ,

				 %writeln(R1),
				 %writeln(R2)
				 %nl
                  %printf("I: %d, J: %d \n",  [I, J])
  			)
  		),*/
		separate_rows(Positions),
		%shallow_backtrack(Positions),
		alldifferent(Positions)
		%shallow_backtrack(Positions)
	),

      % positions cannot be reused
  	  alldifferent(NumbersPositions),
	  %shallow_backtrack(NumbersPositions),

      writeln("end sudoku2").

separate_rows(Positions):-
	dim(Positions, N),
	(for(I, 1, N), param(Positions, N) do
		%PosI #= Positions[I],
		X #= Positions[I] - 1,
		R1 #= X mod N,
		Q1 #= X // N,
		(for(J, 1, I-1), param(Positions, I, N) do
			 %PosJ #= Positions[J],
			 Y #= Positions[J] - 1 ,
			 R2 #= Y mod N,
			 Q2 #= Y // N,
			 Q1 $\= Q2
		)
	).

shallow_backtrack(List) :-
	  ( foreach(Var, List) do get_min(Var,Var)).

struct_to_list(Struct, List):-
	  ( foreacharg(Arg, Struct),
	  foreach(Var,List)
	  do
	  Var = Arg
	  ).
/*
1   2   3   4   5   6   7   8   9
10  11  12  13  14  15  16  17  18
19  20  21  22  23  24  25  26  27
28  29  30  31  32  33  34  35  36
37  38  39  40  41  42  43  44  45
46  47  48  49  50  51  52  53  54
55  56  57  58  59  60  61  62  63
64  65  66  67  68  69  70  71  72
73  74  75  76  77  78  79  80  81

*/

% sudoku_shift_rows :- replace a board positions by the first position of the row those
%                      positions are on in a 9x9 sudoku board.
% example:
% ?- sudoku_shift_rows([2, 15, 22, 36, 44, 23, 12, 54, 3], 9, [1, 10, 19, 28, 37, 19, 10, 46, 1])
% >  Yes (0.00s cpu, solution 1, maybe more)
sudoku_shift_rows([], _, []).

% If X is the first position of a row
sudoku_shift_rows([X | Tail], N, [X | Tail2]) :-
    X #= N*Y + 1,
    0 #=< Y,
    Y #=< N,
    sudoku_shift_rows(Tail, N, Tail2).

% recursive case, if we're not the first position of a row then subtract 1 from current position
sudoku_shift_rows([X | Tail], N, [X2 | Tail2]) :-
    X #> X2,
	% was previously is but since X might not be instantiated yet it is safer to use #=
	% see http://www.swi-prolog.org/pldoc/man?section=clpfd-integer-arith for more detail
    XX #= X-1,
    sudoku_shift_rows([XX | Tail], N, [X2 | Tail2]).


% sudoku_shift_cols :- replace a board positions by the first position of the column
%                      those positions are on in a 9x9 sudoku board.
% example:
% ?- sudoku_shift_cols([2, 15, 22, 36, 44, 23, 12, 54, 3], 9, [2, 6, 4, 9, 8, 5, 3, 9, 3])
% >  Yes (0.00s cpu, solution 1, maybe more)
sudoku_shift_cols([], _, []).

sudoku_shift_cols([X | Tail], N, [X | Tail2]) :-
    X #=< N,
    X #> 0,
    sudoku_shift_cols(Tail, N, Tail2).

sudoku_shift_cols([X | Tail], N, [X2 | Tail2]) :-
    X #> X2,
	% was previously is but since X might not be instantiated yet it is safer to use #=
	% see http://www.swi-prolog.org/pldoc/man?section=clpfd-integer-arith for more detail
    XX #= X-N,
    sudoku_shift_cols([XX | Tail], N, [X2 | Tail2]).

/*
Solution for 1:
    3  6  2  8  4  5  1  7  9
    1  7  5  9  6  3  2  4  8
    9  4  8  2  1  7  6  3  5
    7  1  3  4  5  8  9  6  2
    2  9  6  7  3  1  5  8  4
    8  5  4  6  2  9  7  1  3
    4  3  9  5  7  6  8  2  1
    5  2  7  1  8  4  3  9  6
    6  8  1  3  9  2  4  5  7

NumbersPositions array (our other viewpoint) then should be:
Value 1 goes in positions: 7 10 23 29 42 53 63 67 75
Value 2 goes in positions: 3 16 22 36 37 50 62 65 78
Value 3 goes in positions: 1 15 23 30 41 54 56 70 76
Value 4 goes in positions: 5 17 20 31 45 48 55 69 79
Value 5 goes in positions: 6 12 27 32 43 47 58 64 80
Value 6 goes in positions: 2 14 25 35 39 49 60 72 73
Value 7 goes in positions: 8 11 24 27 40 52 59 66 81
Value 8 goes in positions: 4 18 21 33 44 46 61 68 74
Value 9 goes in positions: 9 13 19 34 38 51 57 71 77
*/

% code based on http://eclipseclp.org/examples/queens_simple.ecl.txt
sudoqueens_arrays(N, Board, Value) :-
	%dim(Board, [N]),
	write(Value),
	Board[1..N, 1..N] :: 1..N,

    ( for(I,1,N), param(Board, Value, N) do
        ( for(J,I+1,N), param(Board, Value, I) do
            Board[I] == Value -> (
                Board[I] #\= Board[J],
                Board[I] #\= Board[J]+J-I,
                Board[I] #\= Board[J]+I-J
            )
        )
    )
	%Board =.. [_|Vars], % CLPArray =.. [_|PrologList],
	%labeling(Vars).
    .

%----------------------------------------------------------------------
% Sample data
%----------------------------------------------------------------------

problem(1, [](
    [](_, _, 2, _, _, 5, _, 7, 9),
    [](1, _, 5, _, _, 3, _, _, _),
    [](_, _, _, _, _, _, 6, _, _),
    [](_, 1, _, 4, _, _, 9, _, _),
    [](_, 9, _, _, _, _, _, 8, _),
    [](_, _, 4, _, _, 9, _, 1, _),
    [](_, _, 9, _, _, _, _, _, _),
    [](_, _, _, 1, _, _, 3, _, 6),
    [](6, 8, _, 3, _, _, 4, _, _))).

problem(2, [](
    [](_, _, 3, _, _, 8, _, _, 6),
    [](_, _, _, 4, 6, _, _, _, _),
    [](_, _, _, 1, _, _, 5, 9, _),
    [](_, 9, 8, _, _, _, 6, 4, _),
    [](_, _, _, _, 7, _, _, _, _),
    [](_, 1, 7, _, _, _, 9, 5, _),
    [](_, 2, 4, _, _, 1, _, _, _),
    [](_, _, _, _, 4, 6, _, _, _),
    [](6, _, _, 5, _, _, 8, _, _))).

problem(3, [](
    [](_, _, _, 9, _, _, _, _, _),
    [](_, _, 7, _, 6, _, 5, _, _),
    [](_, _, 3, 5, _, _, _, 7, 9),
    [](4, _, 5, _, _, 9, _, _, 1),
    [](8, _, _, _, _, _, _, _, 7),
    [](1, _, _, 6, _, _, 9, _, 8),
    [](6, 4, _, _, _, 8, 7, _, _),
    [](_, _, 9, _, 1, _, 2, _, _),
    [](_, _, _, _, _, 7, _, _, _))).

problem(4, [](
    [](_, 5, _, _, _, 1, 4, _, _),
    [](2, _, 3, _, _, _, 7, _, _),
    [](_, 7, _, 3, _, _, 1, 8, 2),
    [](_, _, 4, _, 5, _, _, _, 7),
    [](_, _, _, 1, _, 3, _, _, _),
    [](8, _, _, _, 2, _, 6, _, _),
    [](1, 8, 5, _, _, 6, _, 9, _),
    [](_, _, 2, _, _, _, 8, _, 3),
    [](_, _, 6, 4, _, _, _, 7, _))).

% Problems 5-8 are harder, taken from
% http://www2.ic-net.or.jp/~takaken/auto/guest/bbs46.html
problem(5, [](
    [](_, 9, 8, _, _, _, _, _, _),
    [](_, _, _, _, 7, _, _, _, _),
    [](_, _, _, _, 1, 5, _, _, _),
    [](1, _, _, _, _, _, _, _, _),
    [](_, _, _, 2, _, _, _, _, 9),
    [](_, _, _, 9, _, 6, _, 8, 2),
    [](_, _, _, _, _, _, _, 3, _),
    [](5, _, 1, _, _, _, _, _, _),
    [](_, _, _, 4, _, _, _, 2, _))).

problem(6, [](
    [](_, _, 1, _, 2, _, 7, _, _),
    [](_, 5, _, _, _, _, _, 9, _),
    [](_, _, _, 4, _, _, _, _, _),
    [](_, 8, _, _, _, 5, _, _, _),
    [](_, 9, _, _, _, _, _, _, _),
    [](_, _, _, _, 6, _, _, _, 2),
    [](_, _, 2, _, _, _, _, _, _),
    [](_, _, 6, _, _, _, _, _, 5),
    [](_, _, _, _, _, 9, _, 8, 3))).

problem(7, [](
    [](1, _, _, _, _, _, _, _, _),
    [](_, _, 2, 7, 4, _, _, _, _),
    [](_, _, _, 5, _, _, _, _, 4),
    [](_, 3, _, _, _, _, _, _, _),
    [](7, 5, _, _, _, _, _, _, _),
    [](_, _, _, _, _, 9, 6, _, _),
    [](_, 4, _, _, _, 6, _, _, _),
    [](_, _, _, _, _, _, _, 7, 1),
    [](_, _, _, _, _, 1, _, 3, _))).

problem(8, [](
    [](1, _, 4, _, _, _, _, _, _),
    [](_, _, 2, 7, 4, _, _, _, _),
    [](_, _, _, 5, _, _, _, _, _),
    [](_, 3, _, _, _, _, _, _, _),
    [](7, 5, _, _, _, _, _, _, _),
    [](_, _, _, _, _, 9, 6, _, _),
    [](_, 4, _, _, _, 6, _, _, _),
    [](_, _, _, _, _, _, _, 7, 1),
    [](_, _, _, _, _, 1, _, 3, _))).

% this one is from http://www.skyone.co.uk/programme/pgefeature.aspx?pid=48&fid=129
problem(9, [](
    [](5, _, 6, _, 2, _, 9, _, 3),
    [](_, _, 8, _, _, _, 5, _, _),
    [](_, _, _, _, _, _, _, _, _),
    [](6, _, _, 2, 8, 5, _, _, 9),
    [](_, _, _, 9, _, 3, _, _, _),
    [](8, _, _, 7, 6, 1, _, _, 4),
    [](_, _, _, _, _, _, _, _, _),
    [](_, _, 4, _, _, _, 3, _, _),
    [](2, _, 1, _, 5, _, 6, _, 7))).

% BBC Focus magazine October 2005
problem(10, [](
    [](_, 6, _, 3, 2, _, _, 7, _),
    [](4, 7, _, _, _, _, _, 3, 2),
    [](_, _, _, 9, _, _, 1, 4, 6),
    [](2, 4, _, 8, _, _, _, _, _),
    [](_, _, 8, _, _, _, 2, _, 1),
    [](1, _, _, _, _, 2, _, _, _),
    [](_, _, 2, 4, 7, 6, 8, _, _),
    [](6, 8, 9, _, _, _, _, 5, 4),
    [](_, _, _, _, 8, _, _, _, _))).

problem(11, [](
    [](1, 8, 2, 7, 5, _, 3, _, 9),
    [](9, 5, 6, _, 3, _, _, 8, _),
    [](3, 4, 7, _, _, 9, _, 5, _),
    [](2, _, 3, _, 4, _, _, 9, 8),
    [](4, _, 8, 9, _, 2, 5, _, 3),
    [](5, 7, 9, 3, 6, 8, 1, 2, 4),
    [](_, 2, _, 4, 9, _, 8, 3, _),
    [](_, 3, _, _, 2, _, 9, _, 5),
    [](_, 9, _, _, _, 3, _, 1, _))).

/*
1, 2, 3
4, 5, 6
7, 8, 9

1 -> 1, A, B
2 -> 7, C, D
3 -> 3, 4, E

als we voor 1 checken:
(1-1) mod  3 (=0) \= (A-1) mod 3 => Dus A mag al zeker ni in de eerste kolom
(1-1) mod  3 (=0) \= (B-1) mod 3 => Dus B mag al zeker ni in de eerste kolom
(A-1) mod  3 \= (B-1) mod 3 => Dus A mag niet in dezelfde kolom zijn als B

(1-1) // 3 (=0) \= (A-1)// 3 => A mag niet in de eerste rij
(1-1) // 3 (=0) \= (B-1)// 3 => B mag niet in eerste rij
(A-1) // 3 \= (B-1)// 3 => A mag niet in dezelfde rij als B

...
Mogelijk antwoord
1 -> 1, 5, 9
2 -> 7, 2, 6
3 -> 3, 4, 8
*/
problem(12,[](
	[](1, _, 3),
	[](3, _, _),
	[](2, _, _))).

solution12(1, [](
	[](1, 5, 9),
	[](2, 6, 7),
	[](3, 4, 8))).

solution(1, [](
    [](3,  6,  2,  8,  4,  5,  1,  7,  9),
    [](1,  7,  5,  9,  6,  3,  2,  4,  8),
    [](9,  4,  8,  2,  1,  7,  6,  3,  5),
    [](7,  1,  3,  4,  5,  8,  9,  6,  2),
    [](2,  9,  6,  7,  3,  1,  5,  8,  4),
    [](8,  5,  4,  6,  2,  9,  7,  1,  3),
    [](4,  3,  9,  5,  7,  6,  8,  2,  1),
    [](5,  2,  7,  1,  8,  4,  3,  9,  6),
    [](6,  8,  1,  3,  9,  2,  4,  5,  7))).

solution2(1, [](
    [](7, 10, 23, 29, 42, 53, 63, 67, 75),
    [](3, 16, 22, 36, 37, 50, 62, 65, 78),
    [](1, 15, 23, 30, 41, 54, 56, 70, 76),
    [](5, 17, 20, 31, 45, 48, 55, 69, 79),
    [](6, 12, 27, 32, 43, 47, 58, 64, 80),
    [](2, 14, 25, 35, 39, 49, 60, 72, 73),
    [](8, 11, 24, 27, 40, 52, 59, 66, 81),
    [](4, 18, 21, 33, 44, 46, 61, 68, 74),
    [](9, 13, 19, 34, 38, 51, 57, 71, 77))).
