%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GIVEN SUDOKU SOLUTION WITH TRIVIAL VIEWPOINT
%
% Viewpoint(X, D)
% Variables X: sets of rows, sets of columns, sets of blocks
% Domain D: sets of values 1..N2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

solve(ProblemName) :-
	problem(ProblemName, Board),
	print_board(Board),
	sudoku(Board),
	labeling(Board),
	print_board(Board).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OUR SUDOKU SOLUTION WITH ANOTHER VIEWPOINT
%
% Viewpoint(X, D)
% Variables X: sets of positions
% Domain D: set of values 1..N*N
%
% Values:
%     array of N arrays of N values
%     each possible number in the sudoku problem gets an array
%     each array for a number contains positions of the sudoku board that that number lies on
%     all of the numbers appear an equal amount of times, so each array has equal length
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve2(ProblemName) :-
    problem(ProblemName, Board),

    writeln("Given board:"),
	print_board(Board),

    writeln("Numbers of positions on a 9x9 board:"),
	dim(Board,[N,N]),
    ( for(I, 1,  N*N), param(N) do
        printf("%4d", [I]),
        ( mod(I, N, 0) -> printf("\n", []) ; true)
    ),

    writeln('Sudoku2:'),
    sudoku2(Board, Values),


    %labeling(Values),
	search(naive,Values),
    print_positions(Values).

search(naive,List) :-
		search(List,0,input_order,indomain,complete, []).


sudoku2(Board, NumbersPositions) :-
    % dimensions of board = N by N and there are N possible numbers to be used on the Board
    % Board has sqrt(N) blocks of N numbers (with dim sqrt(N) by sqrt(N))
    dim(Board, [N, N]),

    % declare an array of arrays. each distinct number on the board gets an array.
    % each number is mapped to an array of positions where this number goes
    dim(NumbersPositions, [N, N,2]),
    NumbersPositions[1..N, 1..N,1..2] :: 1..N,
    % assign known positions to values in given board
    ( multifor([Row, Col], 1, N), param(N, Board, NumbersPositions) do
		% Number is a number or "_"
		Number is Board[Row, Col],

		( var(Number) ->
            % if Board has a "_" in this position, do nothing (= true)
            true
            ;
            % else if Board has a number in this position, the position needs
            % to be in the list NumbersPositions[Number]

            % get the array of positions for number as a list
            RowsList is NumbersPositions[Number, 1..N, 1],
			ColsList is NumbersPositions[Number, 1..N, 2],

            % let Pos be a member of the list of positions of number Number
            member(Row, RowsList),
			member(Col, ColsList)
        )
    ),
    ( for(Number, 1, N), param(NumbersPositions, N) do
        % positions of a certain Number
		%PositionsList is NumbersPositions[Number, 1..N,1..2],
		RowsList is NumbersPositions[Number, 1..N, 1],
		alldifferent(RowsList),
		ColsList is NumbersPositions[Number, 1..N, 2],
		alldifferent(ColsList)
    ),
	M is N*N,
	dim(PosList, [M]),
	PosList[1..M] :: 1..M,
	PosList2 is PosList[1..M],
	( for(Number, 1, N), param(Values, N, PosList2) do
		(for(I, 1, N), param(Values, PosList2, N, Number) do
			Row #= NumbersPositions[Number, I, 1],
			Col #= NumbersPositions[Number, I, 2],
			writeln(Row),
			Pos #= (Row-1) * N + Col
			%member(Pos, PosList2)
		)
	),
	%alldifferent(PosList),
	search(naive,NumbersPositions),

	%writeln(NumbersPositions),
    % positions cannot be reused

    writeln("end sudoku2").

test2(Number) :-
    problem(Number, Board),
    solution2(Number, Positions),

    writeln('Problem:'),
    print_board(Board),

    writeln('Solution with other viewpoint:'),
    print_positions(Positions),

    writeln('Sudoku2:'),
    sudoku2(Board, Positions).

sudoku3(Board):-
	dim(Board, [N,N]),
	dim(Blocks, [N,N]),
	Blocks[1..N,1..N] :: 1..N
	.

print_board(Board) :-
    dim(Board, [N,N]),
    ( for(I,1,N), param(Board,N) do
        ( for(J,1,N), param(Board,I) do
        X is Board[I,J],
        ( var(X) -> write("  _") ; printf(" %2d", [X]) )
        ), nl
    ), nl.

print_positions(Values) :-
    dim(Values, [N,N, 2]),
    ( for(I, 1, N), param(Values, N) do
        write(I),
        write(" ->"),
        ( for(J, 1, N), param(Values, I) do
            X is Values[I, J, 1],
			Y is Values[I, J, 2],
            ( var(X) -> write(" _") ; printf("(%2d, %2d)", [X,Y]) )
        ), nl
    ), nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SAMPLE DATA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

	problem(12,[](
		[](1, _, 3),
		[](3, _, _),
		[](2, _, _))).

	problem(13,[](
			[](1, _, 3, _),
			[](3, _, _, 4),
			[](2, _, _, _),
			[](4, _, _, _))).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SOLUTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%  Solution for problem 1 in the trivial viewpoint:
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

% Solution for problem 1 in our other viewpoint:
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
