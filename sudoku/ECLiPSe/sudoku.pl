% TODO: change construction of NumbersPositions again:
%       do not need to keep X coords as it will always be the same as the Position index
%       so dimensions can become [N, N] again and only the Y coords need to be kept explicitly

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
:- import alldifferent/1, sorted/2 from ic_global.
:- coroutine.
% :- lib(lists).
:- import nth1/3 from listut.

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
% OUR SUDOKU SOLUTION AND TESTING WITH ANOTHER VIEWPOINT
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

% findall(_, solve2(13), Sols), length(Sols, N), writeln(["amount of solutions: ", N]).

solve2(ProblemName) :-
    problem(ProblemName, Board),

    writeln("Given board:"),
	print_board(Board),

    writeln('Sudoku2:'),
    % set up variables and their constraints
    sudoku2(Board, NumbersPositions),

    % do search on variables
	search(naive, NumbersPositions),

    % print results
    writeln("Sudoku2 done:"),
    print_positions(NumbersPositions),

    writeln("Converted back to sudoku board:"),
    numbers_positions_to_board(NumbersPositions, Board2),
    print_board(Board2),

    writeln("Given board again for checking:"),
	print_board(Board).

sudoku2(Board, NumbersPositions) :-
    % dimensions of board = N by N and there are N possible numbers to be used on the Board
    % Board has sqrt(N) blocks of N numbers (with dim sqrt(N) by sqrt(N))
    dim(Board, [N, N]),

    % declare an array of arrays. each distinct number on the board gets an array.
    % each number is mapped to an array of positions where this number goes
    % the positions are arrays of length 2 with represent the (x, y) coordinates
    dim(NumbersPositions, [N, N, 2]),
    NumbersPositions[1..N, 1..N, 1..2] :: 1..N,

    % assign known positions in given board to NumbersPositions
    board_to_numbers_positions(Board, NumbersPositions),

    % set sudoku constraints
    sudoku_constraints(NumbersPositions, N).

% should give true if the problem and solution exist
test2(Number) :-
    writeln('Problem:'),
    problem(Number, Board),
    print_board(Board),

    writeln('Solution:'),
    solution(Number, Solution),
    print_board(Solution),

    writeln('Solution with other viewpoint:'),
    solution2(Number, NumbersPositions),
    print_positions(NumbersPositions),

    writeln("Solution with other viewpoint converted back to sudoku board:"),
    numbers_positions_to_board(NumbersPositions, Board2),
    print_board(Board2),

    writeln('Sudoku2:'),
    sudoku2(Board, NumbersPositions),

    writeln("OK").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELPER PROCEDURES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_board(Board) :-
    dim(Board, [N,N]),
    ( for(I,1,N), param(Board,N) do
        ( for(J,1,N), param(Board,I) do
        X is Board[I,J],
        ( var(X) -> write("  _") ; printf(" %2d", [X]) )
        ),
        nl
    ),
    nl.

print_positions(NumbersPositions) :-
    dim(NumbersPositions, [N,N, 2]),
    ( for(I, 1, N), param(NumbersPositions, N) do
        printf("%d -> ", [I]),
        ( for(J, 1, N), param(NumbersPositions, I) do
            X is NumbersPositions[I, J, 1],
			Y is NumbersPositions[I, J, 2],
            ( var(X) ->
                ( var(Y) ->
                    write("(_, _) ")
                    ;
                    printf("(_, %d) ", [Y])
                )
                ;
                ( var(Y) ->
                    printf("(%d, _) ", [X])
                    ;
                    printf("(%d, %d) ", [X, Y])
                )
            )
        ),
        nl
    ),
    nl.

% construct NumbersPositions in a way such that the X coordinates
% are sorted for each list of positions for each number
% this reduces the position search space dramatically
board_to_numbers_positions(Board, NumbersPositions) :-
    dim(NumbersPositions, [N, N, 2]),
    dim(Board, [N, N]),

    ( multifor([Number, Position], 1, N), param(NumbersPositions, Board, N) do
        X is NumbersPositions[Number, Position, 1],
        Y is NumbersPositions[Number, Position, 2],
        X #= Position,

        ( for(BoardY, 1, N), param(Board, Number, Position, Y) do
            BoardValue is Board[Position, BoardY],
            ( not(var(BoardValue)), BoardValue =:= Number ->
                Y #= BoardY
                ;
                true
            )
        )
    ).

% construct a sudoku Board out of a given NumbersPositions
numbers_positions_to_board(NumbersPositions, Board) :-
    dim(NumbersPositions, [N, N, 2]),
    dim(Board, [N, N]),

    ( multifor([Number, Position], 1, N), param(NumbersPositions, Board) do
        X is NumbersPositions[Number, Position, 1],
        Y is NumbersPositions[Number, Position, 2],
        Number #= Board[X, Y]
    ).

block_index(X, Y, SN, BlockIndex):-
	XX #= X-1,
    XXX #= XX // SN,
    BlockRow #= XXX + 1,

    YY #= Y-1,
    YYY #= YY // SN,
    BlockCol #= YYY + 1,

    BlockIndex #= (BlockRow-1) * SN + BlockCol
	.

sudoku_constraints(NumbersPositions, N) :-
    % for each number, it's positions are on different rows and columns
    ( for(Number, 1, N), param(NumbersPositions, N) do
        % list of X coordinates of positions of a certain Number
		XList is NumbersPositions[Number, 1..N, 1],
		alldifferent(XList),

        % list of Y coordinates of positions of a certain Number
        YList is NumbersPositions[Number, 1..N, 2],
		alldifferent(YList)
    ),

    % each position can only appear once in NumbersPositions
	NN is N*N,
    length(PosList, NN),
    PosList :: 1..NN,

	( multifor([Number, Position], 1, N), param(NumbersPositions, PosList, N) do
		X #= NumbersPositions[Number, Position, 1],
		Y #= NumbersPositions[Number, Position, 2],
		Pos #= (X-1) * N + Y,

        Nth is (Number-1) * N + Position,
        nth1(Nth, PosList, Pos)
	),

    alldifferent(PosList),

    % rules for blocks
    SN is integer(sqrt(N)),

	% For every number
	(for(Number, 1,N), param(SN, NumbersPositions) do
		% There are SN block rows
		(for(BlockRow, 1, SN), param(SN, NumbersPositions, Number) do
			% On these rows there are SN values
			(for(I, 1, SN), param(SN, NumbersPositions, Number, BlockRow) do
				% We have to check with the other values in these block rows
				Index is (BlockRow-1) * SN + I,
				X1 is NumbersPositions[Number,Index, 1],
				Y1 is NumbersPositions[Number,Index, 2],
				block_index(X1, Y1, SN, BlockIndex1),
				%writeln(["X1: ", X1, "Y1: ", Y1]),
				(for(K, I+1, SN), param(SN, NumbersPositions, Number, BlockRow, BlockIndex1)do
					Index2 is (BlockRow-1) * SN + K,
					X2 is NumbersPositions[Number, Index2, 1],
					Y2 is NumbersPositions[Number, Index2, 2],
					block_index(X2, Y2, SN, BlockIndex2),
					%writeln(["X2: ", X2, "Y2: ", Y2]),
					%writeln(["BlockIndex1: ", BlockIndex1, "BlockIndex2: ", BlockIndex2]),
					BlockIndex1 #\= BlockIndex2

				)
			)
		)
	),

	%
	%
	%
	% 1 -> (1, Y1), (2, Y2), (3,Y3),
	%	   (4, Y4),(5, Y5), (6, Y6),
	%      (7,Y7), (8, Y8), (9,Y9)
	%
	% Number * J * I*K
	% N * sqrt(N) * (sqrt(N)-1)!
	%
	% 9 * 2 * 3 = 9 * 6 = 54
	% _,1,_,_
	% _,_,1,_
	% _,_,_,_
	% _,_,_,_
	%
	% sqrt(N)-1
	%
	% BlockY1, BlockY2
	% BlockY1 #\= BlockY2

    true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SOME SEARCH STRATEGIES TAKEN FROM SLIDES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

search(naive,List) :-
    search(List,0,input_order,indomain,complete, []).

search(middle_out,List) :-
    middle_out(List,MOList),
    search(MOList,0,input_order,indomain,complete, []).

search(first_fail,List) :-
    search(List,0,first_fail,indomain,complete, []).

search(moff,List) :-
    middle_out(List,MOList),
    search(MOList,0,first_fail,indomain,complete, []).

search(moffmo,List) :-
    middle_out(List,MOList),
    search(MOList,0,first_fail, indomain_middle,complete, []).

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

problem(12, [](
	[](_))).

problem(13, [](
	[](2, 1),
	[](_, _))).

problem(14, [](
	[](1, _, 3),
	[](3, _, _),
	[](2, _, _))).

problem(15, [](
	[](1, _, 3, _),
	[](3, _, _, 4),
	[](2, _, _, _),
	[](4, _, 2, _))).

problem(16, [](
		[](_, _, _, _),
		[](_, _, _, _),
		[](_, _, _, _),
		[](_, _, _, _))).

problem(17, [](
	[](1, _, 3, _, _, 5, 4, 8),
	[](3, 5, 2, 4, _, 6, 7, _),
	[](2, _, 6, 3, _, 4, _, _),
	[](4, 2, 5, _, 7, _, 1, _),
	[](5, 3, 7, _, 6, 2, _, 4),
	[](6, _, _, 5, _, 7, _, _),
    [](7, 4, _, 6, _, _, 3, _),
    [](8, _, _, _, _, _, 6, _))).

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

solution(12, [](
	[](1))).

solution(13, [](
	[](2, 1),
	[](1, 2))).

solution(14, [](
	[](1, 2, 3),
	[](3, 1, 2),
	[](2, 3, 1))).


sulution(15, [](
		[](1, 4, 3, 2),
		[](3, 2, 1, 4),
		[](2, 1, 4, 3),
		[](4, 3, 2, 1))).

% Solution for problem 1 in our other viewpoint (where positions are represented in a 1D way):
solution2_old(1, [](
    [](7, 10, 23, 29, 42, 53, 63, 67, 75),
    [](3, 16, 22, 36, 37, 50, 62, 65, 78),
    [](1, 15, 23, 30, 41, 54, 56, 70, 76),
    [](5, 17, 20, 31, 45, 48, 55, 69, 79),
    [](6, 12, 27, 32, 43, 47, 58, 64, 80),
    [](2, 14, 25, 35, 39, 49, 60, 72, 73),
    [](8, 11, 24, 27, 40, 52, 59, 66, 81),
    [](4, 18, 21, 33, 44, 46, 61, 68, 74),
    [](9, 13, 19, 34, 38, 51, 57, 71, 77))).

solution2_old(14, [](
	[](1, 5, 9),
	[](2, 6, 7),
	[](3, 4, 8))).

solution2(12, [](
	[]([](1, 1)))).

solution2(13, [](
	[]([](1, 2), [](2, 1)),
	[]([](1, 1), [](2, 2)))).

solution2(14, [](
	[]([](1, 1), [](2, 2), [](3, 3)),
	[]([](1, 2), [](2, 3), [](3, 1)),
	[]([](1, 3), [](2, 1), [](3, 2)))).
