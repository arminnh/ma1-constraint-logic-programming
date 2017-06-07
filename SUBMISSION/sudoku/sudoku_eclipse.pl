%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	Sudoku is a puzzle, originating from Japan, where you have a
%	9x9 grid, consisting of 9 3x3 sub-grids. The challenge is
%	to fill the grid with numbers from 1 to 9 such that every row,
%	every column, and every 3x3 sub-grid contains the digits 1 to 9.
%	Some of these numbers are given, which is the way different
%	instances of the problem are made. The solution is usually unique.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- lib(ic).
:- import alldifferent/1, sorted/2 from ic_global.
:- coroutine.
:- import nth1/3 from listut.
:- [boards].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GIVEN SUDOKU SOLVER WITH TRIVIAL VIEWPOINT
% Author: Joachim Schimpf, IC-Parc --- ECLiPSe sample code - Sudoku problem
%
% Viewpoint(X, D)
% Variables X: sets of rows, sets of columns, sets of blocks
% Domain D: sets of values 1..N2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve1(ProblemName) :-
    load_board(ProblemName, Board),
    writeln("Given board:"),  print_board(Board),
	sudoku1(Board),
	search(naive, Board, _),
    writeln("Solution:"),  print_board(Board).

solve1(ProblemName, Back) :-
    load_board(ProblemName, Board),
    writeln("Given board:"), print_board(Board),
	sudoku1(Board),
	search(naive, Board, Back),
    writeln("Solution:"), print_board(Board),
	writeln(["Amount of backtracks: ", Back]).

sudoku1(Board) :-
	dim(Board, [N2,N2]),
	N is integer(sqrt(N2)),
	Board[1..N2,1..N2] :: 1..N2,

	( for(I,1,N2), param(Board,N2) do
	    Row is Board[I,1..N2],
	    alldifferent(Row),
	    Col is Board[1..N2,I],
	    alldifferent(Col)
	),

	( multifor([I, J], 1, N2, N), param(Board, N) do
	    ( multifor([K, L], 0, N-1), param(Board, I, J), foreach(X, SubSquare) do
            X is Board[I+K, J+L]
	    ),
	    alldifferent(SubSquare)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SUDOKU SOLVER WITH ALTERNATIVE VIEWPOINT
% Viewpoint(X, D)
% Variables X: sets of positions
% Domain D: set of values 1..N
%
% In the alternative viewpoint, each possible sudoku number gets an array. The arrays
% contain the positions on which the numbers occur on the board. The index in the arrays
% represents the X value of the positions. The values in the arrays represent the according
% Y values. These Y values are the search variables in the alternative viewpoint.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve2(ProblemName) :-
    load_board(ProblemName, Board),
    writeln("Given board:"), print_board(Board),
    % set up variables and their constraints
    sudoku2(Board, NumbersPositions),
    % do search on variables
	search(naive, NumbersPositions, _),
    writeln("Solution:"), print_positions(NumbersPositions),
    numbers_positions_to_board(NumbersPositions, Board2),
    writeln("Solution converted back to sudoku board:"), print_board(Board2).

solve2(ProblemName, Back) :-
    load_board(ProblemName, Board),
    writeln("Given board:"), print_board(Board),
    % set up variables and their constraints
    sudoku2(Board, NumbersPositions),
    % do search on variables
	search(naive, NumbersPositions, Back),
    writeln("Solution:"), print_positions(NumbersPositions),
    numbers_positions_to_board(NumbersPositions, Board2),
    writeln("Solution converted back to sudoku board:"), print_board(Board2),
    writeln(["Amount of backtracks: ", Back]).

sudoku2(Board, NumbersPositions) :-
    % dimensions of board = N*N. There are N possible numbers to be used on the Board
    % Board has sqrt(N) blocks of N numbers (with dimensions sqrt(N)*sqrt(N))
    dim(Board, [N, N]),

    % declare an array of arrays. each possible number on the sudoku board gets an array.
    % each of those arrays contain positions of their numbers on the board
    % the indices of the array represent the X valuea of positions (and is known before the search
    % phase, every number must appear once on every row), and the values at those indices represent
    % the Y values of positions (and are the search variables).
    dim(NumbersPositions, [N, N]),
    NumbersPositions[1..N, 1..N] :: 1..N,

    % fill in known positions from given board into NumbersPositions
    board_to_numbers_positions(Board, NumbersPositions),

    % set sudoku constraints
    sudoku2_constraints(NumbersPositions, N).

% construct NumbersPositions in such a way that the X coordinates are sorted for each
% list of positions for each number. this reduces the position search space dramatically
board_to_numbers_positions(Board, NumbersPositions) :-
    dim(Board, [N, N]),
    dim(NumbersPositions, [N, N]),

    ( multifor([Number, X], 1, N), param(NumbersPositions, Board, N) do
        Y is NumbersPositions[Number, X],

        ( for(BoardY, 1, N), param(Board, Number, X, Y) do
            BoardValue is Board[X, BoardY],
            ( number(BoardValue), BoardValue =:= Number ->
                Y #= BoardY
                ;
                true
            )
        )
    ).

% the constraints used for sudoku with the alternative viewpoint
sudoku2_constraints(NumbersPositions, N) :-
    % for each number array, make sure that the Y values are all different
    % no need to do this for X values as the indices of the array are already different
    ( for(Number, 1, N), param(NumbersPositions, N) do
        % list of Y coordinates of positions of a certain Number
        YList is NumbersPositions[Number, 1..N],
		alldifferent(YList)
    ),

    % each position can only appear once in NumbersPositions
	NN is N*N,
    length(PosList, NN),
    PosList :: 1..NN,
	( multifor([Number, X], 1, N), param(NumbersPositions, PosList, N) do
		Y #= NumbersPositions[Number, X],
		Pos #= (X-1) * N + Y,
        Nth is (Number-1) * N + X,
        nth1(Nth, PosList, Pos)
	),
    alldifferent(PosList),

    % constraints for blocks
    SN is integer(sqrt(N)),
	% for every number
	(for(Number, 1, N), param(SN, NumbersPositions) do
		% there are SN block rows
		(for(BlockRow, 1, SN), param(SN, NumbersPositions, Number) do
			% on these rows there are SN blocks
			(for(I, 1, SN), param(SN, NumbersPositions, Number, BlockRow) do
				% need to check with the other values in these block rows
				X1 is (BlockRow-1) * SN + I,
				Y1 is NumbersPositions[Number, X1],
				block_index(X1, Y1, SN, BlockIndex1),
				(for(K, I+1, SN), param(SN, NumbersPositions, Number, BlockRow, BlockIndex1) do
					X2 is (BlockRow-1) * SN + K,
					Y2 is NumbersPositions[Number, X2],
					block_index(X2, Y2, SN, BlockIndex2),
					% The same item can't appear in the same block
					BlockIndex1 #\= BlockIndex2
				)
			)
		)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SUDOKU SOLVER WITH CHANNELING CONSTRAINTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve3(ProblemName):-
    load_board(ProblemName, Board),
    writeln("Given board:"), print_board(Board),

    % set up variables and their constraints
    sudoku2(Board, NumbersPositions),
	sudoku1(Board),
	channel(NumbersPositions, Board),

    % do search on variables
	search(naive, NumbersPositions, _),
    writeln("Solution:"),
    print_board(Board), print_positions(NumbersPositions).

solve3(ProblemName, Back):-
    load_board(ProblemName, Board),
    writeln("Given board:"), print_board(Board),

    % set up variables and their constraints
    sudoku2(Board, NumbersPositions),
	sudoku1(Board),
	channel(NumbersPositions, Board),

    % do search on variables
	search(naive, NumbersPositions, Back),
    writeln("Solution:"), print_board(Board), print_positions(NumbersPositions),
    writeln(["Amount of backtracks: ", Back]).

channel(NumbersPositions, Board):-
	dim(Board, [N, N]),
	dim(NumbersPositions, [N, N]),
	( multifor([Number, X, Y], 1, N), param(NumbersPositions, Board) do
		#=(Board[X, Y], Number, B),
		#=(NumbersPositions[Number, X], Y, B)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SOME SEARCH STRATEGIES TAKEN FROM THE COURSE MATERIAL
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

search(naive, List, Backtracks) :-
    search(List, 0, input_order, indomain, complete, [backtrack(Backtracks)]).

search(middle_out, List, Backtracks) :-
    middle_out(List, MOList),
    search(MOList, 0, input_order, indomain, complete, [backtrack(Backtracks)]).

search(first_fail, List, Backtracks) :-
    search(List, 0, first_fail, indomain, complete, [backtrack(Backtracks)]).

search(moff, List, Backtracks) :-
    middle_out(List, MOList),
    search(MOList, 0, first_fail, indomain, complete, [backtrack(Backtracks)]).

search(moffmo, List, Backtracks) :-
    middle_out(List, MOList),
    search(MOList, 0, first_fail,  indomain_middle, complete, [backtrack(Backtracks)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELPER PROCEDURES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% load a usable sudoku array from a given board (board/2 or puzzles/2 fact)
load_board(ProblemName, Board):-
    (board(ProblemName, Input) ; puzzles(Input, ProblemName)),

    length(Input, N),
    dim(Board, [N, N]),

    ( multifor([I, J], 1, N), param(Input, Board) do
        nth1(I, Input, Row),
        nth1(J, Row, Val),
        Board[I, J] #= Val
    ).

% print the Board array
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

% print the board in the other viewpoint
print_positions(NumbersPositions) :-
    dim(NumbersPositions, [N, N]),
    ( for(Number, 1, N), param(NumbersPositions, N) do
        printf("%d -> ", [Number]),
        ( for(X, 1, N), param(NumbersPositions, Number) do
			Y is NumbersPositions[Number, X],
            ( var(Y) -> printf("(%d, _) ", [X]) ; printf("(%d, %d) ", [X, Y]) )
        ),
        nl
    ),
    nl.

% convert the board in the other viewpoint to the classic matrix representation
numbers_positions_to_board(NumbersPositions, Board) :-
    dim(NumbersPositions, [N, N]),
    dim(Board, [N, N]),

    ( multifor([Number, X], 1, N), param(NumbersPositions, Board) do
        Y is NumbersPositions[Number, X],
        Number #= Board[X, Y]
    ).

% BlockIndex is the block number for a given position (X, Y).
block_index(X, Y, SN, BlockIndex):-
	XX #= X-1,
    XXX #= XX // SN,
    BlockRow #= XXX + 1,

    YY #= Y-1,
    YYY #= YY // SN,
    BlockCol #= YYY + 1,

    BlockIndex #= (BlockRow-1) * SN + BlockCol.

experiments :-
	open('experiments.txt', write, Stream),
	write(Stream, "\\begin{table}[h!]
  \\begin{tabular}{|c|c|c|c|c|c|c|}
    \\hline
    \\multirow{2}{*}{Puzzle} &
      \\multicolumn{2}{L|}{Classical Viewpoint (ifirst fail)} &
      \\multicolumn{2}{L|}{Alternative viewpoint (first fail)} &
      \\multicolumn{2}{L|}{ Channeling (first fail)} \\\\
    & ms & backtracks & ms & backtracks & ms & backtracks \\\\
    \\hline\n"),
	(   puzzles(_, X),
		writeln(X),

		% Classic viewpoint
		statistics(runtime, [_ | [_]]),
		solve1(X, B1),
		statistics(runtime, [_ | [ExecutionTimeMS1]]),
		ExTimeS1 is ExecutionTimeMS1 / 1000,
		statistics(runtime, [_ | [_]]),
		solve2(X, B2),
		statistics(runtime, [_ | [ExecutionTimeMS2]]),
		ExTimeS2 is ExecutionTimeMS2 / 1000,
		statistics(runtime, [_ | [_]]),
		solve3(X, B3),
		statistics(runtime, [_ | [ExecutionTimeMS3]]),
		ExTimeS3 is ExecutionTimeMS3 / 1000,
	    %writeln('Execution took '), write(ExTimeS), write(' s.'), nl,
	 	write(Stream, X),
		write(Stream, " & "),
		write(Stream, ExTimeS1),
		write(Stream, "s & "),
		write(Stream, B1),
		write(Stream, " & "),
		write(Stream, ExTimeS2),
		write(Stream, "s & "),
		write(Stream, B2),
		write(Stream, " & "),
		write(Stream, ExTimeS3),
		write(Stream, "s & "),
		write(Stream, B3),
		write(Stream, '\\\\'),
		write(Stream, "\n"),
		writeln(["finished", X]),
		fail
    ;
        true
    ),
	write(Stream," \\hline
  \\end{tabular}
\\end{table}"),
	writeln("Finished all"),
	close(Stream).
