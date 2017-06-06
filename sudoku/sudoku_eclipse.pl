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

translate?

solve(ProblemName, Back) :-
	(problem(ProblemName, Board); translate(ProblemName, Board)),
	%print_board(Board),
	sudoku(Board),
	search(naive, Board, Back),
	%labeling(Board)
	%print_board(Board),
	%writeln(["Backtracks: ", Back])
	true
	.

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
% SOLVE WITH CHANNELING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve3(ProblemName, Back):-
	(problem(ProblemName, Board); translate(ProblemName, Board)),

    % writeln("Given board:"),
	% print_board(Board),
	%
    % writeln('Sudoku2:'),
    % set up variables and their constraints
    sudoku2(Board, NumbersPositions),
	sudoku(Board),
	channel(NumbersPositions, Board),

    % do search on variables
	search(naive, NumbersPositions, Back),
	true.

channel(NumbersPositions, Board):-
	dim(Board, [N, N]),
	dim(NumbersPositions, [N, N, 2]),
	( multifor([Number, Position, Y], 1, N), param(NumbersPositions, Board, N) do
		#=(Board[Position, Y], Number, B),
		#=(NumbersPositions[Number, Position, 2], Y, B)
    )
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% OUR SUDOKU SOLVER WITH ANOTHER VIEWPOINT
% Viewpoint(X, D)
% Variables X: sets of tupple positions
% Domain D: set of values 1..N
%
% Values:
%     array of N arrays of N values
%     each possible number in the sudoku problem gets an array
%     each array for a number contains positions of the sudoku board that that number lies on
%     all of the numbers appear an equal amount of times, so each array has equal length
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve2(ProblemName, Back) :-
    (problem(ProblemName, Board); translate(ProblemName, Board)),

    % writeln("Given board:"),
	% print_board(Board),
	%
    % writeln('Sudoku2:'),
    % set up variables and their constraints
    sudoku2(Board, NumbersPositions),
	%%writeln("Back"),
    % do search on variables
	search(naive, NumbersPositions, Back),

    % print results
    % writeln("Sudoku2 done:"),
    % print_positions(NumbersPositions),
	% writeln(["Backtracks: ", Back]),
    % writeln("Converted back to sudoku board:"),
    % numbers_positions_to_board(NumbersPositions, Board2),
    % print_board(Board2),

    %writeln("Given board again for checking:"),
	%print_board(Board).
	%writeln(["Backtracks: ", Back]).
	true.

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SOME SEARCH STRATEGIES TAKEN FROM SLIDES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

search(naive,List, Back) :-
    search(List,0,input_order,indomain,complete, [backtrack(Back)]).

search(middle_out,List, Back) :-
    middle_out(List,MOList),
    search(MOList,0,input_order,indomain,complete, [backtrack(Back)]).

search(first_fail,List, Back) :-
    search(List,0,first_fail,indomain,complete, [backtrack(Back)]).

search(moff,List, Back) :-
    middle_out(List,MOList),
    search(MOList,0,first_fail,indomain,complete, [backtrack(Back)]).

search(moffmo,List, Back) :-
    middle_out(List,MOList),
    search(MOList,0,first_fail, indomain_middle,complete, [backtrack(Back)]).

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

% construct NumbersPositions in such a way that the X coordinates
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

% Calculates the block index number for a given X,Y pair.
block_index(X, Y, SN, BlockIndex):-
	XX #= X-1,
    XXX #= XX // SN,
    BlockRow #= XXX + 1,

    YY #= Y-1,
    YYY #= YY // SN,
    BlockCol #= YYY + 1,

    BlockIndex #= (BlockRow-1) * SN + BlockCol
	.

% The constraints used for the search
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
				(for(K, I+1, SN), param(SN, NumbersPositions, Number, BlockRow, BlockIndex1) do
					Index2 is (BlockRow-1) * SN + K,
					X2 is NumbersPositions[Number, Index2, 1],
					Y2 is NumbersPositions[Number, Index2, 2],
					block_index(X2, Y2, SN, BlockIndex2),
					% The same item can't appear in the same block
					BlockIndex1 #\= BlockIndex2
				)
			)
		)
	).


experiments :-
	open('experiments.txt', write, Stream),
	write(Stream, "\\begin{table}[h!]
  \\begin{tabular}{|c|c|c|c|c|c|c|}
    \\hline
    \\multirow{2}{*}{Puzzle} &
      \\multicolumn{2}{L|}{Classical Viewpoint (ifirst fail)} &
      \\multicolumn{2}{L|}{Our Viewpoint (first fail)} &
      \\multicolumn{2}{L|}{ Channeling (first fail)} \\\\
    & ms & backracks & ms & backracks & ms & backracks \\\\
    \\hline\n"),
	(   puzzles(P, X),
		writeln(X),

		% Classic viewpoint
		statistics(runtime, [_ | [_]]),
		solve(X, B1),
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
    ;   true
    ),
	write(Stream," \\hline
  \\end{tabular}
\\end{table}"),
	writeln("Finished all"),
	close(Stream).
