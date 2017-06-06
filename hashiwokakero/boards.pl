% boards for which wrong solutions with multiple connected sets are possible:
%   6 (with improvement A turned off), 7 (with improvements A and B turned off), 9 (with improvement B off), 12, 15, 17

% IMPACT OF IMPROVEMENTS ON PREVIOUSLY SLOW BOARDS:
%
%   CONNECTIVITY PROPAGATOR 1
%       board number | time without improvements |  time with improvements
%       1   | 0.054s | 0.047s
%       3   | 0.03s | 0.031s
%       4   | 0.059s | 0.06s
%       5   | 0.057s | 0.054s
%       6   | 0.127s | 0.117s
%       8   | 0.004s | 0.006s
%       9   | 0.007s | 0.006s
%       10  | 27.567s | 1.389s
%       11  | 0.046s | 0.046s
%       12  | 0.055s | 0.051s
%       13  | 0.066s | 0.058s
%       14  | 0.092s | 0.075s
%       15  | 1.614s | 1.533s
%       16  | 1.186s | 1.143s
%       17  | 2.254s | 0.229s
%       2   | aborted after 6405s | 1001s | CHECK THIS AGAIN
%
%   CONNECTIVITY PROPAGATOR 2
%       board number | time without improvements | time with improvements
%       1   | 0.053s | 0.049s
%       3   | 0.029s | 0.026s
%       4   | 0.056s | 0.061s
%       5   | 0.053s | 0.056s
%       6   | 0.128s | 0.118s
%       8   | 0.007s | 0.004s
%       9   | 0.007s | 0.008s
%       10  | 28.369s | 1.364s
%       11  | 0.053s | 0.047s
%       12  | 0.048s | 0.052s
%       13  | 0.06s | 0.057s
%       14  | 0.087s | 0.076s
%       15  | 1.701s | 1.573s
%       16  | 1.219s | 1.156s
%       17  | 2.292s | 0.23s
%       2   | unknown | 847s | CHECK THIS AGAIN

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SAMPLE PROBLEMS
% THE FIRST 4 PUZZLES ARE THE ONES GIVEN WITH THE ASSIGNMENT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% puzzle 1, easy
% http://en.wikipedia.org/wiki/File:Val42-Bridge1n.png
% solution: http://en.wikipedia.org/wiki/File:Val42-Bridge1.png
puzzle(1,7,P):-
	P=[	(1,1,2),(1,2,3),(1,4,4),(1,6,2),
		(2,7,2),
		(3,1,1),(3,2,1),(3,5,1),(3,6,3),(3,7,3),
		(4,1,2),(4,4,8),(4,6,5),(4,7,2),
		(5,1,3),(5,3,3),(5,7,1),
		(6,3,2),(6,6,3),(6,7,4),
		(7,1,3),(7,4,3),(7,5,1),(7,7,2)].

% puzzle 2, moderate
% http://en.wikipedia.org/wiki/File:Bridges-example.png
% solution: http://upload.wikimedia.org/wikipedia/en/1/10/Bridges-answer.PNG

puzzle(2,13,P):-
	P=[	(1,1,2),(1,3,4),(1,5,3),(1,7,1),(1,9,2),(1,12,1),
		(2,10,3),(2,13,1),
		(3,5,2),(3,7,3),(3,9,2),
		(4,1,2),(4,3,3),(4,6,2),(4,10,3),(4,12,1),
		(5,5,2),(5,7,5),(5,9,3),(5,11,4),
		(6,1,1),(6,3,5),(6,6,2),(6,8,1),(6,12,2),
		(7,7,2),(7,9,2),(7,11,4),(7,13,2),
		(8,3,4),(8,5,4),(8,8,3),(8,12,3),
		(10,1,2),(10,3,2),(10,5,3),(10,9,3),(10,11,2),(10,13,3),
		(11,6,2),(11,8,4),(11,10,4),(11,12,3),
		(12,3,1),(12,5,2),
		(13,1,3),(13,6,3),(13,8,1),(13,10,2),(13,13,2)].

% puzzle 3
% http://www.conceptispuzzles.com/index.aspx?uri=puzzle/hashi/techniques
puzzle(3,6,P) :-
     P = [ (1,1,1),(1,3,4),(1,5,2),
			(2,4,2),(2,6,3),
			(3,1,4),(3,3,7),(3,5,1),
			(4,4,2),(4,6,5),
			(5,3,3),(5,5,1),
			(6,1,3),(6,4,3),(6,6,3)].

% puzzle 4
% http://www.conceptispuzzles.com/index.aspx?uri=puzzle/euid/010000008973f050f28ceb4b11c74e73d34e1c47d885e0d8449ab61297e5da2ec85ea0804f0c5a024fbf51b5a0bd8f573565bc1b/play
puzzle(4,8,P) :-
	P = [ (1,1,2),(1,3,2),(1,5,5),(1,7,2),
			(2,6,1),(2,8,3),
			(3,1,6),(3,3,3),
			(4,2,2),(4,5,6),(4,7,1),
			(5,1,3),(5,3,1),(5,6,2),(5,8,6),
			(6,2,2),
			(7,1,1),(7,3,3),(7,5,5),(7,8,3),
			(8,2,2),(8,4,3),(8,7,2)].

% http://stackoverflow.com/questions/20337029/hashi-puzzle-representation-to-solve-all-solutions-with-prolog-restrictions/20364306#20364306
board(5, [
    [3, 0, 6, 0, 0, 0, 6, 0, 3],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 1, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [2, 0, 0, 0, 0, 1, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [1, 0, 3, 0, 0, 2, 0, 0, 0],
    [0, 3, 0, 0, 0, 0, 4, 0, 1]
]).

% same as 1, but more split up for visibility
board(6, [
    [2, 0, 3, 0, 4, 0, 0, 0, 2, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [1, 0, 1, 0, 0, 0, 1, 0, 3, 0, 3],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [2, 0, 0, 0, 8, 0, 0, 0, 5, 0, 2],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [3, 0, 0, 3, 0, 0, 0, 0, 0, 0, 1],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 2, 0, 0, 0, 0, 3, 0, 4],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [3, 0, 0, 0, 3, 0, 1, 0, 0, 0, 2]
]).

% THIS BOARD SHOULD FAIL
board(7, [
    [1, 0, 1, 0, 2],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 2]
]).

board(8, [
    [1, 0, 2, 0, 3],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 2]
]).

board(9, [
    [2, 0, 0, 0, 2],
    [0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0],
    [2, 0, 0, 0, 2]
]).

board(10, [
    [0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 3, 0, 2, 0],
    [4, 0, 0, 0, 4, 0, 0, 3, 0, 0, 0, 4, 0, 4, 0, 0, 2, 0, 0, 0, 1],
    [0, 1, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 4, 0],
    [4, 0, 0, 0, 0, 0, 0, 0, 2, 0, 3, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0],
    [0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0],
    [0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 3, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0],
    [0, 4, 0, 0, 0, 0, 0, 0, 2, 0, 0, 3, 0, 0, 0, 0, 0, 4, 0, 1, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 3, 0, 1, 0, 0, 0, 0, 0],
    [0, 0, 3, 0, 6, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 2, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 4, 0, 6, 0, 0, 0, 0, 0, 0, 5],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 2, 0, 4, 0, 1, 0, 2, 0, 0, 3, 0, 4, 0, 0, 0, 0, 2, 0, 0],
    [0, 5, 0, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 4, 0, 0, 0, 0, 0, 0, 1, 0],
    [3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 3, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 5],
    [0, 0, 0, 1, 0, 0, 0, 4, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 3, 0, 0, 0, 4, 0, 0, 0, 2, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 2],
    [1, 0, 2, 0, 3, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 4, 0, 2, 0]
]).

board(11, [
    [4, 0, 3, 0, 4, 0, 3, 0, 4],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [2, 0, 0, 0, 0, 0, 0, 0, 2],
    [0, 0, 2, 0, 8, 0, 0, 2, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 4, 0, 5, 0, 0, 1, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [2, 0, 6, 0, 2, 0, 0, 0, 0]
]).

board(12, [
    [4, 0, 4, 0, 0, 2, 0, 2, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [3, 0, 1, 0, 0, 4, 0, 4, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [1, 0, 0, 0, 0, 0, 0, 2, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 1, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 2, 0, 0, 0]
]).

board(13, [
    [0, 3, 0, 0, 0, 0, 0, 2, 0],
    [0, 0, 0, 2, 0, 0, 0, 0, 2],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 4, 0, 7, 0, 0, 0, 2, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 1, 0, 3, 0, 0, 3, 0, 3],
    [0, 0, 0, 0, 0, 0, 0, 1, 0],
    [0, 2, 0, 0, 0, 0, 6, 0, 3],
    [0, 0, 0, 0, 0, 0, 0, 0, 0]
]).

board(14, [
    [2, 0, 3, 0, 0, 2, 0, 0, 1],
    [0, 0, 0, 0, 0, 0, 1, 0, 0],
    [1, 0, 5, 0, 2, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 2, 0, 4],
    [0, 0, 0, 0, 1, 0, 0, 0, 0],
    [1, 0, 5, 0, 0, 7, 0, 0, 6],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 3, 0, 2, 0, 0, 0, 0],
    [2, 0, 0, 0, 0, 4, 0, 0, 2]
]).

board(15, [
    [0, 2, 0, 2, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 1, 0],
    [0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 3, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0],
    [0, 0, 4, 0, 4, 0, 8, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 3, 0, 0, 2],
    [0, 0, 0, 1, 0, 2, 0, 4, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 2, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 2, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0, 3, 0, 0, 0, 0, 0, 3, 0, 1],
    [0, 0, 6, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 6, 0, 4, 0, 1, 0, 0, 0, 3, 0, 4],
    [0, 0, 3, 0, 0, 3, 0, 0, 0, 0, 0, 2, 0, 4, 0, 0, 2, 0, 0, 0, 0],
    [0, 0, 0, 0, 2, 0, 0, 4, 0, 0, 4, 0, 0, 0, 0, 0, 0, 6, 0, 0, 3],
    [0, 0, 0, 0, 0, 3, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0],
    [0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 3, 0, 0, 1],
    [0, 0, 3, 0, 0, 3, 0, 4, 0, 0, 4, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0],
    [0, 3, 0, 0, 0, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3],
    [0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 5, 0, 0, 0, 3, 0, 0, 0, 1, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 3, 0, 0, 0, 0, 3, 0, 0, 0, 5, 0, 0, 0, 0, 0, 6, 0, 0, 0, 3],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 4, 0, 0, 0, 0, 2, 0, 1, 0, 4, 0, 4, 0, 2, 0, 2, 0, 1, 0, 2]
]).

board(16, [
    [1, 0, 5, 0, 3, 0, 0, 0, 0, 0, 4, 0, 0, 0, 4, 0, 0, 0, 6, 0, 4],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [1, 0, 4, 0, 0, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0],
    [3, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 3, 0, 5, 0, 0, 2, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [3, 0, 6, 0, 0, 0, 0, 0, 0, 0, 7, 0, 0, 0, 0, 0, 0, 0, 0, 4, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 2, 0, 0],
    [0, 0, 3, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3],
    [3, 0, 0, 0, 2, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2],
    [0, 3, 0, 4, 0, 0, 8, 0, 0, 3, 0, 0, 0, 2, 0, 0, 0, 0, 1, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 2, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 3, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 2, 0, 6, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0],
    [4, 0, 0, 5, 0, 0, 0, 0, 0, 0, 7, 0, 0, 4, 0, 0, 3, 0, 0, 2, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 3],
    [2, 0, 0, 1, 0, 2, 0, 0, 0, 0, 6, 0, 0, 0, 0, 4, 0, 0, 0, 2, 0]
]).

board(17, [
    [2, 0, 6, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0],
    [0, 0, 0, 3, 0, 0, 2, 0, 2, 0, 4, 0, 1],
    [2, 0, 5, 0, 0, 3, 0, 0, 0, 1, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 4, 0, 6, 0, 0, 0, 0, 6, 0, 2],
    [2, 0, 0, 0, 2, 0, 4, 0, 2, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [4, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1],
    [0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0],
    [3, 0, 5, 0, 0, 0, 5, 0, 0, 0, 6, 0, 1],
    [0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [1, 0, 3, 0, 0, 0, 2, 0, 1, 0, 0, 0, 0],
    [0, 2, 0, 4, 0, 0, 0, 0, 0, 0, 5, 0, 2]
]).

board(18, [
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 2, 0, 1, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],
    [1, 0, 4, 0, 6, 0, 2, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 4, 0, 5, 0, 2, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],
    [2, 0, 6, 0, 2, 0, 1, 0]
]).

board(19, [
    [0, 0, 2, 0, 0,1 ],
    [0, 0, 0, 0, 0,0 ],
    [0, 0, 2, 0, 0,0 ],
    [0, 0, 0, 0, 0,0 ],
    [1, 0, 3, 0, 1,0 ]
]).

board(20, [
    [2, 0, 0, 0, 3, 0, 0, 0, 2],
    [0, 4, 0, 5, 0, 0, 0, 3, 0],
    [0, 0, 0, 0, 0, 2, 0, 0, 0],
    [2, 0, 0, 0, 0, 0, 0, 0, 0],
    [6, 0, 0, 6, 0, 5, 0, 0, 0],
    [0, 2, 0, 0, 0, 0, 1, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 1, 0],
    [0, 0, 0, 0, 0, 1, 0, 0, 0],
    [2, 0, 0, 3, 0, 0, 4, 0, 2]
]).

board(21, [
    [2, 0, 3, 0, 1, 0],
    [0, 0, 0, 0, 0, 1],
    [4, 0, 6, 0, 1, 0],
    [0, 0, 0, 2, 0, 4],
    [0, 0, 3, 0, 1, 0],
    [2, 0, 0, 2, 0, 2]
]).


board(22, [
    [2, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0],
    [3, 0, 2, 0, 0, 0],
    [0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0],
    [1, 0, 2, 0, 0, 0]
]).


board(23, [
    [2, 0, 2, 0, 3, 0, 2],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 1, 0, 4, 0, 2],
    [0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0],
    [1, 0, 2, 0, 3, 0, 0]
]).
