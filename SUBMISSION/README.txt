This project contains Sudoku and Hashiwokakero puzzle solvers written in ECLiPSe and CHR.

1. Sudoku
    The Sudoku solvers are written in ECLiPSe and in CHR. Both solvers make use of sudoku/boards.pl
    to load and solve puzzles. The puzzles can be entered as 'board/2' or 'puzzles/2' facts which
    contain the sudoku boards in a matrix representation (prolog list of lists).

    *) ECLiPSe:
        sudoku/sudoku_eclipse.pl
            Queries:
                solve1(ProblemName). solves a board using the classical viewpoint
                solve2(ProblemName). solves a board using the alternative viewpoint
                solve3(ProblemName). solves a board using channeling constraints between both viewpoints

    *) CHR:
        sudoku/sudoku_chr_1.pl
            This file contains the trivial viewpoint. It uses our likelihood heurisic on
            which you can find more information over in the report.
            Query: solve(Problem).
       

        sudoku/sudoku_chr_2.pl
            This file contains the alternative viewpoint. To speedup the search here we use a smart diff.
            More info can also be found in the report
            Query: solve_viewpoint_2(ProblemName).

        sudoku/sudoku_chr_merged.pl
            This file contains the merged version of the two prior files.
            This is used for the channeling constraints and for the experiments.
            Queries:
                solve1(ProblemName).
                solve2(ProblemName).
                solve3(ProblemName) .

2. Hashiwokakero:
    The Hashiwokakero solvers are also written in ECLiPSe and in CHR. The CHR solver
    has two versions. The difference between them is that the second version uses a
    better connectivity constraint propagator.

    Both of the solvers make use of hashiwokakero/boards.pl to load and solve puzzles.
    Puzzles in board.pl can be entered as a 'puzzle/3' fact that follows the
    input format specified in the assignment, or as a 'board/2' fact that contains
    a hashiwokakero board in a matrix representation.

    *) ECLiPSe:
        hashiwokakero/hashiwokakero_eclipse.pl
            Queries:
                solve(Problem).
                findall(Problem).

    *) CHR:
        hashiwokakero/hashiwokakero_chr_1.pl
            Queries:
                solve(Problem).
                time(Problem).
                timeall.
            
        hashiwokakero/hashiwokakero_chr_2.pl
            Queries:
                solve(Problem).
                time(Problem).
                timeall.
