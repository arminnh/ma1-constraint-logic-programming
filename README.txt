This project contains Sudoku and Hashiwokakero puzzle solvers written in ECLiPSe and CHR.

1. Sudoku
    The Sudoku solvers are written in ECLiPSe and in CHR.

    *) ECLiPSe:
        sudoku/sudoku_eclipse.pl

    *) CHR:
        sudoku/sudoku_chr_1.pl
        sudoku/sudoku_chr_2.pl
        sudoku/sudoku_chr_channeling.pl

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

    *) CHR:
        hashiwokakero/hashiwokakero_chr_1.pl
        hashiwokakero/hashiwokakero_chr_2.pl
