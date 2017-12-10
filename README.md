[![Build Status](https://travis-ci.org/Raffaello/scala-sudoku.svg?branch=master)](https://travis-ci.org/Raffaello/scala-sudoku)
[![codecov](https://codecov.io/gh/Raffaello/scala-sudoku/branch/master/graph/badge.svg)](https://codecov.io/gh/Raffaello/scala-sudoku)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/d54b5b9a1d704cb9964d0c5d4831694f)](https://www.codacy.com/app/Raffaello/scala-sudoku?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=Raffaello/scala-sudoku&amp;utm_campaign=Badge_Grade)
[![Codacy Badge](https://api.codacy.com/project/badge/Coverage/d54b5b9a1d704cb9964d0c5d4831694f)](https://www.codacy.com/app/Raffaello/scala-sudoku?utm_source=github.com&utm_medium=referral&utm_content=Raffaello/scala-sudoku&utm_campaign=Badge_Coverage)


# Sudoku Problem

it is converted to an Exact Cover Problem

### Conversion Strategy

to enforce a cell with a clue, it will set to 0 
all the others column constraints relative to that specific grid cell.

So basically is reducing for each clue a number of 32 ones

- 36 are total for 9 values for each grid position

#### Convert back from DLX solution to the grid

The unique solution is composed by 81 elements, each of them is compose by 4 values
representing the row of those 4 columns:

give them ordered the first value represent the cell constraints from which 
it would be simple recovering the row and column value of the grid
just by doing an integer division and getting the rest.

the value at that point can be retrieved by one of the other 3 values.

For consistency it is enforce a check among the value returned.

### JVM tuning

No real need of it

#### Others GC tuning

##### Algorithms

- `-XX:+UseG1GC`
- `-XX:+UseConcMarkSweepGC`

##### Logging

- `-XX:+PrintGCDetails`
- `-XX:+PrintGCDateStamps`
- `-XX:+PrintGCTimeStamps`

### TODO

- n-Queens problem
- generalize sudoku
- solve with integer programming
- use C/C++ scala native dlx algorithm

### References
- [An Integer Programming Model for the Sudoku Problem](https://pdfs.semanticscholar.org/152c/baf232689b44da800437debefdb00b54fc19.pdf)
- [Solve Sudoku Puzzles Via Integer Programming](http://uk.mathworks.com/help/optim/ug/solve-sudoku-puzzles-via-integer-programming.html?requestedDomain=uk.mathworks.com)
- [Sudoku Puzzles Generating: from Easy to Evil](http://zhangroup.aporc.org/images/files/Paper_3485.pdf)
- [Dancing Links](https://arxiv.org/abs/cs/0011047)

### Related References:
- [Complexity of n-Queens Completion](http://jair.org/media/5512/live-5512-10126-jair.pdf)
