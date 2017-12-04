[![Build Status](https://travis-ci.org/Raffaello/scala-sudoku.svg?branch=master)](https://travis-ci.org/Raffaello/scala-sudoku)
[![codecov](https://codecov.io/gh/Raffaello/scala-sudoku/branch/master/graph/badge.svg)](https://codecov.io/gh/Raffaello/scala-sudoku)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/d54b5b9a1d704cb9964d0c5d4831694f)](https://www.codacy.com/app/Raffaello/scala-sudoku?utm_source=github.com&amp;utm_medium=referral&amp;utm_content=Raffaello/scala-sudoku&amp;utm_campaign=Badge_Grade)
[![Codacy Badge](https://api.codacy.com/project/badge/Coverage/d54b5b9a1d704cb9964d0c5d4831694f)](https://www.codacy.com/app/Raffaello/scala-sudoku?utm_source=github.com&utm_medium=referral&utm_content=Raffaello/scala-sudoku&utm_campaign=Badge_Coverage)


### JVM tuning

Due to the nature of FP even with `@tailrec` functions,
a lot of garbage is generated. This is very inefficient.
To run the program was required to use `SBT_OPTS="-XX:-UseGCOverheadLimit"`.
Because it is very inconvenient to spend almost
all the time doing `GC` operations, the `Data.fold` DLX function
is not used anymore. ?????

(probably a bug in the DLX algorithm [wip])

#### Others GC tuning

##### Algorithms

- `-XX:+UseG1GC`
- `-XX:+UseConcMarkSweepGC`

##### Logging

- `-XX:+PrintGCDetails`
- `-XX:+PrintGCDateStamps`
- `-XX:+PrintGCTimeStamps`

### References
- [An Integer Programming Model for the Sudoku Problem](https://pdfs.semanticscholar.org/152c/baf232689b44da800437debefdb00b54fc19.pdf)
- [Solve Sudoku Puzzles Via Integer Programming](http://uk.mathworks.com/help/optim/ug/solve-sudoku-puzzles-via-integer-programming.html?requestedDomain=uk.mathworks.com)
- [Sudoku Puzzles Generating: from Easy to Evil](http://zhangroup.aporc.org/images/files/Paper_3485.pdf)
- [Solving Sudoku efficiently with Dancing Links](https://www.kth.se/social/files/58861771f276547fe1dbf8d1/HLaestanderMHarrysson_dkand14.pdf)
- [Dancing Links](https://arxiv.org/abs/cs/0011047)

### Related References:
- [Complexity of n-Queens Completion](http://jair.org/media/5512/live-5512-10126-jair.pdf)
