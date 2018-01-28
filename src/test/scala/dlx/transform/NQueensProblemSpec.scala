package dlx.transform

import org.scalatest.{FlatSpec, Matchers}

/**
  * [[https://en.wikipedia.org/wiki/Eight_queens_puzzle#Solutions]]
  */
object Solution1 {
  val checkboardSol = Array(
    //               A      B      C      D      E      F      G      H
    Array[Boolean](false, false, false,  true, false, false, false, false),
    Array[Boolean](false, false, false, false, false, false,  true, false),
    Array[Boolean](false, false,  true, false, false, false, false, false),
    Array[Boolean](false, false, false, false, false, false, false,  true),
    Array[Boolean](false,  true, false, false, false, false, false, false),
    Array[Boolean](false, false, false, false,  true, false, false, false),
    Array[Boolean](true,  false, false, false, false, false, false, false),
    Array[Boolean](false, false, false, false, false,  true, false, false)
  )

  val checkBoard = Array(
    //               A      B      C      D      E      F      G      H
    Array[Boolean](false, false, false,  true, false, false, false, false),
    Array[Boolean](false, false, false, false, false, false,  true, false),
    Array[Boolean](false, false,  true, false, false, false, false, false),
    Array[Boolean](false, false, false, false, false, false, false,  true),
    Array[Boolean](false,  true, false, false, false, false, false, false),
    Array[Boolean](false, false, false, false,  true, false, false, false),
    Array[Boolean](true,  false, false, false, false, false, false, false),
    Array[Boolean](false, false, false, false, false, false, false, false),
  )

}

class NQueensProblemSpec extends FlatSpec with Matchers {

  sealed trait Empty8QueenProblem {
    val checkBoard = new Array[Array[Boolean]](8)
    for (i <- checkBoard.indices) {
      checkBoard(i) = Array.fill[Boolean](8)(false)
    }

    val sparseMatrix: Array[Array[Boolean]] = NQueensProblem.convert(checkBoard)
  }

  "NQueens empty problem" should "be converted properly" in new Empty8QueenProblem {
    sparseMatrix should have length 64
    for(i <- sparseMatrix.indices) {
      sparseMatrix(i) should have length 42
      // ranks
      for(j <- 0 until 8) {
        i/8 === j should be (sparseMatrix(i)(j))
      }
      // Files
      for(j <- 0 until 8) {
        sparseMatrix(i)(8 + j) should be (j == i%8)
      }
    }

    // check the ones for each row
    for (i <- sparseMatrix.indices) {
      val sum = sparseMatrix(i).foldLeft(0)((acc, x) => if(x) acc+1 else acc)
      withClue(s"row i=$i sum: ") {
        if (i === sparseMatrix.indices.start || i === sparseMatrix.indices.end) {
          sum should be(3)
        } else {
          sum should be(4)
        }
      }
    }

    NQueensProblem.unconvert(sparseMatrix) should be (checkBoard)
  }

//  "NQueens problem 1" should "be converted back properly" in {
//    NQueensProblem.unconvert(
//      NQueensProblem.convert(Solution1.checkBoard)
//    ) should be (Solution1.checkBoard)
//  }
}
