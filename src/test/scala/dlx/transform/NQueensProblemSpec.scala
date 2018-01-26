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

  val checkboard = Array(
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
    val checkboard = new Array[Array[Boolean]](8)
    for (i <- checkboard.indices) {
      checkboard(i) = Array.fill[Boolean](8)(false)
    }

    val sparseMatrix: Array[Array[Boolean]] = NQueensProblem.convert(checkboard)
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

//    //Diagonals
//    for(i <- sparseMatrix.indices) {
//      sparseMatrix(i)
//    }

  }
}
