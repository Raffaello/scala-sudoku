package dlx

import org.scalatest.FlatSpec

/**
  * D. Knuth paper problem example
  */
object PaperProblem {

  val matrix = Array(
    //              A       B      C      D      E      F      G
    Array[Boolean](false, false, true,  false, true,  true,  false),
    Array[Boolean](true,  false, false, true,  false, false, true),
    Array[Boolean](false, true,  true,  false, false, true,  false),
    Array[Boolean](true,  false, false, true,  false, false, false),
    Array[Boolean](false, true,  false, false, false, false, true),
    Array[Boolean](false, false, false, true,  true,  false, true),
  )

  val sparseMatrix = new SparseMatrix(matrix)

  val ones = 16
}

class SparseMatrixSpec extends FlatSpec with Checkers {

  "dlx.SparseMatrix" should "throw IllegalArgumentException when input is not a m*n Matrix" in {
    intercept[IllegalArgumentException] {
      new SparseMatrix(Array(
        Array[Boolean](true, true),
        Array[Boolean](true, true, true)
      ))
    }
  }

  "dlx.SparseMatrix" should "throw IllegalArgumentException when zero rows" in {
    intercept[IllegalArgumentException] {
      new SparseMatrix(Array())
    }
  }

  "dlx.SparseMatrix" should "throw IllegalArgumentException when zero cols" in {
    intercept[IllegalArgumentException] {
      new SparseMatrix(Array(Array()))
    }
  }

  "dlx.SparseMatrix" should "throw IllegalArgumentException when zero rows of ones" in {
    intercept[IllegalArgumentException] {
      new SparseMatrix(Array(
        Array[Boolean](false)
      ))
    }
  }

  "dlx.SparseMatrix" should "have been built correctly" in {
    val matrixes: List[(SparseMatrix, Int)] = List[(SparseMatrix, Int)](
      (new SparseMatrix(Array(
        Array[Boolean](true, true, true),
        Array[Boolean](false, true, true),
        Array[Boolean](false, false, true)
      )), 6),
      (new SparseMatrix(Array(
        Array[Boolean](true, false, true),
        Array[Boolean](true, true, false),
        Array[Boolean](false, true, true)
      )), 6),
      (new SparseMatrix(Array(
        Array[Boolean](true, true, false, false),
        Array[Boolean](true, false, true, false),
        Array[Boolean](true, false, false, true)
      )), 6),
      (new SparseMatrix(Array(
        Array[Boolean](true,  true,  true),
        Array[Boolean](true,  false, false),
        Array[Boolean](false, true,  false),
        Array[Boolean](false, false, true)
      )), 6),
      (PaperProblem.sparseMatrix, PaperProblem.ones),
    )

    for ((matrix, tot) <- matrixes) {
      rootCheck(matrix.root, tot)
      checkColumnHeader(matrix.root, matrix.n)
      checkOnes(matrix.root)
    }
  }

  "dlx.SparseMatrix" should "throw an error when 1 columns is compose only by zeros" in {
    intercept[IllegalArgumentException] {
      new SparseMatrix(Array(
        Array[Boolean](false, false, false),
        Array[Boolean](false, true, false),
        Array[Boolean](false, false, true)
      ))
    }
  }
}
