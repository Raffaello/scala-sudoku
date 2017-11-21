package DLX

import dlx.{Column, SparseMatrix, Data}
import org.scalatest.{FlatSpec, Matchers}

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

class SparseMatrixSpec extends FlatSpec with Matchers {

  "dlx.Matrix" should "throw IllegalArgumentException when input is not a m*n Matrix" in {
    intercept[IllegalArgumentException] {
      new SparseMatrix(Array(
        Array[Boolean](true, true),
        Array[Boolean](true, true, true)
      ))
    }
  }

  "dlx.Matrix" should "throw IllegalArgumentException when zero rows" in {
    intercept[IllegalArgumentException] {
      new SparseMatrix(Array())
    }
  }

  "dlx.Matrix" should "throw IllegalArgumentException when zero cols" in {
    intercept[IllegalArgumentException] {
      new SparseMatrix(Array(Array()))
    }
  }

  "dlx.Matrix" should "throw IllegalArgumentException when zero rows of ones" in {
    intercept[IllegalArgumentException] {
      new SparseMatrix(Array(
        Array[Boolean](false)
      ))
    }
  }

  /**
    *
    * @param r
    * @return
    */
  private def rootCheck(r: Column, tot: Int): Unit = {
    r.n should be(-1)
    r.s should be(tot)
    r.u should be(null)
    r.d should be(null)
  }

  /**
    * check the column header
    *
    * @param r
    * @param nCols the number of column with at least one 1
    * @return
    */
  private def checkColumnHeader(r: Column, nCols: Int): Unit = {
    var c = r.r.asInstanceOf[Column]
    var count = 1
    while (c != r) {
      c.c should be(c)
      c.d should not be c
      c.u should not be c
      c.u.isInstanceOf[Column] should be(false)
      c.d.isInstanceOf[Column] should be(false)
      c.l.isInstanceOf[Column] should be(true)
      c.r.isInstanceOf[Column] should be(true)

      c = c.r.asInstanceOf[Column]
      count += 1
    }

    count should be(nCols + 1)
  }

  /**
    * check the sparse matrix scanning in down direction
    *
    * @param r root column cell
    * @return
    */
  private def checkOnesDown(r: Column): Unit = {

    var c: Column = r.r.asInstanceOf[Column]
    var d: Data = null
    var count = 0

    while (c != r) {
      d = c.d
      while (d != c) {
        count += 1
        d.c should be(c)
        d = d.d
      }

      c = c.r.asInstanceOf[Column]
    }

    count should be(r.s)
  }

  /**
    * check the sparse matrix scanning in up direction
    *
    * @param r root column cell
    * @return
    */
  private def checkOnesUp(r: Column): Unit = {

    var c: Column = r.r.asInstanceOf[Column]
    var d: Data = null
    var count = 0

    while (c != r) {
      d = c.u
      while (d != c) {
        count += 1
        d.c should be(c)
        d = d.u
      }

      c = c.r.asInstanceOf[Column]
    }

    count should be(r.s)
  }

  /**
    * check the sparse matrix scanning in all the 4 directions
    *
    * @param r root column cell
    * @return
    */
  private def checkOnes(r: Column) = {
    checkOnesDown(r)
    checkOnesUp(r)
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
      (new SparseMatrix(Array(
        Array[Boolean](false, false, false),
        Array[Boolean](false, true, false),
        Array[Boolean](false, false, true)
      )), 2)
    )

    for ((matrix, tot) <- matrixes) {
      rootCheck(matrix.root, tot)
      checkColumnHeader(matrix.root, matrix.n)
      checkOnes(matrix.root)
    }
  }
}
