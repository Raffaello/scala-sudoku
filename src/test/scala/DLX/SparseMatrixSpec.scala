package DLX

import dlx.{Column, SparseMatrix, Data}
import org.scalatest.{FlatSpec, Matchers}

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

  /**
    *
    * @param r
    * @return
    */
  private def rootCheck(r: Column): Unit = {
    r.n should be(-1)
    r.s should be(-1)
    r.u should be(null)
    r.d should be(null)
  }

  /**
    * check the column header
    *
    * @param r
    * @param m the number of column with at least one 1
    * @return
    */
  private def checkColumnHeader(r: Column, m: Int): Unit = {
    var c = r.r.asInstanceOf[Column]
    var count = 1
    var n = 0
    while (c != r) {
      c.c should be(c)
      c.n should be(n)
      c.d should not be c
      c.u should not be c
      c.u.isInstanceOf[Column] should be(false)
      c.d.isInstanceOf[Column] should be(false)
      c.l.isInstanceOf[Column] should be(true)
      c.r.isInstanceOf[Column] should be(true)

      c = c.r.asInstanceOf[Column]
      count += 1
      n += 1
    }

    count should be(m + 1)
  }

  /**
    * check the sparse matrix scanning in down direction
    *
    * @param r   root column cell
    * @param tot total number of 1s
    * @return
    */
  private def checkOnesDown(r: Column, tot: Int): Unit = {

    var c: Column = r.r.asInstanceOf[Column]
    var d: Data = null
    var count = 0

    while (c != r) {
      d = c.d
      while (d != c) {
        count += 1
        d.c should be (c)
        d = d.d
      }

      c = c.r.asInstanceOf[Column]
    }

    count should be(tot)
  }

  /**
    * check the sparse matrix scanning in up direction
    *
    * @param r   root column cell
    * @param tot total number of 1s
    * @return
    */
  private def checkOnesUp(r: Column, tot: Int): Unit = {

    var c: Column = r.r.asInstanceOf[Column]
    var d: Data = null
    var count = 0

    while (c != r) {
      d = c.u
      while (d != c) {
        count += 1
        d.c should be (c)
        d = d.u
      }

      c = c.r.asInstanceOf[Column]
    }

    count should be(tot)
  }

  /**
    * check the sparse matrix scanning in right direction
    *
    * @param r   root column cell
    * @param tot total number of 1s
    * @return
    */
  private def checkOnesRight(r: Column, tot: Int) = {
    var c: Column = r.r.asInstanceOf[Column]
    var d: Data = null
    var count = 0

    while (c != r) {
      d = c.d
      do {
        count += 1
        d.c should be (c)
        d = d.r
      } while (d != c.d)

      c = c.r.asInstanceOf[Column]
    }

    count should be(tot)
  }

  /**
    * check the sparse matrix scanning in left direction
    *
    * @param r   root column cell
    * @param tot total number of 1s
    * @return
    */
  private def checkOnesLeft(r: Column, tot: Int) = {
    var c: Column = r.l.asInstanceOf[Column]
    var d: Data = null
    var count = 0

    while (c != r) {
      d = c.d
      do {
        count += 1
        d.c should be (c)
        d = d.l
      } while (d != c.d)

      c = c.l.asInstanceOf[Column]
    }

    count should be(tot)
  }

  /**
    * check the sparse matrix scanning in all the 4 directions
    *
    * @param r   root column cell
    * @param tot total number of 1s
    * @return
    */
  private def checkOnes(r: Column, tot: Int) = {
    checkOnesDown(r, tot)
    checkOnesUp(r, tot)
    checkOnesRight(r, tot)
    checkOnesLeft(r, tot)
  }


  "dlx.Matrix" should "have the correct header column values" in {
    val matrix = new SparseMatrix(Array(
      Array[Boolean](true, true, true),
      Array[Boolean](false, true, true),
      Array[Boolean](false, false, true)
    ))

    rootCheck(matrix.root)
    checkColumnHeader(matrix.root, 3)
    checkOnes(matrix.root, 6)
  }
}
