package DLX

import dlx.{Column, Data}
import org.scalatest.Matchers

trait Checkers extends Matchers {

  /**
    *
    * @param r
    * @return
    */
  def rootCheck(r: Column, tot: Int): Unit = {
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
  def checkColumnHeader(r: Column, nCols: Int): Unit = {
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
  def checkOnesDown(r: Column): Unit = {

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
  def checkOnesUp(r: Column): Unit = {

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
  def checkOnes(r: Column) = {
    checkOnesDown(r)
    checkOnesUp(r)
  }
}
