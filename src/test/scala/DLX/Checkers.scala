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
  def checkColumnHeader(r: Column, nCols: Int): Unit = Data.fold(1,r, r.r)((acc, cur) => {
      cur.c should be(cur)
      cur.d should not be cur
      cur.u should not be cur
      cur.u.isInstanceOf[Column] should be(false)
      cur.d.isInstanceOf[Column] should be(false)
      cur.l.isInstanceOf[Column] should be(true)
      cur.r.isInstanceOf[Column] should be(true)

      (acc + 1, cur.r)
    }) should be (nCols + 1)

  /**
    * check the sparse matrix scanning in down direction
    *
    * @param r root column cell
    * @return
    */
  def checkOnesDown(r: Column): Unit = {

    def sumDown(s:Data, c:Data) = Data.fold(0,c,c.d)((acc, cur) => (acc + 1, cur.d))

    Data.fold(0, r, r.r)((acc, cur) => (acc + sumDown(cur, cur.d), cur.r)
    ) should be(r.s)
  }

  /**
    * check the sparse matrix scanning in up direction
    *
    * @param r root column cell
    * @return
    */
  def checkOnesUp(r: Column): Unit = {

    def sumUp(s:Data, c:Data) = Data.fold(0,c,c.u)((acc, cur) => (acc + 1, cur.u))

    Data.fold(0, r, r.r)((acc, cur) => (acc + sumUp(cur, cur.u), cur.r)
    ) should be (r.s)
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
