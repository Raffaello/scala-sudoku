package DLX

import dlx.{Column, DLX, Data}
import org.scalatest.{FlatSpec, PrivateMethodTester}

class DLXSpec extends FlatSpec with Checkers with PrivateMethodTester {

  trait PaperCoveredProblem {
    val dlx = new DLX(PaperProblem.matrix)
    val coverColumn = PrivateMethod[Unit]('coverColumn)
    // choose the column A, 1st one
    val A = dlx.sparseMatrix.root.r.asInstanceOf[Column]
    dlx invokePrivate coverColumn(A)
  }

  /**
    *
    * @param col
    * @param ones
    * @return
    */
  private def checkColumn(col: Column, ones: Int): Unit = {
    Data.fold(1, col, col.d)((acc, cur) => (acc+1, cur.d)) should be (ones + 1)
    col.s should be(ones)
  }

  "DLX.chooseColumn" should "be chosen correctly" in {
    val dlx = new DLX(Array(
      Array[Boolean](true, true,  true),
      Array[Boolean](true, false, false),
      Array[Boolean](true, true,  false)
    ))

    val chooseColumn = PrivateMethod[Column]('chooseColumn)
    val c = dlx invokePrivate chooseColumn()
    c.s should be(1)
    c.n should be(2)
  }

  "DLX.coverColumn" should "cover correctly" in new PaperCoveredProblem {

    checkColumnHeader(dlx.sparseMatrix.root, dlx.sparseMatrix.n - 1)
    val D = dlx.sparseMatrix.root.r.r.r.asInstanceOf[Column]
    D.n should be(3)
    checkColumn(D, 1)
    val G = D.r.r.r.asInstanceOf[Column]
    G.n should be (6)
    checkColumn(G, 2)
  }

  "DLX.uncoverColumn" should "uncover correctly" in new PaperCoveredProblem {
    val uncoverColumn = PrivateMethod[Unit]('uncoverColumn)
    dlx invokePrivate uncoverColumn(A)

    checkColumnHeader(dlx.sparseMatrix.root, dlx.sparseMatrix.n)
    val D = dlx.sparseMatrix.root.r.r.r.r.asInstanceOf[Column]
    D.n should be(3)
    checkColumn(D, 3)
    val G = D.r.r.r.asInstanceOf[Column]
    G.n should be (6)
    checkColumn(G, 3)
    checkColumn(A, 2)
  }

  "DLX paper exact cover problem example" should "be solved correctly" in {

    val dlx = new DLX(PaperProblem.matrix)
    val result = dlx.solve()

    result.length should be (3)
    result(0).length should be(2)
    result(0)(0) should be(0)
    result(0)(1) should be(3)
    result(1).length should be(3)
    result(1)(0) should be(4)
    result(1)(1) should be(5)
    result(1)(2) should be(2)
    result(2).length should be(2)
    result(2)(0) should be(1)
    result(2)(1) should be(6)

    val rows = dlx.convertIndexListToRows(result)

    rows.sorted should be(List[Int](0, 3, 4))
  }
}
