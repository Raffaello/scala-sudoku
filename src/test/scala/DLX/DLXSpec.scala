package DLX

import dlx.{Column, DLX}
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
    var c = col.d
    var count = 1
    while (c != col) {
      count += 1
      c = c.d
    }

    count should be(ones + 1)
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
    println(result)
  }
}
