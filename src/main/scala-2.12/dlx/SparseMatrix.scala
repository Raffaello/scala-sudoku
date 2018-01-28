package dlx

import cats.implicits._

import scala.util.control.Breaks._

/**
  * Sparse Matrix A, DLX data structure for exact cover problem
  * Need to be m,n size and have one 1 for each column
  *
  * @param matrix A m,n size of zeros and ones (false, true)
  */
final class SparseMatrix(val matrix: Array[Array[Boolean]]) {

  for (i <- matrix.indices) {
    if (matrix(i).length =!= matrix(0).length) throw new IllegalArgumentException(s"column $i has a different size of ${matrix(0).indices}")
  }

  val m: Int = matrix.foldLeft(0)((acc, x) => if (x.contains(true)) acc + 1 else acc)

  if (m === 0) throw new IllegalArgumentException("matrix has zero rows with ones")

  val n: Int = {
    var max = 0
    for(j <- matrix(0).indices) {
      breakable {
        for (i <- matrix.indices) {
          if (matrix(i)(j)) {
            max += 1
            break
          }
        }

        val e =  Array.ofDim[Boolean](matrix.length)
        for (i <- matrix.indices) {
          e(i) = matrix(i)(j)
        }

        throw new IllegalArgumentException(s"column j=$j is only zeros -> ${e.mkString(", ")}")
      }
    }

    max
  }

  if (n === 0) throw new IllegalArgumentException("matrix has zero columns with ones")
  if (n =!= matrix(0).length) throw new IllegalArgumentException("there is at least 1 column with only zeros")
  /**
    * Build the Sparse Matrix and return the root column header
    *
    * @return
    */
  private[this] def build(): Column = {
    val root: Column = new Column(-1, -1)
    var cur: Column = root
    var curUp: Data = null
    var d: Data = null
    val rows = collection.mutable.Map[Int, Array[Data]]()
    var tot: Int = 0

    /**
      * 1st data in this column
      *
      * @param c column header
      * @return
      */
    def firstDataCell(c: Column): Data = {
      val d = new Data(null, null, c, c, c)
      c.d = d
      c.u = d
      d.l = d
      d.r = d

      d
    }

    def horizontalLinks(): Unit = {
      rows.foreach {
        case (k, v) =>
         val d0 = v(0)
         var dl = d0
         v.foreach(d => {
           d.r = d0
           d0.l = d
           dl.r = d
           d.l = dl
           dl = d
         })
      }
    }

    for {
      j <- matrix(0).indices
      i <- matrix.indices
      if matrix(i)(j)
    } {
      if (cur.n =!= j) {
        // 1st 1 in this column, create column cell
        val c = new Column(1, j, cur, root)
        c.c = c
        cur.r = c
        root.l = c

        d = firstDataCell(c)
        cur = c
      } else {
        d = new Data(null, null, curUp, cur, cur)
        cur.u = d
        curUp.d = d
        cur.s += 1
      }

      tot += 1
      curUp = d

      rows += (i -> (rows.getOrElse(i, Array[Data]()) :+ d))
    }

    horizontalLinks()

    // by definition of the matrix these should be null
    assert(null != cur && null != curUp && null == root.d && null == root.u)

    root.s = tot
    root
  }

  val root: Column = build()
}
