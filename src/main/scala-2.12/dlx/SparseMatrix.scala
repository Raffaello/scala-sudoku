package dlx

/**
  * Sparse Matrix A, DLX data structure for exact cover problem
  *
  * @param matrix A m,n size of zeros and ones (false, true)
  */
final class SparseMatrix(val matrix: Array[Array[Boolean]]) {

  val m: Int = matrix.length
  if (m == 0) throw new IllegalArgumentException("matrix has zero rows")
  val n: Int = matrix(0).length
  if (n == 0) throw new IllegalArgumentException("matrix has zero columns")
  for (i <- matrix.indices) {
    if (matrix(i).length != n) throw new IllegalArgumentException(s"column $i has a different size of $n")
  }

  /**
    * Build the Sparse Matrix and return the root column header
    *
    * @return
    */
  private def build(): Column = {
    val root: Column = new Column(-1, -1, null, null, null, null, null)
    var cur: Column = root
    var curUp: Data = null
    var d: Data = null
    var rows = collection.mutable.Map[Int, Array[Data]]()
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

    for {
      j <- 0 until n
      i <- 0 until m
      if matrix(i)(j)
    } {
      if (cur.n != j) {
        // 1st 1 in this column, create column cell
        val c = new Column(1, j, cur, root, null, null)
        c.c = c
        cur.r = c
        root.l = c

        if (null == root.r) {
          // the 1st column header
          root.r = c
          c.l = root
        }

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

    // by definition of the matrix these should be never null
    assert(null != cur)
    assert(null != curUp)
    assert(null == root.d)
    assert(null == root.u)

    root.s = tot
    root
  }

  val root: Column = build()
}
