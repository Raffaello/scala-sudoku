package dlx

/**
  * Sparse Matrix A, DLX data structure for exact cover problem
  *
  * @param matrix A m,n size of zeros and ones (false, true)
  */
final class SparseMatrix(val matrix: Array[Array[Boolean]]) {

  private val m: Int = matrix.length
  if (m == 0) throw new IllegalArgumentException("matrix has zero rows")
  private val n: Int = matrix(0).length
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
    var curUp:Data = null

    for {
      j <- 0 until n
      i <- 0 until m
      if matrix(i)(j)
    } {
      if (cur.n != j) {
        val c = new Column(0, j, cur, root, null, null)
        c.c = c
        cur.r = c
        root.l = c
        c.u = cur
        cur.d = cur
        c.d = cur
        cur.u = c
        if (null == root.r) {
          root.r = c
          c.l = root
        }


        cur = c
        curUp = c

      } else {
        val c = new Data(null, null, curUp, root, cur)
        root.u = c
        curUp.d = c
        curUp = c
      }
    }

    // by definition of the matrix these should be never null
    assert(null != cur)
    assert(null != curUp)

    root
  }

  val root: Column = build()

  /**
    *
    * @param j Column Index
    * @return
    */
  private def buildSparseColumn(j: Int, colHeader: Column): Column = {

    var leftData: Data = null
    var d0: Data = null

    for (i <- 0 until m) {
      if (matrix(i)(j)) {
        colHeader.s += 1
        val d = new Data()
        d.c = colHeader

        d.u = d
        d.d = d

        if (d0 == null) {
          d0 = d
        }

        if (leftData != null) {
          leftData.r = d
          d.l = leftData
        } else {
          d.l = d
        }

        //          if (upData != null) {
        //            upData.d = d
        //            d.u = upData
        //            upData = upData.r
        //            if(d00 != null) {
        //              d00.u = d
        //              d.d = d00
        //            }
        //          } else {
        //            d.u = d
        //            d.d = d
        //          }

        leftData = d
      }

//      colHeader = colHeader.r
    }

    //      if (d0 != null) {
    //        assert(d0 == leftData.r)
    //        upData = d0 //back to the first
    //        if (d00 == null) {
    //          d00 = upData
    //          d00.d = upData
    //        }
    //      }

    root
  }

  //  val header: Column = init(initColumnHeaders(initHeader))
}
